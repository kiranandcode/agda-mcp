{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module MCP.Server.Transport.Http
  ( -- * HTTP Transport
    HttpConfig(..)
  , transportRunHttp
  , defaultHttpConfig
  ) where

import           Control.Monad            (when)
import           Data.Aeson
import qualified Data.ByteString.Builder  as BB
import qualified Data.ByteString.Lazy     as BSL
import           Data.String              (IsString (fromString))
import           Data.Text                (Text)
import qualified Data.Text                as T
import qualified Data.Text.Encoding       as TE
import           Network.HTTP.Types
import qualified Network.Wai              as Wai
import qualified Network.Wai.Handler.Warp as Warp
import           System.IO                (hPutStrLn, stderr)

import           MCP.Server.Handlers
import           MCP.Server.JsonRpc
import           MCP.Server.Types

-- | HTTP transport configuration following MCP 2025-06-18 Streamable HTTP specification
data HttpConfig = HttpConfig
  { httpPort     :: Int      -- ^ Port to listen on
  , httpHost     :: String   -- ^ Host to bind to (default "localhost")
  , httpEndpoint :: String   -- ^ MCP endpoint path (default "/mcp")
  , httpVerbose  :: Bool     -- ^ Enable verbose logging (default False)
  } deriving (Show, Eq)

-- | Default HTTP configuration
defaultHttpConfig :: HttpConfig
defaultHttpConfig = HttpConfig
  { httpPort = 3000
  , httpHost = "localhost"
  , httpEndpoint = "/mcp"
  , httpVerbose = False
  }

-- | Helper for conditional logging
logVerbose :: HttpConfig -> String -> IO ()
logVerbose config msg = when (httpVerbose config) $ hPutStrLn stderr msg


-- | Transport-specific implementation for HTTP
transportRunHttp :: HttpConfig -> McpServerInfo -> McpServerHandlers IO -> IO ()
transportRunHttp config serverInfo handlers = do
  let settings = Warp.setHost (fromString $ httpHost config) $
                 Warp.setPort (httpPort config) $
                 Warp.defaultSettings

  putStrLn $ "Starting MCP HTTP server on " ++ httpHost config ++ ":" ++ show (httpPort config) ++ httpEndpoint config
  Warp.runSettings settings (mcpApplication config serverInfo handlers)

-- | WAI Application for MCP over HTTP
mcpApplication :: HttpConfig -> McpServerInfo -> McpServerHandlers IO -> Wai.Application
mcpApplication config serverInfo handlers req respond = do
  -- Log the request
  logVerbose config $ "HTTP " ++ show (Wai.requestMethod req) ++ " " ++ T.unpack (TE.decodeUtf8 $ Wai.rawPathInfo req)

  -- Check if this is our MCP endpoint
  if TE.decodeUtf8 (Wai.rawPathInfo req) == T.pack (httpEndpoint config)
    then handleMcpRequest config serverInfo handlers req respond
    else respond $ Wai.responseLBS status404 [("Content-Type", "text/plain")] "Not Found"

-- | Handle MCP requests according to Streamable HTTP specification
handleMcpRequest :: HttpConfig -> McpServerInfo -> McpServerHandlers IO -> Wai.Request -> (Wai.Response -> IO Wai.ResponseReceived) -> IO Wai.ResponseReceived
handleMcpRequest config serverInfo handlers req respond = do
  -- Check for optional MCP-Protocol-Version header (2025-06-18 spec)
  -- Made optional for backward compatibility with clients that only send version in initialize message
  case lookup "MCP-Protocol-Version" (Wai.requestHeaders req) of
    Nothing ->
      logVerbose config "Warning: MCP-Protocol-Version header missing (will check protocol version in initialize message)"
    Just headerValue ->
      if TE.decodeUtf8 headerValue /= "2025-06-18" then
        logVerbose config $ "Warning: Unsupported protocol version in header: " ++ show headerValue
      else
        logVerbose config "MCP-Protocol-Version header present: 2025-06-18"

  -- Process request regardless of header presence
  case Wai.requestMethod req of
    -- GET is the Streamable HTTP spec's channel for a server->client SSE
    -- stream. This server is request/response only, so it must decline GET with
    -- 405 (per spec) rather than return a JSON blob: MCP clients such as
    -- opencode open this GET at connect expecting `text/event-stream`, and a
    -- `200 application/json` here makes them mark the whole server unavailable.
    "GET" -> respond $ Wai.responseLBS
      status405
      [("Content-Type", "text/plain"), ("Allow", "POST, OPTIONS"), ("Access-Control-Allow-Origin", "*")]
      "Method Not Allowed: this server supports POST (request/response) only"

    -- POST requests for JSON-RPC messages
    "POST" -> do
      -- Read request body
      body <- Wai.strictRequestBody req
      logVerbose config $ "Received POST body (" ++ show (BSL.length body) ++ " bytes): " ++ take 200 (show body)
      -- Streamable HTTP: if the client accepts an event stream (opencode always
      -- does), answer as text/event-stream, not application/json -- its client
      -- refuses a plain JSON response and marks the server unavailable.
      let wantsSSE = maybe False
            (\v -> "text/event-stream" `T.isInfixOf` TE.decodeUtf8 v)
            (lookup hAccept (Wai.requestHeaders req))
      handleJsonRpcRequest config wantsSSE serverInfo handlers body respond

    -- OPTIONS for CORS preflight
    "OPTIONS" -> respond $ Wai.responseLBS
      status200
      [ ("Access-Control-Allow-Origin", "*")
      , ("Access-Control-Allow-Methods", "GET, POST, OPTIONS")
      , ("Access-Control-Allow-Headers", "Content-Type, MCP-Protocol-Version")
      ]
      ""

    -- Unsupported methods
    _ -> respond $ Wai.responseLBS
      status405
      [("Content-Type", "text/plain"), ("Allow", "GET, POST, OPTIONS")]
            "Method Not Allowed"

-- A fixed `Mcp-Session-Id` satisfies clients (opencode) that expect
-- `initialize` to assign one; this server is stateless and keys its own sessions
-- off a tool argument, so it accepts any id and ignores it on later requests.

-- | A single JSON-RPC message as a plain application/json response.
jsonResponse :: BSL.ByteString -> Wai.Response
jsonResponse body = Wai.responseLBS
  status200
  [ ("Content-Type", "application/json")
  , ("Access-Control-Allow-Origin", "*")
  , ("Mcp-Session-Id", "agda-mcp")
  ]
  body

-- | A single JSON-RPC message as one Server-Sent Event, then the stream closes
-- (Streamable HTTP allows the server to end the stream after the response).
sseResponse :: BSL.ByteString -> Wai.Response
sseResponse body = Wai.responseStream
  status200
  [ ("Content-Type", "text/event-stream")
  , ("Cache-Control", "no-cache")
  , ("Access-Control-Allow-Origin", "*")
  , ("Mcp-Session-Id", "agda-mcp")
  ]
  (\write flush -> do
      write (BB.byteString "event: message\ndata: " <> BB.lazyByteString body <> BB.byteString "\n\n")
      flush)

-- | Handle JSON-RPC request from HTTP body
handleJsonRpcRequest :: HttpConfig -> Bool -> McpServerInfo -> McpServerHandlers IO -> BSL.ByteString -> (Wai.Response -> IO Wai.ResponseReceived) -> IO Wai.ResponseReceived
handleJsonRpcRequest config wantsSSE serverInfo handlers body respond = do
  case eitherDecode body of
    Left err -> do
      hPutStrLn stderr $ "JSON parse error: " ++ err
      respond $ Wai.responseLBS
        status400
        [("Content-Type", "application/json")]
        (encode $ object ["error" .= ("Invalid JSON" :: Text)])

    Right jsonValue -> handleSingleJsonRpc config wantsSSE serverInfo handlers jsonValue respond

-- | Handle a single JSON-RPC message
handleSingleJsonRpc :: HttpConfig -> Bool -> McpServerInfo -> McpServerHandlers IO -> Value -> (Wai.Response -> IO Wai.ResponseReceived) -> IO Wai.ResponseReceived
handleSingleJsonRpc config wantsSSE serverInfo handlers jsonValue respond = do
  case parseJsonRpcMessage jsonValue of
    Left err -> do
      hPutStrLn stderr $ "JSON-RPC parse error: " ++ err
      respond $ Wai.responseLBS
        status400
        [("Content-Type", "application/json")]
        (encode $ object ["error" .= ("Invalid JSON-RPC" :: Text)])

    Right message -> do
      logVerbose config $ "Processing HTTP message: " ++ show (getMessageSummary message)
      maybeResponse <- handleMcpMessage serverInfo handlers message

      case maybeResponse of
        Just responseMsg -> do
          let responseJson = encode $ encodeJsonRpcMessage responseMsg
          logVerbose config $ "Sending HTTP response for: " ++ show (getMessageSummary message)
          respond $ if wantsSSE then sseResponse responseJson else jsonResponse responseJson

        Nothing -> do
          logVerbose config $ "No response needed for: " ++ show (getMessageSummary message)
          -- Notifications have no response body; acknowledge with 202 Accepted.
          respond $ Wai.responseLBS
            status202
            [("Access-Control-Allow-Origin", "*"), ("Mcp-Session-Id", "agda-mcp")]
            ""


