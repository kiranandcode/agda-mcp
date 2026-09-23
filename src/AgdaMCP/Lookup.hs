{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

-- | Name resolution from a loaded file's highlighting.
--
-- Agda records, for every name occurrence in a type-checked file, where that
-- name is defined (a 'DefinitionSite': the defining top-level module and a
-- character offset in it). That makes "what is this name, and where is it
-- defined" answerable for any name /as it is used/, including names that are
-- only in scope inside a nested, parameterised, or private module, which a
-- top-level scope query cannot see.
--
-- The lookups run against a snapshot of the persistent REPL's type-checking
-- state, taken after each REPL response, so they see exactly what
-- @agda_load@ loaded without type-checking anything again.
module AgdaMCP.Lookup
  ( Snapshot
  , runInSnapshot
  , Resolution(..)
  , resolveAtPosition
  , resolveSymbol
  , renderResolution
  , resolutionJSON
  ) where

import Control.Applicative ((<|>))
import Control.Monad (forM)
import qualified Data.Aeson as JSON
import Data.Aeson ((.=))
import qualified Data.HashMap.Strict as HashMap
import Data.List (find, sortOn)
import qualified Data.Map as Map
import Data.Maybe (mapMaybe, listToMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL

import Agda.Interaction.Highlighting.Precise
  (Aspect(..), Aspects(..), DefinitionSite(..), HighlightingInfo, toList)
import qualified Agda.Interaction.Highlighting.Range as HR
import Agda.Syntax.Abstract.Name (QName(..), nameBindingSite)
import Agda.Syntax.Common.Pretty (prettyShow, render)
import Agda.Syntax.Position (Range'(..), RangeFile(..), rStart, posPos)
import Agda.TypeChecking.Monad
import Agda.TypeChecking.Pretty (prettyTCM)
import Agda.Utils.FileName (filePath)
import Agda.Utils.Lens ((^.))
import qualified Agda.Utils.Maybe.Strict as Strict

-- | The REPL's environment and state after its latest response.
type Snapshot = (TCEnv, TCState)

-- | Run a TCM action against a snapshot. The snapshot is only read: the state
-- the action ends in is discarded.
runInSnapshot :: Snapshot -> TCM a -> IO a
runInSnapshot (env, st) m = fst <$> runTCM env st m

-- | A resolved name occurrence.
data Resolution = Resolution
  { resSymbol     :: Text               -- ^ the occurrence's text
  , resAt         :: (Int, Int)         -- ^ its line and column (1-based)
  , resKind       :: Maybe Text         -- ^ e.g. "Function", "Constructor", "Bound"
  , resNote       :: Maybe Text         -- ^ Agda's note on it (fixity etc.), if any
  , resQName      :: Maybe Text         -- ^ fully qualified name, when found
  , resType       :: Maybe Text         -- ^ its type, when found
  , resModule     :: Text               -- ^ defining top-level module
  , resFile       :: Maybe FilePath     -- ^ defining file
  , resDefAt      :: Maybe (Int, Int)   -- ^ line and column of the definition
  , resSource     :: [Text]             -- ^ the definition's leading source lines
  }

------------------------------------------------------------------------------
-- Modules, files and their text

-- | The interface of the top-level module whose source is this file, among
-- the modules the REPL has visited (the loaded file and all its imports).
interfaceForFile :: AbsolutePath -> TCM (Maybe (TopLevelModuleName, Interface))
interfaceForFile path = do
  m2s <- useTC stModuleToSourceId
  visited <- useTC stVisitedModules
  hits <- forM (Map.toList m2s) $ \(m, src) -> do
    p <- srcFilePath src
    pure $ if p == path then Just m else Nothing
  pure $ do
    m <- listToMaybe (mapMaybe id hits)
    mi <- Map.lookup m visited
    pure (m, miInterface mi)

-- | The source file of a visited top-level module.
fileOfModule :: TopLevelModuleName -> TCM (Maybe AbsolutePath)
fileOfModule m = do
  m2s <- useTC stModuleToSourceId
  traverse srcFilePath (Map.lookup m m2s)

interfaceOfModule :: TopLevelModuleName -> TCM (Maybe Interface)
interfaceOfModule m = fmap miInterface . Map.lookup m <$> useTC stVisitedModules

-- | Source text of an interface. Highlighting offsets count characters in
-- exactly this text, from 1.
sourceText :: Interface -> Text
sourceText = TL.toStrict . iSource

-- | Line and column (both 1-based) of a 1-based character offset.
offsetToLineCol :: Text -> Int -> (Int, Int)
offsetToLineCol src off =
  let before = T.take (off - 1) src
      line = T.count "\n" before + 1
      col = T.length (snd (T.breakOnEnd "\n" before)) + 1
  in (line, col)

-- | 1-based character offset of a line and column; Nothing if out of range.
lineColToOffset :: Text -> Int -> Int -> Maybe Int
lineColToOffset src line col
  | line < 1 || col < 1 = Nothing
  | otherwise =
      let ls = T.splitOn "\n" src
      in if line > length ls
           then Nothing
           else Just (sum (map ((+ 1) . T.length) (take (line - 1) ls)) + col)

-- | The definition's leading lines: its first line, then the lines indented
-- deeper than it (a multi-line type signature), at most @limit@ lines.
definitionLines :: Int -> Text -> Int -> [Text]
definitionLines limit src line =
  case drop (line - 1) (T.lines src) of
    [] -> []
    (first : rest) ->
      let indent = T.length . T.takeWhile (== ' ')
          deeper l = not (T.null (T.strip l)) && indent l > indent first
      in take limit (first : takeWhile deeper rest)

------------------------------------------------------------------------------
-- Resolution

-- | Highlighting entries that know their definition site, with the text of
-- the token each covers.
namedTokens :: Text -> HighlightingInfo -> [(HR.Range, Text, Aspects, DefinitionSite)]
namedTokens src hl =
  [ (r, T.take (HR.to r - HR.from r) (T.drop (HR.from r - 1) src), asp, site)
  | (r, asp) <- toList hl
  , Just site <- [definitionSite asp]
  ]

resolveEntry :: Text -> (HR.Range, Text, Aspects, DefinitionSite) -> TCM Resolution
resolveEntry src (r, tok, asp, site) = do
  let defMod = defSiteModule site
  file <- fileOfModule defMod
  defIface <- interfaceOfModule defMod
  let defSrc = sourceText <$> defIface
      defAt = (`offsetToLineCol` defSitePos site) <$> defSrc
      source = case (defSrc, defAt) of
        (Just s, Just (l, _)) -> definitionLines 12 s l
        _ -> []
  (qname, ty) <- definedName defMod defIface (defSitePos site)
  pure Resolution
    { resSymbol = tok
    , resAt = offsetToLineCol src (HR.from r)
    , resKind = kindText <$> aspect asp
    , resNote = if null (note asp) then Nothing else Just (T.pack (note asp))
    , resQName = qname
    , resType = ty
    , resModule = T.pack (prettyShow defMod)
    , resFile = filePath <$> file
    , resDefAt = defAt
    , resSource = source
    }

-- | The qualified name bound at a definition site, and its type, when the
-- definition is in the loaded signature (bound variables and module names
-- are not).
definedName :: TopLevelModuleName -> Maybe Interface -> Int -> TCM (Maybe Text, Maybe Text)
definedName defMod defIface pos = do
  imported <- useTC stImports
  current <- useTC stSignature
  let defs = HashMap.toList (imported ^. sigDefinitions)
          ++ HashMap.toList (current ^. sigDefinitions)
      boundHere (q, _) = case nameBindingSite (qnameName q) of
        r@(Range f _) ->
          fmap (fromIntegral . posPos) (rStart r) == Just pos
            && case f of
                 Strict.Just rf -> rangeFileName rf == Just defMod
                 Strict.Nothing -> False
        _ -> False
      -- Opening or applying a module copies its definitions under new names
      -- with the same binding site; prefer the original.
      original (q, def) =
        not (defCopy def)
          && T.pack (prettyShow defMod) `T.isPrefixOf` T.pack (prettyShow (qnameModule q))
      candidates = filter boundHere defs
  case find original candidates <|> listToMaybe candidates of
    Nothing -> pure (Nothing, Nothing)
    Just (q, def) -> do
      -- Print in the defining module's scope, so names read as they do there.
      mapM_ (setScope . iInsideScope) defIface
      ty <- inTopContext (prettyTCM (defType def))
      pure (Just (T.pack (prettyShow q)), Just (T.pack (render ty)))

-- | Resolve the name at a line and column of a loaded file.
resolveAtPosition :: AbsolutePath -> Int -> Int -> TCM (Either Text Resolution)
resolveAtPosition path line col = do
  found <- interfaceForFile path
  case found of
    Nothing -> pure (Left (notLoaded path))
    Just (_, iface) -> do
      let src = sourceText iface
      case lineColToOffset src line col of
        Nothing -> pure (Left "That position is outside the file.")
        Just off ->
          case find (\(r, _, _, _) -> HR.from r <= off && off < HR.to r)
                    (namedTokens src (iHighlighting iface)) of
            Nothing -> pure (Left "No name with a known definition at that position (it may be whitespace, a keyword, or a symbol Agda does not link).")
            Just entry -> Right <$> resolveEntry src entry

-- | Resolve a symbol by name as it is used in a loaded file: the occurrence
-- nearest @near@ (a line), or the first one. A qualified symbol also matches
-- an unqualified use of its last component, and a mixfix operator such as
-- @_≈⟨_⟩_@ matches a use of any of its name parts.
resolveSymbol :: AbsolutePath -> Text -> Maybe Int -> TCM (Either Text Resolution)
resolveSymbol path sym near = do
  found <- interfaceForFile path
  case found of
    Nothing -> pure (Left (notLoaded path))
    Just (_, iface) -> do
      let src = sourceText iface
          tokens = namedTokens src (iHighlighting iface)
          matches = [ e | e@(_, tok, _, _) <- tokens, tok `elem` spellings ]
          lineOf (r, _, _, _) = fst (offsetToLineCol src (HR.from r))
          -- Uses before binding sites (a renaming or the definition itself),
          -- then nearest the requested line.
          here (_, _, _, site) = defSiteHere site
          ranked = case near of
            Just l -> sortOn (\e -> (here e, abs (lineOf e - l))) matches
            Nothing -> sortOn here matches
      case ranked of
        [] -> pure (Left ("No use of " <> sym <> " in " <> T.pack (filePath path) <> " that Agda links to a definition."))
        (entry : _) -> Right <$> resolveEntry src entry
  where
    lastPart = last (T.splitOn "." sym)
    spellings = filter (not . T.null) $
      [sym, lastPart] ++ T.splitOn "_" lastPart

kindText :: Aspect -> Text
kindText = \case
  Name (Just k) _ -> T.pack (show k)
  Name Nothing _ -> "Name"
  a -> T.pack (show a)

notLoaded :: AbsolutePath -> Text
notLoaded path =
  "Not loaded in this session: " <> T.pack (filePath path) <> ". Call agda_load on it first."

------------------------------------------------------------------------------
-- Output

renderResolution :: Resolution -> Text
renderResolution r = T.unlines $
  [ resSymbol r <> " (line " <> showT (fst (resAt r)) <> ", column " <> showT (snd (resAt r)) <> ")"
      <> maybe "" (\k -> " -- " <> k) (resKind r)
  , "  name:    " <> maybe ("(not a top-level definition) in " <> resModule r) id (resQName r)
  ]
  ++ [ "  type:    " <> T.intercalate "\n           " (T.lines t) | Just t <- [resType r] ]
  ++ [ "  defined: " <> T.pack f <> maybe "" (\(l, c) -> ":" <> showT l <> ":" <> showT c) (resDefAt r)
     | Just f <- [resFile r] ]
  ++ [ "  note:    " <> n | Just n <- [resNote r] ]
  ++ (if null (resSource r) then [] else "  source:" : map ("    " <>) (resSource r))
  where showT = T.pack . show

resolutionJSON :: Resolution -> JSON.Value
resolutionJSON r = JSON.object
  [ "symbol" .= resSymbol r
  , "line" .= fst (resAt r)
  , "column" .= snd (resAt r)
  , "kind" .= resKind r
  , "note" .= resNote r
  , "qualifiedName" .= resQName r
  , "type" .= resType r
  , "module" .= resModule r
  , "file" .= resFile r
  , "definitionLine" .= fmap fst (resDefAt r)
  , "definitionColumn" .= fmap snd (resDefAt r)
  , "source" .= resSource r
  ]
