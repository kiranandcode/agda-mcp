#!/usr/bin/env bash
# Launch N agda-mcp instances on consecutive ports (AGDA_MCP_PORT), one per
# parallel worker, so their Agda sessions are isolated by destination -- no
# reliance on the model passing a sessionId. Prints one URL per line.
#
# Usage: AGDA_DIR=/path/to/agdadir ./scripts/serve-pool.sh <N> [base_port]
set -euo pipefail
N="${1:?usage: serve-pool.sh <N> [base_port]}"; BASE="${2:-3000}"
BIN="$(cd "$(dirname "$0")/.." && cabal list-bin exe:agda-mcp 2>/dev/null)"
[ -x "$BIN" ] || { echo "build first: cabal build exe:agda-mcp" >&2; exit 1; }
pids=(); trap 'kill "${pids[@]}" 2>/dev/null || true' EXIT INT TERM
for i in $(seq 0 $((N - 1))); do
  port=$((BASE + i))
  AGDA_MCP_PORT="$port" "$BIN" >"/tmp/agda-mcp-$port.log" 2>&1 &
  pids+=($!); echo "http://localhost:$port/mcp"
done
wait
