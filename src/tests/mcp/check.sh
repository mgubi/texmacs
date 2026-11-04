#!/bin/bash
# Test `texmacs --mcp' (src/docs/mcp-server.md): a small MCP client
# (mcp_client.py) pipes JSON-RPC into it and checks the answers.
#
# usage: tests/mcp/check.sh [scenario...]
#
#   protocol   initialize, ping, tools/list, errors, notifications, the end
#              of the input, only protocol lines on stdout, the log file
#   tools      open_document, get_outline, read (all formats, cut, handles),
#              find, get_references, on a generated paper
#   shared     a second headless TeXmacs hosts the file; the MCP server joins
#              it and reads the host's live edit and deletion
#   mcp-host   the MCP server hosts the file; another TeXmacs joins and edits
#   unshared   share-open-files off (the default): nothing is shared
#   edit       the editing tools in a file nobody else has open: each tool, conflict,
#              undo_agent, save, close, create, bad input
#   readonly   TEXMACS_MCP_READONLY and TEXMACS_MCP_ROOTS for the editing tools
#   edit-shared the agent edits a file a second TeXmacs hosts and edits too: merge,
#              refusal, conflict, undo, the person's undo skips the agent
#   filter     every name of the lists of refused content is a tag of the DRD
#   styles     tags of the style of the document and of its assigns are accepted
#   recursion  a macro that calls itself: typesetting and PDF export do not crash
#   static     the static rules: definitions are the person's, dangerous macros are not called,
#              errors refuse, whole-content keys; normal writing in generic/article/book/beamer
#   guard      an edit may not introduce what the filter refuses: leaves, definitions,
#              the environment, the budget, a macro that calls itself
#   content    what content is refused (code, files, links, wrong arities), create_document
#   undo       the undo of the editor in a headless process (it crashed)
#   launcher   the script TeXmacs/bin/texmacs keeps stdin for --mcp
#   robust     utf8 and newlines in every format, odd paths, TEXMACS_MCP_ROOTS,
#              sections in concatenations, bad arguments and messages, limits
#   hostdeath  a host killed with kill -9 leaves the list of open files
#   nonascii   a file with a non-ascii name hosted elsewhere is listed and opened
#   stalled    a host stopped with SIGSTOP stays listed, the server is not blocked
#   logcap     the log is rotated during the run
#   outputs    outline and references cut and continued; a vanished client
#
# Every process is stopped above TM_TEST_RSS_KB (default 2 GB) and after
# TM_TEST_TIMEOUT seconds (default 120); the work folder is TM_TEST_HOME (not
# under /tmp, which is a link on macOS) or tests/build/mcp. Needs python3.

here=$(cd "$(dirname "$0")" && pwd)
top=$(cd "$here/../.." && pwd)
[ -x "$top/TeXmacs/bin/texmacs.bin" ] || { echo "no $top/TeXmacs/bin/texmacs.bin, build TeXmacs first" >&2; exit 1; }
base="${TM_TEST_HOME:-$top/tests/build/mcp}"
run="$base/run-$$"
mkdir -p "$run" || exit 1
PY=${PYTHON:-python3}
[ $# -eq 0 ] && set -- protocol launcher tools robust outputs logcap shared mcp-host unshared edit readonly edit-shared undo recursion filter content styles static guard hostdeath nonascii stalled
status=0
for name in "$@"; do
  w="$run/$name"
  mkdir -p "$w"
  log="$w/log"
  "$PY" "$here/mcp_client.py" "$name" "$w" > "$log" 2>&1
  rc=$?
  n=$(grep -c "^CHECK .* PASS" "$log")
  if [ $rc -eq 0 ]; then
    echo "PASS $name ($n checks)"
  else
    echo "FAIL $name (log in $log)"
    grep -E "CHECK .* FAIL|Traceback|Error" "$log" | sed 's/^/    /' | head -20
    status=1
  fi
done
# (the work folder is removed when everything passed)
if [ $status -eq 0 ]; then
  case "$run" in "$base"/run-*) rm -r -- "$run" ;; esac
fi
exit $status
