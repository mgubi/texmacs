;; The other TeXmacs of the scenario `mcp-host': the MCP server hosts the
;; file; this process joins it and edits.
(load (string-append (getenv "SH_DIR") "/lib.scm"))

(run-steps
 (list
  (wait-sync "c-opened")
  (act "load" (load-buffer FILE))
  (act "join" (check "the other TeXmacs joins" (== (role-of) 'participant)))
  (act "edit" (insert-text! 0 6 "FROMB-"))
  (act "sig" (sync-set! "h-edited"))
  (wait-sync "c-done")))
