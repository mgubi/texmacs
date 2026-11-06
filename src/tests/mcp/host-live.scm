;; The other TeXmacs of the scenario `shared' of tests/mcp/check.sh: it hosts
;; the file, the MCP server joins it and reads what this process edits.
(load (string-append (getenv "SH_DIR") "/lib.scm"))

(run-steps
 (list
  (act "load" (load-buffer FILE))
  (act "host" (check "the other TeXmacs hosts" (== (role-of) 'host)))
  (act "go" (sync-set! "h-loaded"))
  (wait-sync "c-joined")
  (step "joined" (and (> (peers-count) 0) #t))
  (act "edit" (insert-text! 0 6 "LIVE-"))
  (act "sig" (sync-set! "h-edited"))
  (wait-sync "c-read1")
  (act "delete" (tree-remove (body) 1 1))
  (act "sig2" (sync-set! "h-deleted"))
  (wait-sync "c-done")))
