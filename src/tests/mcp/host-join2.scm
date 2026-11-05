;; The other TeXmacs of the scenario `elicit-race': the MCP server hosts the file and asks its
;; client about an edit; this process joins and edits while the question is open, and reports its
;; document during the question and after it.
(load (string-append (getenv "SH_DIR") "/lib.scm"))
(run-steps
 (list
  (wait-sync "c-opened")
  (act "load" (load-buffer FILE))
  (act "join" (sync-set! "b-joined"))
  (wait-sync "go")
  (act "edit" (insert-text! 0 6 "FROMB-"))
  (act "e" (sync-set! "b-edited"))
  (wait-sync "dump1")
  (act "dump1" (sync-write! "b-body1" (object->string (body-stree))) (sync-set! "b-dumped1"))
  (wait-sync "dump2")
  (act "dump2" (sync-write! "b-body2" (object->string (body-stree))) (sync-set! "b-dumped2"))
  (wait-sync "c-done")))
