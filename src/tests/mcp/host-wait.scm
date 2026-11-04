;; The other TeXmacs of the scenarios `hostdeath' and `stalled': it hosts the
;; file and waits (it is killed or stopped by the test).
(load (string-append (getenv "SH_DIR") "/lib.scm"))

(run-steps
 (list
  (act "load" (load-buffer FILE))
  (act "host" (check "the other TeXmacs hosts" (== (role-of) 'host)))
  (act "go" (sync-set! "h-loaded"))
  (wait-sync "c-done")))
