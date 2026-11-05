;; A third process, which is not TeXmacs, holds the lock of the file: nobody
;; hosts it, nobody takes its place, the files are opened unshared. When it
;; is gone the lock is free: one TeXmacs hosts and the other joins it.
(load (string-append (getenv "SH_DIR") "/lib.scm"))

(define (A-steps)
  (list
   (act "an external process holds the lock for 5 s"
        (system (string-append
                 "perl -e 'use Fcntl qw(:flock); open(F, \">>\", $ARGV[0]) or exit 2; "
                 "flock(F, LOCK_EX|LOCK_NB) or exit 3; sleep 5;' '"
                 (shared-lock-path (the-name)) "' > /dev/null 2>&1 &")))
   (timed-wait 800)
   (act "load" (load-buffer FILE))
   (act "check" (check "A does not host a file whose lock is held" (not (shared-file? (the-name))))
        (sync-set! "a-loaded"))
   (wait-sync "b-loaded")
   (act "close" (buffer-close (the-name)))
   (timed-wait 4500)
   (act "load again" (load-buffer FILE))
   (act "check" (check "A hosts when the lock is free" (== (role-of) 'host))
        (sync-set! "a-hosts"))
   (wait-sync "b-joined")))

(define (B-steps)
  (list
   (wait-sync "a-loaded")
   (act "load" (load-buffer FILE))
   (act "check" (check "B does not host either" (not (shared-file? (the-name))))
        (check "B has the file" (== (text-of 1) "Hello world."))
        (sync-set! "b-loaded"))
   (act "close" (buffer-close (the-name)))
   (wait-sync "a-hosts")
   (act "load again" (load-buffer FILE))
   (act "check" (check "B joins A" (== (role-of) 'participant))
        (sync-set! "b-joined"))))

(run-steps (if (== ROLE "A") (A-steps) (B-steps)))
