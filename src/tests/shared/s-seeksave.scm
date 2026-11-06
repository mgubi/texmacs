;; A participant that has no host (its host died, and a process which is not
;; TeXmacs holds the lock) does not write the file when it is asked to: the
;; Save is queued, and done when the participant becomes the host.
(load (string-append (getenv "SH_DIR") "/lib.scm"))

(define (A-steps)
  (list
   (act "load" (load-buffer FILE) (sync-set! "a-loaded"))
   (wait-sync "b-joined")
   (act "an external process waits for the lock, to hold it 4 s"
        (system (string-append
                 "perl -e 'use Fcntl qw(:flock); open(F, \">>\", $ARGV[0]) or exit 2; "
                 "while (!flock(F, LOCK_EX|LOCK_NB)) { select(undef, undef, undef, 0.05); exit 3 if ++$n > 400; } "
                 "open(M, \">\", $ARGV[1]); print M 1; close M; sleep 4;' '"
                 (shared-lock-path (the-name)) "' '" SYNC "/perl-held' > /dev/null 2>&1 &")))
   (timed-wait 300)
   (act "die" (die!))))

(define (B-steps)
  (list
   (wait-sync "a-loaded")
   (act "load" (load-buffer FILE) (shared-pause! #t) (sync-set! "b-joined"))
   (wait-sync "perl-held")
   (act "resume" (shared-pause! #f))
   (timed-wait 800)
   (act "edit and save" (insert-text! 1 5 "BB") (buffer-pretend-modified (the-name))
        (save-buffer-main (the-name)))
   (timed-wait 1000)
   (act "check" (check "the file was not written by a process with no host"
                       (not (file-has? (the-name) "BB")))
        (check "B is a participant meanwhile" (== (role-of) 'participant))
        (check "the Save was queued" (>= (shared-stat 'save-queued) 1)))
   (step "B hosts when the lock is free" (== (role-of) 'host))
   (step "and the queued Save is done" (file-has? (the-name) "BB"))))

(run-steps (if (== ROLE "A") (A-steps) (B-steps)))
