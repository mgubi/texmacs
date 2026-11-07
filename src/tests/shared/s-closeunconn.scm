;; A participant that has no host (its host died, and a process which is not
;; TeXmacs holds the lock) and is closed with something that is not saved
;; saves its whole document in the recovered folder, with a notice.
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
   (act "edit and close with no host" (insert-text! 1 5 "BB") (buffer-close (the-name)))
   (act "check" (check "the whole document is saved when it is closed with no host"
                       (list-or (map (lambda (f) (and (string-contains? f "-document.tm") (file-has? (string->url f) "BB")))
                                     (recovered-files))))
        (check "and said" (>= (shared-stat 'closed-unconnected) 1)))))

(run-steps (if (== ROLE "A") (A-steps) (B-steps)))
