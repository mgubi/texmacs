;; The lock file is removed under a host and the folder can not be written
;; (the file can not be made again): the host does not stay host; it stops, with
;; its document saved in the recovered folder. (The case of other errors, such as
;; no descriptor left, which are tried five times, is not provoked here.)
(load (string-append (getenv "SH_DIR") "/lib.scm"))

(define (dir) (url->system (url-head (string->url (shared-lock-path (the-name))))))

(define (A-steps)
  (list
   (act "load" (load-buffer FILE) (check "A hosts" (== (role-of) 'host)))
   (act "remove the lock, close the folder" (shared-pause! #t)
        (insert-text! 1 5 "AA")
        (system-remove (string->url (shared-lock-path (the-name))))
        (system (string-append "chmod 000 '" (dir) "'"))
        (shared-pause! #f))
   (timed-wait 1800)
   (act "reopen the folder" (system (string-append "chmod 700 '" (dir) "'")))
   (act "check" (check "the host does not go on when the lock can not be made again"
                       (and (== (role-of) 'participant) (>= (shared-stat 'lock-lost) 1)))
        (check "its document is saved"
               (list-or (map (lambda (f) (file-has? (string->url f) "HelloAA")) (recovered-files)))))))

(run-steps (if (== ROLE "A") (A-steps) (list)))
