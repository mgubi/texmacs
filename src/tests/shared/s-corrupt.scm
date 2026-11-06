;; An entry is not trusted: with the lock free, an empty entry and the entry
;; of a program that is alive but serves nothing are of no matter. The case of
;; the name of the file is not one either.
(load (string-append (getenv "SH_DIR") "/lib.scm"))

(define (A-steps)
  (list
   (act "empty entry" (shared-create-file (shared-entry-path (the-name)) ""))
   (act "load at once" (load-buffer FILE))
   (act "check" (check "an empty entry does not keep the file from being hosted" (== (role-of) 'host))
        (buffer-close (the-name)))
   (act "pid of a program that is alive, nothing serves"
        (shared-replace-file (shared-entry-path (the-name))
                             (object->string (list 'entry (shared-real-path FILE) 1 "tok" 1 #f "x" 0))))
   (act "load" (load-buffer FILE))
   (act "check" (check "an entry of a pid alive but not serving is replaced"
                       (and (== (role-of) 'host) (== (list-ref (shared-entry (the-name)) 4) (shared-process-id))))
        (check "the case of the name does not matter"
               (let ((upper (string-append (url->system (url-head (the-name))) "/F.TM")))
                 (or (== (shared-real-path upper) "")        ; (a file system with case)
                     (== (shared-real-path upper) (shared-real-path FILE))))))))

(run-steps (if (== ROLE "A") (A-steps) (list)))
