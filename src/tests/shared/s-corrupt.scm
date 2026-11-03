;; An entry that is empty and old, and one of a pid that is alive but of
;; nothing that serves the file, are stale.
(load (string-append (getenv "SH_DIR") "/lib.scm"))

(define (A-steps)
  (list
   (act "empty entry" (shared-create-file (shared-entry-path (the-name)) ""))
   (act "load at once" (load-buffer FILE))
   (act "check" (check "a new empty entry is not taken for stale" (not (shared-file? (the-name))))
        (buffer-close (the-name)))
   (timed-wait 6500)
   (act "load later" (load-buffer FILE))
   (act "check" (check "an old empty entry is replaced" (and (== (role-of) 'host) (shared-entry (the-name)) #t))
        (buffer-close (the-name)))
   (act "pid of a program that is alive, nothing serves"
        (shared-create-file (shared-entry-path (the-name))
                            (object->string (list 'entry (shared-real-path FILE) 1 "tok" 1 #f "x"))))
   (act "load" (load-buffer FILE))
   (act "check" (check "an entry of a pid alive but not serving is replaced"
                       (and (== (role-of) 'host) (== (list-ref (shared-entry (the-name)) 4) (shared-process-id))))
        (check "the case of the name does not matter"
               (let ((upper (string-append (url->system (url-head (the-name))) "/F.TM")))
                 (or (== (shared-real-path upper) "")        ; (a file system with case)
                     (== (shared-real-path upper) (shared-real-path FILE))))))))

(run-steps (if (== ROLE "A") (A-steps) (list)))
