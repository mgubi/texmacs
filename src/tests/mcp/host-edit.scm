;; The other TeXmacs of the scenario `edit-shared' of tests/mcp/check.sh: the
;; person's TeXmacs. It hosts the file and does what the test asks, one
;; command at a time (the files cmd-N of the sync folder, answered by ack-N):
;;   publish            write the body in the sync file B-body
;;   type-p1 TEXT       type TEXT at the end of the paragraph 1 (a step of its own undo)
;;   type-p0 TEXT       the same at the end of the paragraph 0
;;   delete-p1          delete the paragraph 1
;;   pause / resume     stop / go on serving the links
;;   undo               undo the last step of its own
;;   stats              write the number of patches refused to the sync file stats
(load (string-append (getenv "SH_DIR") "/lib.scm"))

(define (typing! i text)
  (with-buffer (the-name)
    (start-editing)
    (let ((p (para i)))
      (tree-insert p (string-length (tree->string p)) text))
    (end-editing)))

(define (command! c)
  (let* ((sp (string-index c #\space))
         (name (if sp (substring c 0 sp) c))
         (arg (if sp (substring c (+ sp 1) (string-length c)) "")))
    (cond ((== name "publish") (publish-body!))
          ((== name "type-p1") (typing! 1 arg))
          ((== name "type-p0") (typing! 0 arg))
          ((== name "delete-p1")
           (with-buffer (the-name) (start-editing) (tree-remove (body) 1 1) (end-editing)))
          ((== name "pause") (shared-pause! #t))
          ((== name "resume") (shared-pause! #f))
          ((== name "undo") (with-buffer (the-name) (undo 0)))
          ((== name "stats")
           (sync-write! "stats" (object->string (list (shared-stat 'refused)))))
          (else (display* "unknown command " c "\n")))))

(define (command-steps n)
  (let loop ((i 1) (acc '()))
    (if (> i n)
        (reverse acc)
        (let ((cmd (string-append "cmd-" (number->string i)))
              (ack (string-append "ack-" (number->string i))))
          (loop (+ i 1)
                (append (list (act ack
                                   (when (sync-has? cmd) (command! (sync-read cmd)) (sync-set! ack)))
                              ;; (the test ends the commands with c-done)
                              (step (string-append "wait " cmd)
                                    (or (sync-has? "c-done") (sync-has? cmd))))
                        acc))))))

(run-steps
 (append
  (list (act "load" (load-buffer FILE))
        (act "host" (check "the other TeXmacs hosts" (== (role-of) 'host)))
        (act "go" (sync-set! "h-loaded")))
  (command-steps 60)))
