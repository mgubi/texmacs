;; Export a document to PDF the way a user does after Document > Update >
;; All: the auxiliary data (references, table of contents, indices) are
;; generated and the document typeset again, twice so that page numbers
;; which move with the table of contents settle. update-document defers this
;; work to idle time, which never comes without a window, and without a
;; window nothing typesets the buffer either, so the steps are taken here
;; directly, each followed by a forced typesetting (update-forced).
;;
;; usage: texmacs.bin -x '(load "export.scm")' -x '(test-export "in.tm" "out.pdf")' -q

(define (test-export in out)
  (let ((u (system->url in)))
    (load-buffer u)
    (switch-to-buffer u)
    (update-forced)
    (generate-all-aux)
    (update-current-buffer)
    (update-forced)
    (generate-all-aux)
    (update-current-buffer)
    (update-forced)
    (print-to-file (system->url out))))
