;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : tmtex-pdflatex.scm
;; DESCRIPTION : running pdflatex on an exported LaTeX file
;; COPYRIGHT   : (C) 2015  Joris van der Hoeven
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; NOTE: in a module of its own (it was in check/check-master.scm), so that
;; the LaTeX widgets (tmtex-widgets.scm, loaded with the menus) do not load
;; the test suites, whose test plugins then showed in Insert -> Session

(texmacs-module (convert latex tmtex-pdflatex))

(tm-define (run-pdflatex tex-file)
  (and (url-exists? tex-file)
       (let* ((tex-dir  (url-head tex-file))
              (pdf-file (url-glue (url-unglue tex-file 4) ".pdf"))
              (log-file (url-glue (url-unglue tex-file 4) ".log"))
              (cmd1 (string-append "cd " (system-url->string tex-dir)))
              (cmd2 (string-append "pdflatex -interaction=batchmode "
                                   (url->string (url-tail tex-file))))
              (cmd  (string-append cmd1 "; " cmd2 " > /dev/null")))
         (system-remove pdf-file)
         (system-remove log-file)
         (system cmd)
         (and (url-exists? log-file)
              (list (url-exists? pdf-file)
                    (number-latex-errors log-file)
                    (number-latex-pages log-file))))))

