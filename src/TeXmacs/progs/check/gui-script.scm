
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : gui-script.scm
;; DESCRIPTION : the line language of the scripted tests of the interface
;; COPYRIGHT   : (C) 2026  The TeXmacs Upsilon developers
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; A script is a file of lines, each a step of gui-driver.scm:
;;
;;   # comment
;;   wait <ms>              pause
;;   scheme <expression>    run a Scheme command at the next turn of the loop
;;   idle                   wait until the port has had no event for a while
;;   activate <selector>    click a text or an icon of the window, named by
;;                          the bars and menus it is in: "Format > Font",
;;                          "footer > equation*", "mainbar > icon:tm_save"
;;   expect <text>          a line which the output must have (the runner,
;;                          tests/Plugins/Vue/run.sh, looks for it)
;;   port <name>            the rest is for that port only ("vue")
;;   exit                   at the end, quit with the status of the checks
;;
;; and the commands of the port, which it runs itself (gui-test-raw): for
;; Vue, window, move, press, release, click, wheel, key, text, commit,
;; compose, focus, drop, repaint, snapshot, resize, close and tab (see
;; vue_gui.cpp). TEXMACS_VUE_SCRIPT=<file> runs one (gui-script-start).

(texmacs-module (check gui-script)
  (:use (check gui-driver)))

(define (gui-script-line-step line)
  ;; the step of a line, or #f for a comment
  (let* ((l (tm-string-trim-both line))
         (n (string-length l))
         (cmd (car (string-tokenize-by-char-n (string-append l " ") #\space 1)))
         (rest (if (> n (string-length cmd))
                   (tm-string-trim-both (substring l (string-length cmd) n))
                   "")))
    (cond ((or (== l "") (string-starts? l "#")) #f)
          ((== cmd "wait")
           (if (string->number rest) (gt-wait (string->number rest) l)
               (gt-step l (lambda ()
                            (check-report #f l "wait needs a number of ms")
                            'next))))
          ((and (== cmd "scheme") (!= rest ""))
           (gt-eval-later rest l))
          ((== cmd "idle")
           (if (string->number rest) (gt-idle (string->number rest)) (gt-idle)))
          ((and (== cmd "activate") (!= rest ""))
           (gt-activate rest))
          ((== cmd "expect")
           (gt-step l (lambda () 'continue)))
          ((and (== cmd "port") (!= rest ""))
           (gt-port rest))
          ((== cmd "exit")
           (gt-exit))
          (else (gt-raw l)))))

(tm-define (gui-script-steps s)
  (:synopsis "The steps of the script @s (its text)")
  ;; the lines of a checkout on Windows end with \r
  (list-filter (map gui-script-line-step
                    (string-tokenize-by-char (string-replace s "\r" "")
                                             #\newline))
               identity))

(tm-define (gui-script-start file)
  (:synopsis "Run the script in the file @file")
  (with u (system->url file)
    (if (not (url-exists? u))
        (gt-log "cannot read " file)
        (with s (string-load u)
          (gt-log (number->string
                   (length (string-tokenize-by-char s #\newline)))
                  " lines")
          (gui-test-run (gui-script-steps s))))))
