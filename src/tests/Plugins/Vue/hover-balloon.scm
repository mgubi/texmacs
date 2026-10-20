;; The balloon of a language server under the mouse (#399), with clangd
;; (skipped without it): it opens when the mouse rests on a word, stays when
;; the hand moves a little (it was closed at the next move, the cursor being
;; said to have changed after each layout), and closes when the mouse rests
;; on another line, under the balloon (whose pointer the editor did not see:
;; Vue gave it to the balloon as to a popup menu). The places are those of
;; the window of run.sh:
;;   sh tests/Plugins/Vue/run.sh hover-balloon 60
(use-modules (check gui-driver) (utils misc tooltip))

(define hb-file
  (string-append (getenv "TEXMACS_HOME_PATH") "/hover-balloon.c"))

(define hb-shown '()) ; the balloons open, newest first

(tm-define (show-tooltip id ref tip ha va type magf)
  (when (string-starts? id "lsp-hover:")
    (set! hb-shown (cons id (list-remove hb-shown id))))
  (former id ref tip ha va type magf))

(tm-define (close-tooltip id)
  (set! hb-shown (list-remove hb-shown id))
  (former id))

(define (hb-open?) (nnull? hb-shown))

(gui-test-run
 (list (gt-wait 6000)
       (gt-port "vue")
       (gt-eval (lambda ()
                  (string-save (string-append
                                "#include <stdio.h>\n\n"
                                "int main (void) {\n"
                                "  int count = undefined_name + 1;\n"
                                "  printf (\"%d\\n\", count);\n"
                                "  return missing_function (count);\n"
                                "}\n")
                               (system->url hb-file))))
       (gt-eval-later (string-append "(load-buffer (system->url "
                                     (object->string hb-file) "))"))
       (gt-wait 8000)
       (gt-window "main")
       (gt-raw "focus")
       (gt-idle)
       ;; at rest on a word
       (gt-raw "move 190 304")
       (gt-wait 2500)
       (gt-check "the balloon opens"
                 (lambda () (or (not (url-exists-in-path? "clangd"))
                                (hb-open?))))
       ;; the hand moves a little
       (gt-raw "move 194 304")
       (gt-wait 150)
       (gt-raw "move 196 305")
       (gt-wait 1000)
       (gt-check "it stays when the mouse moves a little"
                 (lambda () (or (not (url-exists-in-path? "clangd"))
                                (hb-open?))))
       ;; at rest on another line
       (gt-raw "move 205 360")
       (gt-wait 2000)
       (gt-check "it closes when the mouse rests on another line"
                 (lambda () (not (hb-open?))))
       (gt-exit)))
