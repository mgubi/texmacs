;; The footer says which emoji is before the cursor: the whole sequence (a
;; family, a flag, a keycap), not its last code point, drawn with the emoji
;; font (the font of the widgets has none, and drew a square), and its name
;; (<emoji-grinning-face>, the universal encoding of the emoji). An emoji
;; typed comes into the document by its name. Both footers: the texts of the
;; editor (edit_footer.cpp) and the interactive footer (footer-menu.scm).
;; No script: run it with
;;   sh tests/Plugins/Vue/run.sh footer-emoji 90
;; it quits by itself, with the status 1 if a check failed. The preference
;; is set in the home of the run (run.sh makes a copy of the home for it and
;; removes it afterwards).
(use-modules (check gui-driver))

(define (footer-texts)
  ;; the texts drawn in the footer
  (map (lambda (l) (list-ref l 5))
       (list-filter (map (lambda (s) (string-tokenize-by-char s #\tab))
                         (gui-test-texts))
                    (lambda (l) (and (>= (length l) 6)
                                     (string-starts? (list-ref l 4) "footer"))))))

(define (footer-says? emoji name)
  ;; the emoji, as a text of its own (in the emoji font), then its name
  (let ((l (footer-texts)))
    (and (in? emoji l)
         (in? (string-append " (" name ")") l))))

(define (check-char what s emoji name)
  (list (gt-eval (lambda () (insert s)))
        (gt-idle)
        (gt-wait 500)
        (gt-check (string-append what ": " name)
                  (lambda () (footer-says? emoji name)))))

(define (checks)
  (append
   (check-char "code points" " <#1F600>" "<#1F600>" "emoji-grinning-face")
   (check-char "a family" " <#1F468><#200D><#1F469><#200D><#1F467>"
               "<#1F468><#200D><#1F469><#200D><#1F467>"
               "emoji-family-man-woman-girl")
   (check-char "a flag" " <#1F1EE><#1F1F9>" "<#1F1EE><#1F1F9>"
               "emoji-flag-italy")
   (check-char "a keycap" " 1<#FE0F><#20E3>" "1<#FE0F><#20E3>"
               "emoji-keycap-1")
   (check-char "a name" " <emoji-red-heart>" "<#2764><#FE0F>"
               "emoji-red-heart")))

(set-preference "interactive footer" "off")
(gui-test-run
 (append
  (list (gt-wait 3000)
        (gt-raw "window main")
        (gt-raw "focus")
        ;; (the messages of the start take the footer for a while)
        (gt-wait 9000))
  (checks)
  ;; an emoji typed: by its name in the document
  (list (gt-raw (string-append "text " (cork->utf8 "<#1F680>")))
        (gt-idle)
        (gt-check "typed by its name"
                  (lambda () (gt-document-has? "<emoji-rocket>")))
        (gt-eval (lambda () (set-preference "interactive footer" "on"))))
  (checks)
  (list (gt-exit))))
