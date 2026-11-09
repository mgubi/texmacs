;; A link to a label of another document (#816): following it opens the
;; document with the label in view, not at its start. Runs on every port
;; with the hooks of the tests (docs/gui-scripting.md):
;;   SCM=$PWD/tests/gui/link-label.scm sh tests/Plugins/Vue/run.sh link-label 40
;; The jump was made in the event which loaded the document, before it was
;; first typeset and shown; in Cocoa the scroll to the cursor was then lost
;; (most of the time), the cursor at the label but out of view. The view
;; after the link must be the one which going to the label again gives.
(use-modules (check gui-driver))

(define link-dir
  (string-append (url->system (url-temp-dir)) "/gui-link-label"))

(define (link-file name)
  (system->url (string-append link-dir "/" name)))

(define (link-write name body)
  ;; the label is on the second page of a paper document, far from its start
  (tree-export
   (stree->tree
    `(document (TeXmacs "2.1")
               (style (tuple "article"))
               (body (document ,@body))
               (initial (collection (associate "page-medium" "paper")))))
   (link-file name) "texmacs"))

(define (link-clear)
  (when (url-exists? (system->url link-dir))
    (for (u (url-read-directory (system->url link-dir) "*"))
      (system-remove u))
    (system-rmdir (system->url link-dir))))

(define (link-setup)
  (link-clear)
  (system-mkdir (system->url link-dir))
  (link-write "other.tm"
              '((section "First") "aaa" (new-page)
                (section "Second") "bbb"
                (theorem (document (concat (label "thm:x") "Target.")))
                "ccc"))
  (link-write "from.tm"
              '((concat "See " (hlink "8.2" "other.tm#thm:x") " here."))))

(define link-scroll #f)

(gui-test-run
 (list (gt-wait 6000)
       (gt-window "main")
       (gt-eval link-setup)
       (gt-eval-later
        (string-append "(load-buffer (system->url \""
                       (url->system (link-file "from.tm")) "\"))"))
       (gt-wait 1000)
       (gt-idle)
       ;; follow the link, as a click on it does
       (gt-eval-later
        "(begin (go-to (append (buffer-path) (list 0 1 0 1))) (locus-link-follow))")
       (gt-wait 1000)
       (gt-idle)
       (gt-check "the other document is open"
                 (lambda () (== (url->system (url-tail (current-buffer)))
                                "other.tm")))
       (gt-check "the cursor is at the label"
                 (lambda () (== (cursor-path) (label->path "thm:x"))))
       (gt-eval (lambda () (set! link-scroll (get-scroll-y))))
       (gt-check "the view moved from the start" (lambda () (!= link-scroll 0)))
       ;; the view which going to the label gives
       (gt-eval-later "(go-to-label \"thm:x\")")
       (gt-wait 500)
       (gt-idle)
       (gt-check "the label is in view"
                 (lambda () (== (get-scroll-y) link-scroll)))
       (gt-eval link-clear)
       (gt-exit)))
