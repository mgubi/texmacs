;; The page of the design of the fonts (Font design, in the menu of the
;; font of the document) follows the window (page medium automatic), for
;; which the editor asks the window for its size; the Cocoa window had no
;; answer and TeXmacs aborted at once (#558). Runs on every port with the
;; hooks of the tests (docs/gui-scripting.md):
;;   SCM=$PWD/tests/gui/font-design.scm sh tests/Plugins/Vue/run.sh font-design 40
(use-modules (check gui-driver) (fonts font-design))

(define (font-design-shown?)
  (string-starts? (url->system (current-buffer)) "tmfs://font-design/"))

(define (page-width-followed?)
  ;; the width of the page was set from the window, not left at 10cm
  (with w (get-init "page-screen-width")
    (and (string? w) (string-ends? w "tmpt"))))

(gui-test-run
 (list (gt-wait 6000)
       (gt-eval-later "(open-font-design)")
       (gt-wait 3000)
       (gt-idle)
       (gt-check "the page of the fonts is shown" font-design-shown?)
       (gt-check "the page follows the window" page-width-followed?)
       (gt-eval-later "(font-design-page-tab \"math\")")
       (gt-wait 1500)
       (gt-idle)
       (gt-check "a tab of the page is shown" font-design-shown?)
       (gt-exit)))
