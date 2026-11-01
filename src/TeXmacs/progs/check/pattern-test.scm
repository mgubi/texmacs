;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : pattern-test.scm
;; DESCRIPTION : tests of the pattern tree and of its base color
;; COPYRIGHT   : (C) 2026  Massimiliano Gubinelli
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; The tree (pattern name width height [effect [base]]): the accessors of the
;; pattern selector (generic/pattern-selector.scm), the way a page background
;; is classified in the page properties (generic/format-widgets.scm), and the
;; evaluation of the primitive (Typeset/Env/env_exec.cpp), which must keep
;; the arguments. The drawing is tested in tests/Typeset/Boxes/page_box_test.cpp.

(texmacs-module (check pattern-test)
  (:use (check check-lib) (generic pattern-selector)))

(define (page-val col)
  ((module-ref (resolve-module '(generic format-widgets))
               'page-this-bg-color-val)
   (stree->tree col)))

(define (test-accessors)
  (check-group "accessors")
  (with p3 '(pattern "a.png" "1cm" "100@")
    (check= (pattern-effect p3) #f)
    (check= (pattern-base p3) #f))
  (with p4 '(pattern "a.png" "1cm" "100@" (eff-recolor "0" "red"))
    (check= (pattern-effect p4) '(eff-recolor "0" "red"))
    (check= (pattern-base p4) #f))
  ;; a color as 4th argument of an old document is no effect and no base
  (with old '(pattern "a.png" "1cm" "100@" "blue")
    (check= (pattern-effect old) #f)
    (check= (pattern-base old) #f))
  (with p5 '(pattern "a.png" "1cm" "100@" (eff-recolor "0" "red") "blue")
    (check= (pattern-effect p5) '(eff-recolor "0" "red"))
    (check= (pattern-base p5) "blue"))
  ;; an empty base color is none
  (check= (pattern-base '(pattern "a.png" "1cm" "100@" "" "")) #f))

(define (test-build)
  (check-group "build")
  (with col '(pattern "a.png" "1cm" "100@")
    (check= (pattern-build "a.png" "1cm" "100@" #f #f) col)
    ;; only a base color: the effect is empty
    (check= (pattern-set-base col "blue")
            '(pattern "a.png" "1cm" "100@" "" "blue"))
    (with col2 (pattern-set-base col "blue")
      (check= (pattern-effect col2) #f)
      (check= (pattern-base col2) "blue")
      ;; the effect is kept when the base changes, and the base when the
      ;; effect changes
      (with col3 (pattern-set-effect col2 '(eff-recolor "0" "red"))
        (check= col3 '(pattern "a.png" "1cm" "100@"
                               (eff-recolor "0" "red") "blue"))
        (check= (pattern-set-base col3 "green")
                '(pattern "a.png" "1cm" "100@" (eff-recolor "0" "red") "green"))
        (check= (pattern-set-base col3 #f)
                '(pattern "a.png" "1cm" "100@" (eff-recolor "0" "red"))))
      (check= (pattern-set-base col2 #f) col)
      (check= (pattern-set-effect col2 #f)
              '(pattern "a.png" "1cm" "100@" "" "blue")))))

(define (test-legacy-color)
  (check-group "color of an old document")
  (with old '(pattern "a.png" "1cm" "100@" "blue")
    ;; kept when a base color is set, and when it is taken away again
    (check= (pattern-set-base old "red")
            '(pattern "a.png" "1cm" "100@" "blue" "red"))
    (check= (pattern-set-base (pattern-set-base old "red") #f) old)
    ;; replaced by an effect
    (check= (pattern-set-effect old '(eff-recolor "0" "red"))
            '(pattern "a.png" "1cm" "100@" (eff-recolor "0" "red")))
    (check= (pattern-set-effect old #f) old))
  ;; an empty base is no base
  (with col '(pattern "a.png" "1cm" "100@")
    (check= (pattern-set-base col "") col)
    (check= (pattern-set-base '(pattern "a.png" "1cm" "100@" "" "red") "") col)))

(define (pc t) (string-downcase (paper-color t)))

(define (test-paper)
  (check-group "paper color")
  ;; the color under a pattern on the page: that of the document if it is a
  ;; color (opaque), else the base of its pattern, else the default of bg-color
  (check= (pc "#ff000080") "#ff0000")
  (check= (pc "red") "#ff0000")
  (check= (pc '(pattern "a.png" "1cm" "1cm" "" "#ffe0a0")) "#ffe0a0")
  (check= (pc '(pattern "a.png" "1cm" "1cm" "" "#ffe0a080")) "#ffe0a0")
  (check= (paper-color '(pattern "a.png" "1cm" "1cm")) (paper-color ""))
  (check= (paper-color '(pattern "a.png" "1cm" "1cm" "" "none")) (paper-color ""))
  (check= (paper-color "none") (paper-color ""))
  ;; the selector starts from it
  (check= (default-base-color) (paper-color (get-init-tree "bg-color"))))

(define (test-page-background)
  (check-group "page background")
  (check= (page-val "red") "color")
  (check= (page-val '(pattern "a.png" "1cm" "100@")) "pattern")
  (check= (page-val '(pattern "a.png" "100%" "100%")) "picture")
  ;; a picture with an effect and with a base color is a picture
  (check= (page-val '(pattern "a.png" "100%" "100%" (eff-recolor "0" "red")))
          "picture")
  (check= (page-val '(pattern "a.png" "100%" "100%" "" "blue")) "picture")
  (check= (page-val '(pattern "a.png" "1cm" "100@" "" "blue")) "pattern"))

(define (test-evaluation)
  (check-group "evaluation")
  (with ev (lambda (t) (tree->stree (texmacs-expand (stree->tree t))))
    (let ((p (ev '(pattern "neutral-pattern.png" "1cm" "100@"))))
      (check= (length p) 4))
    (let ((p (ev '(pattern "neutral-pattern.png" "1cm" "100@" "" "blue"))))
      (check= (length p) 6)
      (check= (list-ref p 4) "")
      (check= (list-ref p 5) "blue"))
    (let ((p (ev '(pattern "neutral-pattern.png" "1cm" "100@"
                           (eff-recolor "0" "red") "blue"))))
      (check= (length p) 6)
      (check= (car (list-ref p 4)) 'eff-recolor)
      (check= (list-ref p 5) "blue"))
    ;; the old form with a color as 4th argument is kept
    (let ((p (ev '(pattern "neutral-pattern.png" "1cm" "100@" "blue"))))
      (check= (length p) 5)
      (check= (list-ref p 4) "blue"))
    ;; patterns off: a color stands for the image, the base first, else the
    ;; color of the old form; an empty or invalid base is none
    (with off (lambda (pat) (with r (ev `(with "no-patterns" "true" ,pat)) (car (last-pair r))))
      (check= (off '(pattern "neutral-pattern.png" "1cm" "100@" "" "blue"))
              "blue")
      (check= (off '(pattern "neutral-pattern.png" "1cm" "100@"
                             (eff-recolor "0" "red") "blue"))
              "blue")
      (check= (off '(pattern "neutral-pattern.png" "1cm" "100@" "red" "blue"))
              "blue")
      (check= (off '(pattern "neutral-pattern.png" "1cm" "100@" "red" ""))
              "red")
      (check= (off '(pattern "neutral-pattern.png" "1cm" "100@" "red" "none"))
              "red")
      (check= (off '(pattern "neutral-pattern.png" "1cm" "100@" "red"
                             (value "no-such-variable")))
              "red")
      ;; an effect and no color: the pattern itself
      (check= (car (off '(pattern "neutral-pattern.png" "1cm" "100@"
                                  (eff-recolor "0" "red") "")))
              'pattern))
    ;; a missing image: the base color, else white
    (check= (ev '(pattern "no-such-image.png" "1cm" "100@" "" "blue")) "blue")
    (check= (ev '(pattern "no-such-image.png" "1cm" "100@")) "white")))

(define (run-group thunk)
  ;; an error in a group is one failure
  (with r (check-run thunk)
    (when (and (pair? r) (== (car r) 'error))
      (check-report #f "the group" (object->string r)))))

(tm-define (pattern-test-failures)
  (check-suite "pattern")
  (run-group test-accessors)
  (run-group test-build)
  (run-group test-legacy-color)
  (run-group test-paper)
  (run-group test-page-background)
  (run-group test-evaluation)
  (check-end))
