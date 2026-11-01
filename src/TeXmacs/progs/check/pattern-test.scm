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
      (check= (list-ref p 4) "blue"))))

(define (run-group thunk)
  ;; an error in a group is one failure
  (with r (check-run thunk)
    (when (and (pair? r) (== (car r) 'error))
      (check-report #f "the group" (object->string r)))))

(tm-define (pattern-test-failures)
  (check-suite "pattern")
  (run-group test-accessors)
  (run-group test-build)
  (run-group test-page-background)
  (run-group test-evaluation)
  (check-end))
