;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : graphics-edit-test.scm
;; DESCRIPTION : tests of the graphics editor, without a window
;; COPYRIGHT   : (C) 2026  Massimiliano Gubinelli
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; The suite checks the graphics editor of TeXmacs/progs/graphics on
;; graphics in a buffer without a window:
;;
;;   - a graphics is a graphics tag inside a with which holds the gr-
;;     variables of the editor (gr-mode, gr-frame, gr-geometry, gr-grid,
;;     gr-color...); graphics-set-property and graphics-get-property set
;;     and read them on the innermost graphics around the cursor, and the
;;     objects carry their own attributes (color, line-width...) in a with
;;     around them;
;;   - the mouse handlers (edit_left-button, edit_right-button, edit_move...)
;;     are called with the coordinates of the click, as mouse_graphics does
;;     (Edit/Interface/edit_graphics.cpp). Without a window no mouse event
;;     reaches the editor, so that the position of the mouse, which the
;;     graphics state reads (get-graphical-x, get-graphical-y), stays at the
;;     origin: an object is found under the mouse when it is at the origin
;;     (graphical-select works on the typeset boxes), new objects are
;;     created at the coordinates of the click, and a selection by area
;;     takes the coordinates of two clicks;
;;   - a curve is made of several clicks which the mouse position decides
;;     (a click where the mouse has hardly moved finishes the curve or
;;     undoes it); the suite starts the curve with a click and ends it with
;;     object_commit, the function the last click calls;
;;   - the environment at the cursor (graphics-mode, graphics-geometry,
;;     graphics-cartesian-frame read it with get-env-tree) is only
;;     recomputed by apply_changes, which needs a window: the functions which
;;     read it are checked on a new buffer for each case.

(texmacs-module (check graphics-edit-test)
  (:use (check check-lib)
        (graphics graphics-drd)
        (graphics graphics-utils)
        (graphics graphics-env)
        (graphics graphics-main)
        (graphics graphics-object)
        (graphics graphics-single)
        (graphics graphics-group)
        (graphics graphics-edit)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Helpers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (body) (tree->stree (buffer-get-body (current-buffer))))

(define (at . l)
  ;; the absolute path of @l in the current buffer
  (append (buffer-path) l))

(define (rel p)
  ;; the path @p relative to the current buffer
  (and p (list-tail p (length (buffer-path)))))

(define (cursor) (rel (cursor-path)))

(define (gr)
  ;; the graphics of the buffer, (document (with ... (graphics ...)))
  (tree->stree (tree-ref (buffer-tree) 0 :last)))

(define (attrs)
  ;; the attributes of the with around the graphics
  (cDr (cdr (tree->stree (tree-ref (buffer-tree) 0)))))

(define (rect . l)
  (tree-bounding-rectangle (path->tree (apply at l))))

(define (rect-width r) (- (caddr r) (car r)))
(define (rect-height r) (- (cadddr r) (cadr r)))

(define (near? x y eps) (< (abs (- x y)) eps))

(define (point-near? p x y)
  ;; @p is (point "x" "y") with coordinates close to @x and @y
  (and (tm-func? p 'point 2)
       (near? (string->number (cadr p)) x 1e-6)
       (near? (string->number (caddr p)) y 1e-6)))

(define (sorted-with t)
  ;; the attributes of the with @t as a sorted list of pairs, and its body
  (define (pairs l)
    (if (or (null? l) (null? (cdr l))) '()
        (cons (cons (car l) (cadr l)) (pairs (cddr l)))))
  (if (tm-func? t 'with)
      (list (sort (pairs (cDr (cdr t)))
                  (lambda (a b) (string<? (car a) (car b))))
            (cAr t))
      (list '() t)))

(define (run-group thunk)
  ;; an error in a group counts as one failure
  (with r (check-run thunk)
    (when (and (pair? r) (== (car r) 'error))
      (check-report #f "the group" (object->string r)))))

(define (with-buffer-doc doc path thunk)
  ;; run @thunk in a new buffer holding @doc, with the cursor at @path
  (let* ((old (current-buffer))
         (u (new-buffer)))
    ;; new-buffer shows the buffer in the current window
    (switch-to-buffer* u)
    (buffer-set-body u (stree->tree doc))
    ;; the body set above is not an editing step of its own: without one,
    ;; an undo in the group may also undo it (as it does after other
    ;; suites in the same process)
    (archive-state)
    (start-editing)
    (end-editing)
    (clear-undo-history)
    (go-to (apply at path))
    (update-forced)
    (graphics-reset-context 'begin)
    (sketch-reset)
    (with r (check-run thunk)
      (when (and (pair? r) (== (car r) 'error))
        (check-report #f "the group" (object->string r)))
      (graphics-reset-state)
      (graphics-forget-states)
      (buffer-pretend-saved u)
      (buffer-close u)
      (when (buffer-exists? old) (switch-to-buffer old)))))

(define (with-graphics vars objs thunk)
  ;; a graphics with the variables @vars and the objects @objs, with the
  ;; cursor in its first, empty, child
  (with-buffer-doc `(document (with ,@vars (graphics "" ,@objs)))
                   (list 0 (length vars) 0 0)
                   thunk))

(define (commit)
  ;; the end of a curve, as the last click (graphics-single.scm)
  ((eval 'object_commit (resolve-module '(graphics graphics-single)))))

(define default-frame '(tuple "scale" "1cm" (tuple "0.5gw" "0.5gh")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Graphical tags and attributes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; graphics-drd.scm: the groups of tags, the attributes which each kind of
;; object takes, and their default values.
(define (test-drd)
  (check-group "drd")
  (check= (graphical-atomic-tag-list) '(point))
  (check= (graphical-open-curve-tag-list) '(line spline bezier smooth arc))
  (check= (graphical-closed-curve-tag-list)
          '(cline cspline cbezier csmooth carc))
  (check-true (graphical-curve-tag? 'cline))
  (check-true (graphical-curve-tag? 'spline))
  (check-false (graphical-curve-tag? 'point))
  (check-true (graphical-text-tag? 'text-at))
  (check-true (graphical-text-tag? 'math-at))
  (check-true (graphical-long-text-tag? 'document-at))
  (check-false (graphical-long-text-tag? 'text-at))
  (check-true (graphical-group-tag? 'gr-group))
  (check-true (graphical-tag? 'arc))
  (check-false (graphical-tag? 'graphics))
  (check-true (graphical-context? '(point "0" "0")))
  (check-false (graphical-context? '(graphics "")))
  (check-true (graphical-text-context? '(text-at "a" (point "0" "0"))))
  (check-true (graphical-text-at-context? '(text-at "a" (point "0" "0"))))
  (check-false (graphical-text-arg-context? '(text-at "a" (point "0" "0"))))
  (check-true (graphical-over-under-context? '(draw-over "" "" "")))
  (check= (gr-prefix "color") "gr-color")
  (check= (gr-unprefix "gr-color") "color")
  (check-true (gr-prefixed? "gr-mode"))
  (check-false (gr-prefixed? "color"))
  ;; defaults
  (check= (graphics-attribute-default "color") "black")
  (check= (graphics-attribute-default "gr-color") "black")
  (check= (graphics-attribute-default "line-width") "1ln")
  (check= (graphics-attribute-default "fill-color") "none")
  (check= (graphics-attribute-default "point-style") "disk")
  (check= (graphics-attribute-default "arrow-end") "none")
  (check= (graphics-attribute-default "dash-style") "none")
  (check= (graphics-attribute-default "text-at-halign") "left")
  (check= (graphics-attribute-default "no-such-attribute") #f)
  (check= (length (graphics-all-attributes)) 34)
  ;; the attributes of each kind of object
  (check= (graphics-common-attributes)
          '("gid" "anim-id" "proviso" "magnify" "color" "opacity"))
  (check= (graphics-attributes 'point)
          '("gid" "anim-id" "proviso" "magnify" "color" "opacity"
            "fill-color" "point-style" "point-size" "point-border"))
  (check-true (graphics-attribute? 'line "arrow-end"))
  (check-true (graphics-attribute? 'cline "fill-color"))
  (check-true (graphics-attribute? 'spline "dash-style"))
  (check-false (graphics-attribute? 'point "line-width"))
  (check-false (graphics-attribute? 'line "point-style"))
  (check-true (graphics-attribute? 'text-at "text-at-halign"))
  (check-false (graphics-attribute? 'text-at "line-width"))
  (check-true (graphics-attribute? 'document-at "doc-at-width"))
  (check-true (graphics-attribute? 'gr-group "line-width"))
  (check= (graphics-mode-attributes '(edit point)) (graphics-attributes 'point))
  (check= (graphics-mode-attributes '(group-edit move)) '())
  (check= (graphics-mode-attributes '(group-edit props))
          (graphics-all-attributes))
  (check-true (graphics-mode-attribute? '(edit line) "arrow-begin"))
  (check= (graphics-valign-var 'text-at) "text-at-valign")
  (check= (graphics-valign-var 'document-at) "doc-at-valign")
  (check= (graphics-valign-var '(math-at "x" (point "0" "0")))
          "text-at-valign")
  ;; menus show the dashes and arrows by these names
  (check= (decode-dash "default") "---")
  (check= (decode-dash "10") ". . . . .")
  (check= (decode-dash "11100") "- - - - -")
  (check= (decode-dash "1111010") "- . - . -")
  (check= (decode-dash "none") "none")
  (check= (decode-dash "1101") "other")
  (check= (decode-arrow "none") "")
  (check= (decode-arrow "<gtr>") ">")
  (check= (decode-arrow "|<gtr>") "|>")
  (check= (decode-arrow "<less><less>") "<<")
  ;; arity
  (check-true (graphics-minimal? '(point "0" "0")))
  (check-true (graphics-minimal? '(line (point "0" "0") (point "1" "1"))))
  (check-false (graphics-minimal?
                '(line (point "0" "0") (point "1" "1") (point "2" "2"))))
  (check-true (graphics-incomplete? '(line (point "0" "0"))))
  (check-true (graphics-incomplete? '(arc (point "0" "0") (point "1" "1"))))
  (check-false (graphics-incomplete? '(line (point "0" "0") (point "1" "1"))))
  (check-true (graphics-complete?
               '(arc (point "0" "0") (point "1" "1") (point "1" "0"))))
  (check-false (graphics-complete?
                '(line (point "0" "0") (point "1" "1") (point "1" "0"))))
  (check= (graphics-complete '(line)) '((line) #f)))

;; graphics-utils.scm: the attributes of an object are those of the with
;; around it; magnifications are numbers or "default".
(define (test-object-attributes)
  (check-group "object attributes")
  (let ((t (stree->tree '(with "color" "red" "line-width" "2ln"
                               (line (point "0" "0") (point "1" "1"))))))
    (check= (graphical-get-attribute t "color") "red")
    (check= (graphical-get-attribute t "line-width") "2ln")
    ;; the default of an attribute which is not set
    (check= (graphical-get-attribute t "dash-style") "none")
    (check= (graphical-get-attribute* t "dash-style") #f)
    (check= (graphical-relevant-attributes (tree->stree t))
            (graphics-attributes 'line))
    (check= (sorted-with (tm->stree (graphical-set-attribute t "color" "blue")))
            '((("color" . "blue") ("line-width" . "2ln"))
              (line (point "0" "0") (point "1" "1"))))
    (check= (sorted-with (tm->stree (graphical-set-attribute t "color" #f)))
            '((("line-width" . "2ln"))
              (line (point "0" "0") (point "1" "1"))))
    ;; the last attribute goes, and the with with it
    (check= (tm->stree (graphical-set-attribute t "line-width" #f))
            '(line (point "0" "0") (point "1" "1"))))
  ;; an object without attributes gets a with
  (let ((t (stree->tree '(point "1" "2"))))
    (check= (graphical-get-attribute t "color") "black")
    (check= (tm->stree (graphical-set-attribute t "color" "red"))
            '(with "color" "red" (point "1" "2"))))
  ;; on a scheme tree, a scheme tree is returned
  (check= (graphical-set-attribute '(point "1" "2") "color" "red")
          '(with "color" "red" (point "1" "2")))
  (check= (ahash-ref (with-get-attributes
                      (stree->tree '(with "color" "red" "fill-color" "blue"
                                          (point "0" "0"))))
                     "fill-color")
          "blue")
  (check= (multiply-magnify "2" 1.5) "3.0")
  (check= (multiply-magnify "2" 0.5) "default")
  (check= (multiply-magnify "default" 3) "3")
  (check= (magnify->number "default") 1)
  (check= (magnify->number "1.5") 1.5)
  (check= (number->magnify 1.0) "default")
  (check= (length-extract-unit "12.5cm") "cm")
  (check= (length-extract-unit "3gw") "gw")
  ;; FIXME: graphical-get-selected-attributes calls itself instead of
  ;; graphical-get-selected-attributes* (graphics/graphics-utils.scm:708):
  ;; (graphical-get-selected-attributes (stree->tree '(with "color" "red"
  ;; (point "0" "0"))) '("color")) gives the error stack-overflow,
  ;; expected a table with "color" -> "red".
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Inserting a graphics
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; make-graphics inserts a graphics with the default frame and size, and
;; puts the cursor inside.
(define (test-insert)
  (check-group "insert")
  (with-buffer-doc '(document "") '(0 0)
    (lambda ()
      (check-false (in-graphics?))
      (check-false (graphics-graphics-path))
      (make-graphics)
      (update-forced)
      (check= (body)
              `(document (with "gr-mode" "point"
                               "gr-frame" ,default-frame
                               "gr-geometry" (tuple "geometry" "1par" "0.6par")
                               (graphics ""))))
      (check= (cursor) '(0 6 1))
      (check-true (in-graphics?))
      (check= (rel (graphics-graphics-path)) '(0 6))
      (check= (rel (graphics-group-path)) '(0 6))
      (check-false (graphics-active-path))
      (check-false (graphics-active-object))
      (check= (graphics-mode) '(edit point))
      (check= (graphics-geometry) '(tuple "geometry" "1par" "0.6par" "center"))
      (check= (graphics-cartesian-frame) default-frame)
      (check= (graphics-get-property "gr-mode") "point")
      (check= (graphics-get-property "gr-frame") default-frame)
      ;; leaving the graphics
      (graphics-exit-right)
      (check= (cursor) '(0 1))
      (check-false (in-graphics?))))
  ;; with the variables given
  (with-buffer-doc '(document "") '(0 0)
    (lambda ()
      (make-graphics "gr-mode" "line" "gr-color" "red")
      (update-forced)
      (check= (body)
              '(document (with "gr-mode" "line" "gr-color" "red" (graphics ""))))
      (check= (cursor) '(0 4 1))
      (check= (graphics-mode) '(edit line))
      (check= (graphics-get-property "gr-color") "red")))
  ;; a graphics with a frame of its own
  (with-graphics '("gr-mode" (tuple "edit" "spline")) '()
    (lambda ()
      (check= (graphics-mode) '(edit spline))
      (check= (graphics-get-property "gr-mode") '(tuple "edit" "spline"))
      (check= (graphics-cartesian-frame) default-frame)
      (check= (graphics-geometry)
              '(tuple "geometry" "1par" "0.6par" "center")))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Properties of the graphics
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; graphics-set-property adds a variable to the with around the graphics,
;; and removes it when the value is "default" or the default of the
;; attribute; the setters of the menus go through it.
(define (test-properties)
  (check-group "properties")
  (with-graphics '("gr-mode" "point") '()
    (lambda ()
      (check= (graphics-get-property "gr-color") "default")
      (check= (graphics-get-property "gr-line-width") "default")
      (graphics-set-property "gr-color" "red")
      (check= (attrs) '("gr-mode" "point" "gr-color" "red"))
      (check= (graphics-get-property "gr-color") "red")
      (graphics-set-property "gr-color" "blue")
      (check= (attrs) '("gr-mode" "point" "gr-color" "blue"))
      ;; the default value removes the variable
      (graphics-set-property "gr-color" "black")
      (check= (attrs) '("gr-mode" "point"))
      (check= (graphics-get-property "gr-color") "default")
      (graphics-set-property "gr-color" "red")
      (graphics-set-property "gr-color" "default")
      (check= (attrs) '("gr-mode" "point"))
      (graphics-set-property "gr-color" "red")
      (graphics-remove-property "gr-color")
      (check= (attrs) '("gr-mode" "point"))
      ;; a tree value is stored as such
      (graphics-set-property "gr-dash-style-unit" (stree->tree "8ln"))
      (check= (graphics-get-property "gr-dash-style-unit") "8ln")
      (graphics-remove-property "gr-dash-style-unit")
      ;; the setters of the menus
      (graphics-set-color "red")
      (graphics-set-line-width "2ln")
      (graphics-set-dash-style "11100")
      (graphics-set-arrow-begin "<less>")
      (graphics-set-arrow-end "<gtr>")
      (graphics-set-fill-color "yellow")
      (graphics-set-point-style "square")
      (graphics-set-opacity "50%")
      (graphics-set-text-at-halign "center")
      (check= (attrs)
              '("gr-mode" "point" "gr-color" "red" "gr-line-width" "2ln"
                "gr-dash-style" "11100" "gr-arrow-begin" "<less>"
                "gr-arrow-end" "<gtr>" "gr-fill-color" "yellow"
                "gr-point-style" "square" "gr-opacity" "50%"
                "gr-text-at-halign" "center"))
      (check= (graphics-get-property "gr-arrow-end") "<gtr>")
      (check= (graphics-get-property "gr-fill-color") "yellow")
      ;; the defaults are removed
      (graphics-set-line-width "1ln")
      (graphics-set-arrow-begin "none")
      (graphics-set-text-at-halign "left")
      (check= (attrs)
              '("gr-mode" "point" "gr-color" "red"
                "gr-dash-style" "11100"
                "gr-arrow-end" "<gtr>" "gr-fill-color" "yellow"
                "gr-point-style" "square" "gr-opacity" "50%"))
      ;; a new object gets the attributes which apply to it
      (check= (sorted-with (graphics-enrich
                            '(line (point "0" "0") (point "1" "1"))))
              '((("arrow-end" . "<gtr>") ("color" . "red")
                 ("dash-style" . "11100") ("fill-color" . "yellow")
                 ("opacity" . "50%"))
                (line (point "0" "0") (point "1" "1"))))
      (check= (sorted-with (graphics-enrich '(point "0" "0")))
              '((("color" . "red") ("fill-color" . "yellow")
                 ("opacity" . "50%") ("point-style" . "square"))
                (point "0" "0")))
      (check= (sorted-with (graphics-enrich '(text-at "a" (point "0" "0"))))
              '((("color" . "red") ("opacity" . "50%"))
                (text-at "a" (point "0" "0"))))
      ;; the properties of the pen and the proviso
      (check= (graphics-get-pen-enhance-method) "gaussian")
      (check= (graphics-get-pen-enhance-strength) "1")
      (check= (graphics-get-pen-style) '("oval" "1" "0"))
      (graphics-set-pen-ratio "2")
      (check= (graphics-get-property "gr-pen-style") '(tuple "oval" "2" "0"))
      (graphics-set-proviso "false")
      (check= (graphics-get-proviso) "false")))
  ;; without attributes, nothing is added to an object
  (with-graphics '("gr-mode" "point") '()
    (lambda ()
      (check= (graphics-enrich '(line (point "0" "0") (point "1" "1")))
              '(line (point "0" "0") (point "1" "1")))
      (check= (graphics-enrich-filter 'point '(("color" "red")
                                               ("line-width" "2ln")
                                               ("fill-color" "none")))
              '("color" "red"))))
  ;; outside a graphics nothing is set
  (with-buffer-doc '(document "text") '(0 0)
    (lambda ()
      (graphics-set-property "gr-color" "red")
      (check= (body) '(document "text")))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Geometry and frame
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; The size of the graphics is gr-geometry, (tuple "geometry" w h align),
;; and its coordinates gr-frame, (tuple "scale" unit (tuple x0 y0)).
(define (test-geometry)
  (check-group "geometry")
  (let ((vars '("gr-mode" "point"
                "gr-geometry" (tuple "geometry" "4cm" "2cm" "center")
                "gr-frame" (tuple "scale" "1cm" (tuple "0.5gw" "0.5gh")))))
    (with-graphics vars '()
      (lambda ()
        (check= (graphics-geometry) '(tuple "geometry" "4cm" "2cm" "center"))
        (graphics-set-extents "5cm" "3cm")
        (check= (graphics-get-property "gr-geometry")
                '(tuple "geometry" "5cm" "3cm" "center"))))
    (with-graphics vars '()
      (lambda ()
        (graphics-set-width "7cm")
        (check= (graphics-get-property "gr-geometry")
                '(tuple "geometry" "7cm" "2cm" "center"))))
    (with-graphics vars '()
      (lambda ()
        (graphics-set-height "1cm")
        (check= (graphics-get-property "gr-geometry")
                '(tuple "geometry" "4cm" "1cm" "center"))))
    (with-graphics vars '()
      (lambda ()
        (graphics-set-geo-valign "top")
        (check= (graphics-get-property "gr-geometry")
                '(tuple "geometry" "4cm" "2cm" "top"))))
    ;; center, bottom, top, center going down
    (with-graphics vars '()
      (lambda ()
        (graphics-change-geo-valign #t)
        (check= (graphics-get-property "gr-geometry")
                '(tuple "geometry" "4cm" "2cm" "bottom"))))
    (with-graphics vars '()
      (lambda ()
        (graphics-change-geo-valign #f)
        (check= (graphics-get-property "gr-geometry")
                '(tuple "geometry" "4cm" "2cm" "top"))))
    (with-graphics vars '()
      (lambda ()
        (graphics-change-extents "1cm" "0cm")
        (with g (graphics-get-property "gr-geometry")
          (check= (car g) 'tuple)
          ;; length-add works in tmpt: 4cm + 1cm is "5.00003cm"
          (check-true (near? (length-decode (caddr g)) (length-decode "5cm")
                             10))
          (check-true (near? (length-decode (cadddr g)) (length-decode "2cm")
                             10))
          (check= (cAr g) "center"))))
    ;; the extents do not go below zero
    (with-graphics vars '()
      (lambda ()
        (graphics-change-extents "-5cm" "-1cm")
        (with g (graphics-get-property "gr-geometry")
          (check= (caddr g) "4cm")
          (check-true (near? (length-decode (cadddr g)) (length-decode "1cm")
                             10)))))
    ;; the frame
    (with-graphics vars '()
      (lambda ()
        (check= (graphics-cartesian-frame) default-frame)
        (graphics-set-unit "2cm")
        (check= (graphics-get-property "gr-frame")
                '(tuple "scale" "2cm" (tuple "0.5gw" "0.5gh")))))
    (with-graphics vars '()
      (lambda ()
        (graphics-set-origin "1cm" "1cm")
        (check= (graphics-get-property "gr-frame")
                '(tuple "scale" "1cm" (tuple "1cm" "1cm")))))
    (with-graphics vars '()
      (lambda ()
        (check= (graphics-get-zoom) 1)
        (graphics-zoom 2)
        (check= (graphics-get-property "gr-frame")
                '(tuple "scale" "2cm" (tuple "0.5gw" "0.5gh")))
        (check= (graphics-get-property "magnify") "2")))
    (with-graphics vars '()
      (lambda ()
        (graphics-move-origin "1cm" "-1cm")
        (with f (graphics-get-property "gr-frame")
          (check= (sublist f 0 3) '(tuple "scale" "1cm"))
          (check-true (string-ends? (cadr (cadddr f)) "gw"))
          (check-true (string-ends? (caddr (cadddr f)) "gh")))))
    ;; cropping
    (with-graphics vars '()
      (lambda ()
        (check-false (graphics-auto-crop?))
        (graphics-toggle-auto-crop)
        (check= (graphics-get-property "gr-auto-crop") "true")
        (graphics-set-crop-padding "1mm")
        (check= (graphics-get-property "gr-crop-padding") "1mm")))
    ;; nothing moves in a cropped graphics
    (with-graphics (append vars '("gr-auto-crop" "true")) '()
      (lambda ()
        (check-true (graphics-auto-crop?))
        (graphics-move-origin "1cm" "1cm")
        (graphics-change-extents "1cm" "1cm")
        (check= (attrs) (append vars '("gr-auto-crop" "true")))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Grids
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; The visual grid (gr-grid) is the one which is drawn, the edit grid
;; (gr-edit-grid) the one to which the points snap; by default the edit
;; grid follows the visual one. The -old variables keep a grid which was
;; switched off.
(define (test-grid)
  (check-group "grid")
  (let ((cart '(tuple "cartesian" (point "0" "0") "1"))
        (edit-aspect '(tuple (tuple "axes" "none") (tuple "1" "none")
                             (tuple "10" "none"))))
    (with-graphics '("gr-mode" "point") '()
      (lambda ()
        (check= (graphics-get-grid-type #t) 'empty)
        (check= (graphics-get-grid-type #f) 'empty)
        (graphics-set-visual-grid 'cartesian)
        (check= (attrs)
                `("gr-mode" "point" "gr-grid" ,cart "gr-grid-old" ,cart
                  "gr-edit-grid-aspect" ,edit-aspect
                  "gr-edit-grid" ,cart "gr-edit-grid-old" ,cart))))
    (with-graphics '("gr-mode" "point") '()
      (lambda ()
        (graphics-set-visual-grid 'polar)
        (with polar '(tuple "polar" (point "0" "0") "1" "24")
          (check= (graphics-get-property "gr-grid") polar)
          (check= (graphics-get-property "gr-edit-grid") polar))))
    (with-graphics '("gr-mode" "point") '()
      (lambda ()
        (graphics-set-visual-grid 'logarithmic)
        (with log '(tuple "logarithmic" (point "0" "0") "1" "10")
          (check= (graphics-get-property "gr-grid") log)
          (check= (graphics-get-property "gr-edit-grid") log))))
    ;; the grid toggle sets a cartesian grid on a graphics without grids
    (with-graphics '("gr-mode" "point") '()
      (lambda ()
        (graphics-toggle-grid)
        (check= (graphics-get-property "gr-grid") cart)
        (check= (graphics-get-property "gr-edit-grid") cart)
        ;; and graphics-reset-grids removes all the grid variables
        (graphics-reset-grids)
        (check= (attrs) '("gr-mode" "point"))))
    (with-graphics `("gr-mode" "point" "gr-grid" ,cart) '()
      (lambda ()
        (check= (graphics-get-grid-type #t) 'cartesian)
        (check= (graphics-get-grid-type #f) 'empty)
        (graphics-set-grid-step "0.5" #t)
        (with half '(tuple "cartesian" (point "0" "0") "0.5")
          (check= (graphics-get-property "gr-grid") half)
          (check= (graphics-get-property "gr-edit-grid") half))))
    (with-graphics `("gr-mode" "point" "gr-grid" ,cart) '()
      (lambda ()
        (graphics-set-grid-center "1" "2" #t)
        (check= (graphics-get-property "gr-grid")
                '(tuple "cartesian" (point "1" "2") "1"))))
    ;; switching the visual grid off keeps it in gr-grid-old
    (with-graphics `("gr-mode" "point" "gr-grid" ,cart) '()
      (lambda ()
        (graphics-toggle-visual-grid)
        (check= (attrs)
                `("gr-mode" "point" "gr-grid" (tuple "empty")
                  "gr-grid-old" ,cart))))
    (with-graphics `("gr-mode" "point" "gr-grid" (tuple "empty")
                     "gr-grid-old" ,cart) '()
      (lambda ()
        (graphics-toggle-visual-grid)
        (check= (graphics-get-property "gr-grid") cart)))
    ;; the colors of the visual grid
    (with-graphics '("gr-mode" "point") '()
      (lambda ()
        (graphics-set-grid-aspect-properties "red" "blue" "4" "green")
        (with aspect '(tuple (tuple "axes" "red") (tuple "1" "blue")
                             (tuple "4" "green"))
          (check= (graphics-get-property "gr-grid-aspect") aspect)
          (check= (graphics-get-property "gr-grid-aspect-props") aspect))
        (check= (graphics-get-property "gr-edit-grid-aspect")
                '(tuple (tuple "axes" "none") (tuple "1" "none")
                        (tuple "4" "none")))))
    ;; an edit grid of its own
    (with-graphics '("gr-mode" "point") '()
      (lambda ()
        (graphics-set-edit-grid 'cartesian)
        (check= (graphics-get-property "gr-as-visual-grid") "off")
        (check= (graphics-get-property "gr-edit-grid")
                '(tuple "cartesian" (point "0" "0") "0.1"))
        (check= (graphics-get-property "gr-grid") "")))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Modes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; gr-mode is a tag name (a string, for the edit mode of old documents) or
;; a tuple (edit tag), (group-edit operation), (hand-edit pen).
(define (test-modes)
  (check-group "modes")
  (with-graphics '("gr-mode" "line") '()
    (lambda () (check= (graphics-mode) '(edit line))))
  (with-graphics '("gr-mode" (tuple "group-edit" "move")) '()
    (lambda ()
      (check= (graphics-mode) '(group-edit move))
      (check-true (graphics-group-mode? (graphics-mode)))))
  (with-graphics '() '()
    (lambda ()
      ;; the default mode of a graphics
      (check= (graphics-get-property "gr-mode") "line")
      (check= (graphics-mode) '(edit line))))
  (with-graphics '("gr-mode" "point") '()
    (lambda ()
      (check-false (graphics-group-mode? (graphics-mode)))
      (graphics-set-mode '(edit cline))
      (check= (graphics-get-property "gr-mode") '(tuple "edit" "cline"))
      (check= (attrs) '("gr-mode" (tuple "edit" "cline")))
      (graphics-set-mode '(group-edit zoom))
      (check= (graphics-get-property "gr-mode") '(tuple "group-edit" "zoom"))
      (graphics-set-mode '(hand-edit penscript))
      (check= (graphics-get-property "gr-mode")
              '(tuple "hand-edit" "penscript"))))
  (check-true (graphics-group-mode? '(group-edit props)))
  (check-false (graphics-group-mode? '(edit point)))
  (check-false (graphics-group-mode? '(group-edit))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Typesetting
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; A graphics is a box of the size of gr-geometry; a cropped one is the
;; size of its contents and the padding; the objects are placed by the
;; frame.
(define (test-typeset)
  (check-group "typeset")
  (with-buffer-doc
   '(document
     (with "gr-geometry" (tuple "geometry" "6cm" "3cm" "center")
           (graphics "" (point "0" "0") (point "2" "0") (point "0" "1")
                     (line (point "-1" "-1") (point "1" "1"))
                     (with "color" "red" "line-width" "2ln"
                           "dash-style" "11100" "arrow-end" "<gtr>"
                           (cline (point "0" "0") (point "1" "0")
                                  (point "1" "1")))
                     (with "fill-color" "yellow"
                           (spline (point "0" "0") (point "1" "1")
                                   (point "2" "0")))
                     (arc (point "1" "0") (point "0" "1") (point "-1" "0"))
                     (text-at "Hello" (point "0" "0"))))
     (with "gr-geometry" (tuple "geometry" "3cm" "3cm" "center")
           (graphics ""))
     (with "gr-auto-crop" "true"
           (graphics "" (line (point "0" "0") (point "2" "0"))))
     (with "gr-auto-crop" "true"
           (graphics "" (line (point "0" "0") (point "4" "0")))))
   '(0 2 0 0)
   (lambda ()
     (let* ((g1 (rect 0 2))
            (g2 (rect 1 2))
            (c1 (rect 2 2))
            (c2 (rect 3 2))
            (cm (/ (rect-width g1) 6.0)))
       (check-true (> cm 0))
       (check-true (near? (rect-width g1) (* 2 (rect-width g2)) 2))
       (check-true (near? (rect-height g1) (rect-height g2) 2))
       (check-true (near? (rect-height g1) (* 3 cm) 2))
       ;; the cropped graphics: 2cm more of line, 2cm more of box
       (check-true (near? (- (rect-width c2) (rect-width c1)) (* 2 cm) 2))
       (check-true (< (rect-width c1) (rect-width g2)))
       ;; the objects in the frame: the origin is in the middle
       (let ((p0 (rect 0 2 1))
             (p1 (rect 0 2 2))
             (p2 (rect 0 2 3))
             (l (rect 0 2 4)))
         (check-true (near? (- (car p1) (car p0)) (* 2 cm) 2))
         (check-true (near? (cadr p1) (cadr p0) 1))
         (check-true (near? (- (cadr p2) (cadr p0)) cm 2))
         (check-true (near? (rect-width p0) (rect-width p1) 1))
         (check-true (near? (/ (+ (car p0) (caddr p0)) 2)
                            (/ (+ (car g1) (caddr g1)) 2) 2))
         (check-true (near? (/ (+ (cadr p0) (cadddr p0)) 2)
                            (/ (+ (cadr g1) (cadddr g1)) 2) 2))
         ;; the line from (-1,-1) to (1,1) is about 2cm wide and high
         (check-true (near? (rect-width l) (* 2 cm) (* 0.2 cm)))
         (check-true (near? (rect-width l) (rect-height l) 2)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Creating objects
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; A left click in the edit mode of a tag makes an object of that tag at
;; the place of the click, with the attributes of the graphics which apply
;; to it.
(define (test-create)
  (check-group "create")
  (with-graphics '("gr-mode" (tuple "edit" "point")) '()
    (lambda ()
      (edit_left-button 'edit "1" "2")
      (check= (gr) '(graphics "" (point "1" "2")))
      (check= (cursor) '(0 2 1 0))
      (check= (rel (graphics-active-path)) '(0 2 1))
      (check= (graphics-active-object) '(point "1" "2"))
      (edit_left-button 'edit "-0.5" "3")
      (check= (gr) '(graphics "" (point "1" "2") (point "-0.5" "3")))
      (check= (cursor) '(0 2 2 0))
      (update-forced)
      (check= (graphics-active-object) '(point "-0.5" "3"))))
  (with-graphics '("gr-mode" (tuple "edit" "point")
                   "gr-color" "red" "gr-point-style" "square"
                   "gr-line-width" "2ln") '()
    (lambda ()
      (edit_left-button 'edit "1" "0")
      (check= (sorted-with (cAr (gr)))
              '((("color" . "red") ("point-style" . "square"))
                (point "1" "0")))
      (check= (cursor) '(0 8 1 4 1))))
  ;; text
  (with-graphics '("gr-mode" (tuple "edit" "text-at")
                   "gr-text-at-halign" "center") '()
    (lambda ()
      (edit_left-button 'edit "1" "2")
      (check= (gr) '(graphics "" (with "text-at-halign" "center"
                                       (text-at "" (point "1" "2")))))
      (check= (cursor) '(0 4 1 2 0 0))
      (check-true (inside-graphical-text?))
      (insert "hi")
      (check= (gr) '(graphics "" (with "text-at-halign" "center"
                                       (text-at "hi" (point "1" "2")))))))
  (with-graphics '("gr-mode" (tuple "edit" "math-at")) '()
    (lambda ()
      (edit_left-button 'edit "1" "0")
      (check= (gr) '(graphics "" (math-at "" (point "1" "0"))))
      (check= (cursor) '(0 2 1 0 0))))
  (with-graphics '("gr-mode" (tuple "edit" "document-at")) '()
    (lambda ()
      (edit_left-button 'edit "1" "0")
      (check= (gr) '(graphics "" (document-at (document "") (point "1" "0"))))
      (check= (cursor) '(0 2 1 0 0 0))))
  ;; an empty text is removed by the next click
  (with-graphics '("gr-mode" (tuple "edit" "point"))
                 '((text-at "" (point "0" "0")) (point "1" "1"))
    (lambda ()
      (edit_left-button 'edit "2" "2")
      (check= (gr) '(graphics "" (point "1" "1") (point "2" "2")))))
  ;; a curve: the first click starts it in the sketch, with its two first
  ;; points at the click, and its end puts it into the graphics
  (with-graphics '("gr-mode" (tuple "edit" "line")
                   "gr-color" "red" "gr-line-width" "2ln") '()
    (lambda ()
      (edit_left-button 'edit "1" "2")
      (check-true sticky-point)
      (check= current-point-no 1)
      (check= (sketch-get)
              '((with "color" "red" "line-width" "2ln"
                      (line (point "1" "2") (point "1" "2")))))
      ;; the graphics does not have it yet
      (check= (gr) '(graphics ""))
      (check-true (graphics-busy?))
      (commit)
      (check-false sticky-point)
      (check= (rel current-path) '(0 6 1))
      (check= current-obj '(line (point "1" "2") (point "1" "2")))
      (check= (gr) '(graphics "" (with "color" "red" "line-width" "2ln"
                                       (line (point "1" "2")
                                             (point "1" "2")))))
      (check-false (graphics-busy?))))
  (with-graphics '("gr-mode" (tuple "edit" "spline") "gr-fill-color" "blue"
                   "gr-point-style" "square") '()
    (lambda ()
      (edit_left-button 'edit "0" "1")
      (check= (sketch-get)
              '((with "fill-color" "blue"
                      (spline (point "0" "1") (point "0" "1")))))
      (commit)
      (check= (gr) '(graphics "" (with "fill-color" "blue"
                                       (spline (point "0" "1")
                                               (point "0" "1")))))))
  ;; a closed curve or an arc needs three points: two are not committed
  (check= (map tag-minimal-arity '(line spline cline arc))
          '(2 2 3 3))
  (with-graphics '("gr-mode" (tuple "edit" "cline")) '()
    (lambda ()
      (edit_left-button 'edit "1" "0")
      (check= (sketch-get) '((cline (point "1" "0") (point "1" "0"))))
      (commit)
      (check-true sticky-point)
      (check= (gr) '(graphics ""))))
  (with-graphics '("gr-mode" (tuple "edit" "arc")) '()
    (lambda ()
      (edit_left-button 'edit "1" "0")
      (check= (sketch-get) '((arc (point "1" "0") (point "1" "0"))))
      (commit)
      (check-true sticky-point)
      (check= (gr) '(graphics ""))))
  ;; an object made by a click can be undone
  (with-graphics '("gr-mode" (tuple "edit" "point")) '()
    (lambda ()
      (archive-state)
      (start-editing)
      (edit_left-button 'edit "1" "1")
      (end-editing)
      (check= (gr) '(graphics "" (point "1" "1")))
      (undo 0)
      (check= (gr) '(graphics "")))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Removing and changing objects
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; The paths of objects, the object under the mouse (at the origin, see
;; the top of the file), and the functions which remove or replace one.
(define (test-objects)
  (check-group "objects")
  (with-graphics '("gr-mode" (tuple "edit" "point"))
                 '((point "0" "0") (line (point "0" "0") (point "1" "1"))
                   (with "color" "red" (point "2" "2")))
    (lambda ()
      (check-false (graphics-active-path))
      ;; the innermost graphical object at the cursor
      (go-to (at 0 2 3 2 0))
      (check= (rel (graphics-active-path)) '(0 2 3 2))
      (check= (graphics-active-object) '(point "2" "2"))
      (check= (rel (graphics-path (at 0 2 3 2 0))) '(0 2 3 2))
      (check= (rel (graphics-path (at 0 2 2 1 0))) '(0 2 2 1))
      (check= (rel (graphics-path (at 0 2 2 0))) '(0 2 2))
      (check= (rel (graphics-path (at 0 2 0 0))) #f)
      ;; with the with around it
      (check= (rel (graphics-object-root-path (at 0 2 3 2))) '(0 2 3))
      (check= (rel (graphics-object-root-path (at 0 2 2))) '(0 2 2))
      (check= (graphics-object (at 0 2 2 0 0)) '(point "0" "0"))
      (check= (graphics-object (at 0 2 2 0))
              '(line (point "0" "0") (point "1" "1")))
      (check= (graphics-path-property (at 0 2 3 2) "color") "red")
      (check= (graphics-path-property (at 0 2 2) "color") "default")
      ;; the object inside the with has the attributes of the with
      (check= (graphical-get-attribute (path->tree (at 0 2 3 2)) "color")
              "red")
      (check= (graphical-get-attribute (path->tree (at 0 2 2)) "color")
              "black")
      (check= (tree->stree (graphics-tree (at 0 2 1 0))) '(point "0" "0"))
      ;; replacing and removing
      (graphics-assign (at 0 2 1) '(point "5" "5"))
      (check= (cursor) '(0 2 1 1))
      (check= (gr) '(graphics "" (point "5" "5")
                              (line (point "0" "0") (point "1" "1"))
                              (with "color" "red" (point "2" "2"))))
      (graphics-remove (at 0 2 3 2))
      (check= (gr) '(graphics "" (point "5" "5")
                              (line (point "0" "0") (point "1" "1"))))
      (graphics-group-insert '(point "3" "3"))
      (check= (gr) '(graphics "" (point "5" "5")
                              (line (point "0" "0") (point "1" "1"))
                              (point "3" "3")))
      (check= (cursor) '(0 2 3 0))))
  ;; the object under the mouse, at the origin
  (with-graphics '("gr-mode" (tuple "edit" "point"))
                 '((point "0" "0") (point "2" "0"))
    (lambda ()
      (edit_move 'edit "0" "0")
      (check= (rel current-path) '(0 2 1))
      (check= current-obj '(point "0" "0"))
      ;; a right click removes it
      (edit_right-button 'edit "0" "0")
      (check= (gr) '(graphics "" (point "2" "0")))))
  ;; the objects at a point, on the typeset graphics
  (with-graphics '("gr-mode" (tuple "edit" "point"))
                 '((point "0" "0") (point "2" "0") (point "0" "1"))
    (lambda ()
      (check= (rel (car (select-first 0.0 0.0))) '(0 2 1 0))
      (check= (rel (car (select-first 2.0 0.0))) '(0 2 2 0))
      (check= (rel (car (select-first 0.0 1.0))) '(0 2 3 0))
      (check= (rel (car (car (graphics-select 0.0 0.0 15)))) '(0 2 1 0))))
  (with-graphics '("gr-mode" (tuple "edit" "point"))
                 '((with "color" "red" (point "0" "0")) (point "3" "3"))
    (lambda ()
      ;; the middle button removes as well, with the attributes
      (edit_middle-button 'edit "0" "0")
      (check= (gr) '(graphics "" (point "3" "3")))))
  ;; on a curve, a right click removes the point under the mouse
  (with-graphics '("gr-mode" (tuple "edit" "line"))
                 '((line (point "-1" "0") (point "0" "0") (point "1" "1")))
    (lambda ()
      (edit_move 'edit "0" "0")
      (check= current-point-no 1)
      (edit_right-button 'edit "0" "0")
      (check= (gr) '(graphics "" (line (point "-1" "0") (point "1" "1"))))))
  ;; and the whole curve when it has its minimal number of points
  (with-graphics '("gr-mode" (tuple "edit" "line"))
                 '((line (point "-1" "0") (point "0" "0")) (point "1" "1"))
    (lambda ()
      (edit_right-button 'edit "0" "0")
      (check= (gr) '(graphics "" (point "1" "1")))))
  ;; a left click on an object in point mode adds a point
  (with-graphics '("gr-mode" (tuple "edit" "point")) '((point "0" "0"))
    (lambda ()
      (edit_left-button 'edit "1" "1")
      (check= (gr) '(graphics "" (point "0" "0") (point "1" "1"))))))

;; graphics-zmove moves the object under the mouse in the stack of objects:
;; to the top (foreground), the bottom (background), or past the next
;; object which it overlaps (closer, farther).
(define (test-zmove)
  (check-group "zmove")
  (with-graphics '("gr-mode" (tuple "edit" "point"))
                 '((point "0" "0") (point "1" "1") (point "2" "2")
                   (point "3" "3"))
    (lambda ()
      (go-to (at 0 2 2 0))
      (graphics-reset-context 'begin)
      (check= (rel current-path) '(0 2 2))
      (graphics-zmove 'foreground)
      (check= (gr) '(graphics "" (point "0" "0") (point "2" "2")
                              (point "3" "3") (point "1" "1")))
      (check= (rel current-path) '(0 2 4))
      (graphics-zmove 'background)
      (check= (gr) '(graphics "" (point "1" "1") (point "0" "0")
                              (point "2" "2") (point "3" "3")))
      (check= (rel current-path) '(0 2 1))
      ;; already at the bottom
      (graphics-zmove 'background)
      (check= (gr) '(graphics "" (point "1" "1") (point "0" "0")
                              (point "2" "2") (point "3" "3")))
      ;; the points do not overlap
      (graphics-zmove 'closer)
      (check= (gr) '(graphics "" (point "1" "1") (point "0" "0")
                              (point "2" "2") (point "3" "3")))))
  (with-graphics '("gr-mode" (tuple "edit" "point"))
                 '((line (point "-1" "-1") (point "1" "1"))
                   (point "5" "5")
                   (line (point "-1" "1") (point "1" "-1")))
    (lambda ()
      (go-to (at 0 2 1 0))
      (graphics-reset-context 'begin)
      (check= (rel current-path) '(0 2 1))
      (graphics-zmove 'closer)
      (update-forced)
      (check= (gr) '(graphics "" (point "5" "5")
                              (line (point "-1" "1") (point "1" "-1"))
                              (line (point "-1" "-1") (point "1" "1"))))
      (check= (rel current-path) '(0 2 3))
      (graphics-zmove 'farther)
      (update-forced)
      (check= (gr) '(graphics "" (point "5" "5")
                              (line (point "-1" "-1") (point "1" "1"))
                              (line (point "-1" "1") (point "1" "-1"))))
      (check= (rel current-path) '(0 2 2)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Groups of objects
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; In the group-edit modes the selected objects are in the sketch: a right
;; click selects the object under the mouse, two right clicks away from
;; any object select the objects in the rectangle between them; a left
;; click starts the operation of the mode, and a second one ends it.
(define (test-select)
  (check-group "select")
  (with-graphics '("gr-mode" (tuple "group-edit" "move"))
                 '((point "0" "0") (point "1" "1") (point "5" "5"))
    (lambda ()
      (check-false (graphics-selection-active?))
      (edit_right-button 'group-edit "0" "0")
      (check= (map tree->stree (sketch-get)) '((point "0" "0")))
      (check-true (graphics-selection-active?))
      (check-true (sketch-in? (path->tree (at 0 2 1))))
      (check-false (sketch-in? (path->tree (at 0 2 2))))
      (edit_right-button 'group-edit "0" "0")
      (check= (sketch-get) '())
      (sketch-toggle (path->tree (at 0 2 3)))
      (check= (map tree->stree (sketch-get)) '((point "5" "5")))
      (sketch-reset)
      (check-false (graphics-selection-active?))))
  (with-graphics '("gr-mode" (tuple "group-edit" "move"))
                 '((point "1" "1") (point "2" "2") (point "5" "5"))
    (lambda ()
      (edit_right-button 'group-edit "-1" "-1")
      (check-true multiselecting)
      (edit_right-button 'group-edit "3" "3")
      (check-false multiselecting)
      (check= (map tree->stree (sketch-get))
              '((point "1" "1") (point "2" "2")))
      ;; a selection removed with shift and the middle button
      (set-keyboard-modifiers ShiftMask)
      (edit_middle-button 'group-edit "0" "0")
      (set-keyboard-modifiers 0)
      (check= (gr) '(graphics "" (point "5" "5")))))
  (with-graphics '("gr-mode" (tuple "group-edit" "move"))
                 '((point "1" "1") (point "2" "2") (point "5" "5"))
    (lambda ()
      (sketch-toggle (path->tree (at 0 2 3)))
      (remove-selected-objects)
      (check= (gr) '(graphics "" (point "1" "1") (point "2" "2")))
      (check= (sketch-get) '()))))

(define grp #f)

(define (test-group)
  (check-group "group")
  (with-graphics '("gr-mode" (tuple "group-edit" "group-ungroup"))
                 '((point "1" "1") (line (point "1" "1") (point "2" "2"))
                   (point "5" "5"))
    (lambda ()
      (edit_right-button 'group-edit "0" "0")
      (edit_right-button 'group-edit "3" "3")
      (check= (length (sketch-get)) 2)
      (with sel (map tree->stree (sketch-get))
        (check-true (in? '(point "1" "1") sel))
        (check-true (in? '(line (point "1" "1") (point "2" "2")) sel))
        ;; the objects of the group are those of the selection
        (edit_left-button 'group-edit "0" "0")
        (check= (gr) `(graphics "" (gr-group ,@sel) (point "5" "5")))
        (check= (map tree->stree (sketch-get)) `((gr-group ,@sel))))
      ;; the group is selected: a left click ungroups it
      (set! grp (cdr (tree->stree (car (sketch-get)))))
      (edit_left-button 'group-edit "0" "0")
      (check= (length (sketch-get)) 2)
      (check= (length (gr)) 5)
      (check= (cAr (gr)) '(point "5" "5"))
      (check-true (in? '(point "1" "1") (gr)))
      (check-true (in? '(line (point "1" "1") (point "2" "2")) (gr)))
      ;; the objects come back in the order of the group
      (check= (gr) `(graphics "" ,@grp (point "5" "5")))))
  ;; group-selected-objects and ungroup-current-object on the sketch
  (with-graphics '("gr-mode" (tuple "group-edit" "move"))
                 '((point "0" "0") (point "1" "1") (point "2" "2"))
    (lambda ()
      (sketch-toggle (path->tree (at 0 2 2)))
      (sketch-toggle (path->tree (at 0 2 3)))
      (group-selected-objects)
      (check= (gr) '(graphics "" (point "0" "0")
                              (gr-group (point "1" "1") (point "2" "2"))))
      (check-false sticky-point)
      ;; nothing to ungroup with two objects selected
      (sketch-toggle (path->tree (at 0 2 1)))
      (ungroup-current-object)
      (check= (gr) '(graphics "" (point "0" "0")
                              (gr-group (point "1" "1") (point "2" "2"))))))
  ;; ungrouping keeps the order of the group, at its place
  (with-graphics '("gr-mode" (tuple "group-edit" "group-ungroup"))
                 '((point "0" "0")
                   (gr-group (point "1" "1") (point "2" "2") (point "3" "3"))
                   (point "5" "5"))
    (lambda ()
      (sketch-toggle (path->tree (at 0 2 2)))
      (ungroup-current-object)
      (check= (gr) '(graphics "" (point "0" "0") (point "1" "1")
                              (point "2" "2") (point "3" "3")
                              (point "5" "5")))
      (check= (map tree->stree (sketch-get))
              '((point "1" "1") (point "2" "2") (point "3" "3"))))))

;; The operations of the group-edit modes are made between two left clicks,
;; by the moves of the mouse between them.
(define (test-transform)
  (check-group "transform")
  (with-graphics '("gr-mode" (tuple "group-edit" "move"))
                 '((point "1" "1") (point "5" "5"))
    (lambda ()
      (edit_right-button 'group-edit "0" "0")
      (edit_right-button 'group-edit "3" "3")
      (check= (map tree->stree (sketch-get)) '((point "1" "1")))
      (edit_left-button 'group-edit "0" "0")
      ;; the objects are taken out of the graphics while they move
      (check-true sticky-point)
      (check= (gr) '(graphics "" (point "5" "5")))
      (edit_move 'group-edit "1" "2")
      (check= (sketch-get) '((point "2.0" "3.0")))
      (edit_move 'group-edit "2" "2")
      (check= (sketch-get) '((point "3.0" "3.0")))
      (edit_left-button 'group-edit "2" "2")
      (check-false sticky-point)
      (check= (gr) '(graphics "" (point "3.0" "3.0") (point "5" "5")))))
  ;; the objects taken out together come back in their order and at their
  ;; places, whatever the order of the selection
  (with-graphics '("gr-mode" (tuple "group-edit" "move"))
                 '((point "1" "1") (point "2" "2"))
    (lambda ()
      (sketch-toggle (path->tree (at 0 2 1)))
      (sketch-toggle (path->tree (at 0 2 2)))
      (sketch-checkout)
      (sketch-commit)
      (check= (gr) '(graphics "" (point "1" "1") (point "2" "2")))
      (check= (map tree->stree (sketch-get))
              '((point "1" "1") (point "2" "2")))))
  (with-graphics '("gr-mode" (tuple "group-edit" "move"))
                 '((point "0" "0") (point "1" "1") (point "5" "5")
                   (point "2" "2") (point "6" "6"))
    (lambda ()
      (sketch-toggle (path->tree (at 0 2 4)))
      (sketch-toggle (path->tree (at 0 2 2)))
      (sketch-checkout)
      (sketch-commit)
      (check= (gr) '(graphics "" (point "0" "0") (point "1" "1")
                              (point "5" "5") (point "2" "2")
                              (point "6" "6")))
      (check= (map tree->stree (sketch-get))
              '((point "2" "2") (point "1" "1")))))
  ;; two objects moved together with the mouse
  (with-graphics '("gr-mode" (tuple "group-edit" "move"))
                 '((point "1" "1") (point "5" "5") (point "2" "2"))
    (lambda ()
      (edit_right-button 'group-edit "0" "0")
      (edit_right-button 'group-edit "3" "3")
      (check= (length (sketch-get)) 2)
      (edit_left-button 'group-edit "0" "0")
      (edit_move 'group-edit "1" "1")
      (edit_left-button 'group-edit "1" "1")
      (check= (gr) '(graphics "" (point "2.0" "2.0") (point "5" "5")
                              (point "3.0" "3.0")))))
  (with-graphics '("gr-mode" (tuple "group-edit" "zoom"))
                 '((line (point "1" "0") (point "3" "0")))
    (lambda ()
      (edit_right-button 'group-edit "0.5" "-1")
      (edit_right-button 'group-edit "4" "1")
      (check= (length (sketch-get)) 1)
      ;; from the origin to (6,0): twice as far from the barycenter (2,0)
      (edit_left-button 'group-edit "0" "0")
      (edit_move 'group-edit "6" "0")
      (edit_left-button 'group-edit "6" "0")
      (check= (gr) '(graphics "" (with "magnify" "2.0"
                                       (line (point "0.0" "0.0")
                                             (point "4.0" "0.0")))))))
  (with-graphics '("gr-mode" (tuple "group-edit" "rotate"))
                 '((line (point "1" "0") (point "3" "0")))
    (lambda ()
      (edit_right-button 'group-edit "0.5" "-1")
      (edit_right-button 'group-edit "4" "1")
      ;; from the origin to (2,-2): a quarter turn around (2,0)
      (edit_left-button 'group-edit "0" "0")
      (edit_move 'group-edit "2" "-2")
      (edit_left-button 'group-edit "2" "-2")
      (with l (cAr (gr))
        (check= (car l) 'line)
        (check-true (point-near? (cadr l) 2 -1))
        (check-true (point-near? (caddr l) 2 1))))))

;; In the edit-props mode the properties are those of the selected
;; objects; "mixed" when they differ.
(define (test-edit-props)
  (check-group "edit props")
  (with-graphics '("gr-mode" (tuple "group-edit" "edit-props"))
                 '((point "0" "0") (line (point "0" "0") (point "1" "1"))
                   (with "color" "red" (point "2" "2")))
    (lambda ()
      (sketch-toggle (path->tree (at 0 2 1)))
      (sketch-toggle (path->tree (at 0 2 3 2)))
      (check= (map tree->stree (sketch-get))
              '((point "0" "0") (with "color" "red" (point "2" "2"))))
      (check= (graphics-get-property "gr-color") "mixed")
      (check= (graphics-get-property "gr-point-style") "default")
      (check-true (graphics-mode-attribute? (graphics-mode) "gr-color"))
      (check-false (graphics-mode-attribute? (graphics-mode) "line-width"))
      (graphics-set-property "gr-color" "green")
      (check= (gr) '(graphics "" (with "color" "green" (point "0" "0"))
                              (line (point "0" "0") (point "1" "1"))
                              (with "color" "green" (point "2" "2"))))
      (check= (graphics-get-property "gr-color") "green")
      (graphics-set-property "gr-color" "default")
      (check= (gr) '(graphics "" (point "0" "0")
                              (line (point "0" "0") (point "1" "1"))
                              (point "2" "2")))
      ;; a property which the objects do not have goes to the graphics
      (graphics-set-property "gr-line-width" "2ln")
      (check= (attrs) '("gr-mode" (tuple "group-edit" "edit-props")
                        "gr-line-width" "2ln")))))

;; Copy, cut and paste of the selected objects, as a graphics.
(define (test-clipboard)
  (check-group "clipboard")
  (with-graphics '("gr-mode" (tuple "group-edit" "move"))
                 '((point "0" "0") (point "1" "1") (point "2" "2"))
    (lambda ()
      (check= (tree->stree (graphics-copy)) "")
      (sketch-toggle (path->tree (at 0 2 1)))
      (sketch-toggle (path->tree (at 0 2 3)))
      (check= (tree->stree (graphics-copy))
              '(graphics (point "0" "0") (point "2" "2")))
      ;; the selection is gone, the objects stay
      (check= (sketch-get) '())
      (check= (gr) '(graphics "" (point "0" "0") (point "1" "1")
                              (point "2" "2")))
      (sketch-toggle (path->tree (at 0 2 2)))
      (check= (tree->stree (graphics-cut)) '(graphics (point "1" "1")))
      (check= (gr) '(graphics "" (point "0" "0") (point "2" "2")))
      (graphics-paste (stree->tree '(graphics (point "7" "7"))))
      (check= (gr) '(graphics "" (point "0" "0") (point "2" "2")
                              (point "7" "7")))
      ;; the pasted objects are selected
      (check= (map tree->stree (sketch-get)) '((point "7" "7")))
      ;; not a graphics: nothing
      (graphics-paste (stree->tree "text"))
      (check= (length (gr)) 5)))
  ;; outside the group modes, nothing is copied
  (with-graphics '("gr-mode" (tuple "edit" "point")) '((point "0" "0"))
    (lambda ()
      (sketch-toggle (path->tree (at 0 2 1)))
      (check= (tree->stree (graphics-copy)) "")
      (check= (tree->stree (graphics-cut)) "")
      (sketch-reset))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Main
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (graphics-edit-test-failures)
  (check-suite "graphics-edit")
  (run-group test-drd)
  (run-group test-object-attributes)
  (run-group test-insert)
  (run-group test-properties)
  (run-group test-geometry)
  (run-group test-grid)
  (run-group test-modes)
  (run-group test-typeset)
  (run-group test-create)
  (run-group test-objects)
  (run-group test-zmove)
  (run-group test-select)
  (run-group test-group)
  (run-group test-transform)
  (run-group test-edit-props)
  (run-group test-clipboard)
  (check-end))
