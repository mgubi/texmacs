
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : menu-define.scm
;; DESCRIPTION : Definition of menus
;; COPYRIGHT   : (C) 1999  Joris van der Hoeven
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (kernel gui menu-define)
  (:use (kernel gui gui-markup)))

(define use-minibars? (== (cpp-get-preference "use minibars" "off") "on"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Definition of dynamic menus
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (require-format x pattern)
  (if (not (match? x pattern))
    (texmacs-error "gui-make" "invalid menu item ~S" x)))

(define (gui-make-eval x)
  (require-format x '(eval :%1))
  (cadr x))

(define (gui-make-dynamic x)
  (require-format x '(dynamic :%1))
  `($dynamic ,(cadr x)))

(define (gui-make-former x)
  (require-format x '(former :*))
  `($dynamic ,x))

(define (gui-make-link x)
  (require-format x '(link :%1))
  `($menu-link ,(cadr x)))

(define (gui-make-let x)
  (require-format x '(:%1 :%1 :*))
  `(,(car x) ,(cadr x) (menu-dynamic ,@(cddr x))))

(define (gui-make-with x)
  (require-format x '(:%1 :%2 :*))
  `(,(car x) ,(cadr x) ,(caddr x) (menu-dynamic ,@(cdddr x))))

(define (gui-make-push-focus x)
  (require-format x '(push-focus :%1 :*))
  `(with pushed-tree ,(cadr x)
     (with pushed-focus (tree->fingerprint pushed-tree)
       (menu-dynamic
         (invisible (tree->path pushed-tree))
         ,@(cddr x)))))

(define (gui-make-cond x)
  (require-format x '(cond :*))
  (with fun (lambda (x)
              (with (pred? . body) x
                (list pred? (cons* 'menu-dynamic body))))
    `(cond ,@(map fun (cdr x)))))

(define (gui-make-loop x)
  (require-format x '(loop (:%1 :%1) :*))
  (with fun `(lambda (,(caadr x)) (menu-dynamic ,@(cddr x)))
    `($dynamic (append-map ,fun ,(cadadr x)))))

(define (gui-make-refresh x)
  (require-format x '(refresh :%1 :*))
  (with opts (cddr x)
    (when (and (null? opts) (symbol? (cadr x)))
      (set! opts (list (cadr x))))
    (when (not (symbol? (car opts)))
      (texmacs-error "gui-make-refresh" "invalid menu item ~S" x))
    `($refresh ,(cadr x) ,(symbol->string (car opts)))))

(define (gui-make-refreshable x)
  (require-format x '(refreshable :%1 :*))
  `($refreshable ,(cadr x) ,@(map gui-make (cddr x))))

(define (gui-make-cached x)
  (require-format x '(cached :%1 :%1 :*))
  `($cached ,(cadr x) ,(caddr x) ,@(map gui-make (cdddr x))))

(define (gui-make-group x)
  (require-format x '(group :%1))
  `($menu-group ,(cadr x)))

(define (gui-make-text x)
  (require-format x '(text :%1))
  `($menu-text ,(cadr x)))

(define (gui-make-invisible x)
  (require-format x '(invisible :%1))
  `($menu-invisible ,(cadr x)))

(define (gui-make-glue x)
  (require-format x '(glue :%4))
  `($glue ,(second x) ,(third x) ,(fourth x) ,(fifth x)))

(define (gui-make-color x)
  (require-format x '(color :%5))
  `($colored-glue ,(second x) ,(third x) ,(fourth x) ,(fifth x) ,(sixth x)))

(define (gui-make-texmacs-output x)
  (require-format x '(texmacs-output :%2))
  `($texmacs-output ,@(cdr x)))

(define (gui-make-texmacs-input x)
  (require-format x '(texmacs-input :%3))
  `($texmacs-input ,@(cdr x)))

(define (gui-make-input x)
  (require-format x '(input :%4))
  `($input ,@(cdr x)))

(define (gui-make-enum x)
  (require-format x '(enum :%4))
  `($enum ,@(cdr x)))

(define (gui-make-setting-enum x)
  (require-format x '(setting-enum :%5))
  `($setting-enum ,@(cdr x)))

(define (gui-make-setting-group x)
  (require-format x '(setting-group :%1 :*))
  `($setting-group ,(cadr x) ,@(map gui-make (cddr x))))

(define (gui-make-choice x)
  (require-format x '(choice :%3))
  `($choice ,@(cdr x)))

(define (gui-make-choices x)
  (require-format x '(choices :%3))
  `($choices ,@(cdr x)))

(define (gui-make-filtered-choice x)
  (require-format x '(filtered-choice :%4))
  `($filtered-choice ,@(cdr x)))

(define (gui-make-color-input x)
  (require-format x '(color-input :%3))
  `($color-input ,@(cdr x)))

(define (gui-make-tree-view x)
  (require-format x '(tree-view :%3))
  `($tree-view ,@(cdr x)))

(define (gui-make-toggle x)
  (require-format x '(toggle :%2))
  `($toggle ,@(cdr x)))

(define (gui-make-setting-toggle x)
  (require-format x '(setting-toggle :%3))
  `($setting-toggle ,@(cdr x)))

(define (gui-make-icon x)
  (require-format x '(icon :%1))
  `($icon ,(cadr x)))

(define (gui-make-replace x)
  (require-format x '(replace :%1 :*))
  `($replace-text ,(cadr x) ,@(cddr x)))

(define (gui-make-concat x)
  (require-format x '(concat :*))
  `($concat-text ,@(cdr x)))

(define (gui-make-verbatim x)
  (require-format x '(verbatim :*))
  `($verbatim-text ,@(cdr x)))

(define (gui-make-check x)
  (require-format x '(check :%3))
  `($check ,(gui-make (cadr x)) ,(caddr x) ,(cadddr x)))

(define (gui-make-shortcut x)
  (require-format x '(shortcut :%2))
  `($shortcut* ,(gui-make (cadr x)) ,(caddr x)))

(define (gui-make-balloon x)
  (require-format x '(balloon :%2))
  `($balloon ,(gui-make (cadr x)) ,(gui-make (caddr x))))

(define (gui-make-submenu x)
  (require-format x '(-> :%1 :*))
  `($-> ,@(map gui-make (cdr x))))

(define (gui-make-top-submenu x)
  (require-format x '(=> :%1 :*))
  `($=> ,@(map gui-make (cdr x))))

(define (gui-make-horizontal x)
  (require-format x '(horizontal :*))
  `($horizontal ,@(map gui-make (cdr x))))

(define (gui-make-vertical x)
  (require-format x '(vertical :*))
  `($vertical ,@(map gui-make (cdr x))))

(define (gui-make-hlist x)
  (require-format x '(hlist :*))
  `($hlist ,@(map gui-make (cdr x))))

(define (gui-make-vlist x)
  (require-format x '(vlist :*))
  `($vlist ,@(map gui-make (cdr x))))

(define (gui-make-division x)
  (require-format x '(division :%1 :*))
  `($division ,(cadr x) ,@(map gui-make (cddr x))))

(define (gui-make-class x)
  (require-format x '(class :%1 :*))
  `($class ,(cadr x) ,@(map gui-make (cddr x))))

(define (gui-make-aligned x)
  (require-format x '(aligned :*))
  `($aligned ,@(map gui-make (cdr x))))

(define (gui-make-item x)
  (require-format x '(item :%2))
  `($aligned-item ,@(map gui-make (cdr x))))

(define (gui-make-meti x)
  (require-format x '(meti :%2))
  `($aligned-item ,@(map gui-make (reverse (cdr x)))))

(define (gui-make-tabs x)
  (require-format x '(tabs :*))
  `($tabs ,@(map gui-make (cdr x))))

(define (gui-make-tab x)
  (require-format x '(tab :%1 :*))
  `($tab ,@(map gui-make (cdr x))))

(define (gui-make-responsive-tabs x)
  (require-format x '(responsive-tabs :*))
  `($responsive-tabs ,@(map gui-make (cdr x))))

(define (gui-make-responsive-tab x)
  (require-format x '(responsive-tab :%1 :*))
  `($responsive-tab ,@(map gui-make (cdr x))))

(define (gui-make-icon-tabs x)
  (require-format x '(icon-tabs :*))
  `($icon-tabs ,@(map gui-make (cdr x))))

(define (gui-make-icon-tab x)
  (require-format x '(icon-tab :%2 :*))
  `($icon-tab ,@(map gui-make (cdr x))))

(define (gui-make-responsive-icon-tabs x)
  (require-format x '(responsive-icon-tabs :*))
  `($responsive-icon-tabs ,@(map gui-make (cdr x))))

(define (gui-make-responsive-icon-tab x)
  (require-format x '(responsive-icon-tab :%2 :*))
  `($responsive-icon-tab ,@(map gui-make (cdr x))))

(define (gui-make-plain-style x)
  (require-format x '(plain-style :*))
  `($widget-style 0 ,@(map gui-make (cdr x))))

(define (gui-make-inert x)
  (require-format x '(inert :*))
  `($widget-style ,widget-style-inert ,@(map gui-make (cdr x))))

(define (gui-make-explicit-buttons x)
  (require-format x '(explicit-buttons :*))
  `($widget-style ,widget-style-button ,@(map gui-make (cdr x))))

(define (gui-make-bold x)
  (require-format x '(bold :*))
  `($widget-style ,widget-style-bold ,@(map gui-make (cdr x))))

(define (gui-make-grey x)
  (require-format x '(grey :*))
  `($widget-style ,widget-style-grey ,@(map gui-make (cdr x))))

(define (gui-make-monospaced x)
  (require-format x '(mono :*))
  `($widget-style ,widget-style-monospaced ,@(map gui-make (cdr x))))

(define (gui-make-verb x)
  (require-format x '(verb :*))
  `($widget-style ,widget-style-verb ,@(map gui-make (cdr x))))

(define (gui-make-tile x)
  (require-format x '(tile :integer? :*))
  `($tile ,(cadr x) ,@(map gui-make (cddr x))))

(define (gui-make-scrollable x)
  (require-format x '(scrollable :*))
  `($scrollable ,@(map gui-make (cdr x))))

(define (gui-make-resize x)
  (require-format x '(resize :%2 :*))
  `($resize ,(cadr x) ,(caddr x) ,@(map gui-make (cdddr x))))

(define (gui-make-hsplit x)
  (require-format x '(hsplit :%2))
  `($hsplit ,@(map gui-make (cdr x))))

(define (gui-make-vsplit x)
  (require-format x '(vsplit :%2))
  `($vsplit ,@(map gui-make (cdr x))))

(define (gui-make-minibar x)
  (require-format x '(minibar :*))
  (if use-minibars?
      `(gui$minibar ,@(map gui-make (cdr x)))
      `($when #t ,@(map gui-make (cdr x)))))

(define (gui-make-extend x)
  (require-format x '(extend :%1 :*))
  `($widget-extend ,@(map gui-make (cdr x))))

(define (gui-make-padded x)
  (require-format x '(padded :*))
  `($vlist
     ($glue #f #f 0 10)
     ($hlist
       ($glue #f #f 25 0)
       ($vlist ,@(map gui-make (cdr x)))
       ($glue #f #f 25 0))
     ($glue #f #f 0 10)))

(define (gui-make-centered x)
  (require-format x '(centered :*))
  `($vlist
     ($glue #f #f 0 10)
     ($hlist
       ($glue #t #f 25 0)
       ($vlist ,@(map gui-make (cdr x)))
       ($glue #t #f 25 0))
     ($glue #f #f 0 10)))

(define (gui-make-bottom-buttons x)
  (require-format x '(bottom-buttons :*))
  `($vlist
     $---
     ($glue #f #f 0 5)
     ($hlist
       ($glue #f #f 5 0)
       ($widget-style ,widget-style-button
         ,@(map gui-make (cdr x)))
       ($glue #f #f 5 0))
     ($glue #f #f 0 5)))

(define (gui-make-assuming x)
  (require-format x '(assuming :%1 :*))
  `($when ,(cadr x) ,@(map gui-make (cddr x))))

(define (gui-make-if x)
  (require-format x '(if :%1 :*))
  `($delayed-when ,(cadr x) ,@(map gui-make (cddr x))))

(define (gui-make-when x)
  (require-format x '(when :%1 :*))
  `($assuming ,(cadr x) ,@(map gui-make (cddr x))))

(define (gui-make-for x)
  (require-format x '(for (:%1 :%1) :*))
  `($for* ,(cadr x) ,@(map gui-make (cddr x))))

(define (gui-make-mini x)
  (require-format x '(mini :%1 :*))
  (if use-minibars?
      `($mini ,(cadr x) ,@(map gui-make (cddr x)))
      `($when #t ,@(map gui-make (cddr x)))))

(define (gui-make-symbol x)
  (require-format x '(symbol :string? :*))
  `($symbol ,@(cdr x)))

(define (gui-make-promise x)
  (require-format x '(promise :%1))
  `($promise ,(cadr x)))

(define (gui-make-ink x)
  (require-format x '(ink :%1))
  `($ink ,(cadr x)))

(define (gui-make-form x)
  (require-format x '(form :%1 :*))
  `($form ,@(map gui-make (cdr x))))

(define (gui-make-form-input x)
  (require-format x '(form-input :%4))
  `($form-input ,@(cdr x)))

(define (gui-make-form-enum x)
  (require-format x '(form-enum :%4))
  `($form-enum ,@(cdr x)))

(define (gui-make-form-choice x)
  (require-format x '(form-choice :%3))
  `($form-choice ,@(cdr x)))

(define (gui-make-form-choices x)
  (require-format x '(form-choices :%3))
  `($form-choices ,@(cdr x)))

(define (gui-make-form-toggle x)
  (require-format x '(form-toggle :%2))
  `($form-toggle ,@(cdr x)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Table with Gui primitives and dispatching
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-table gui-make-table
  (eval ,gui-make-eval)
  (dynamic ,gui-make-dynamic)
  (former ,gui-make-former)
  (link ,gui-make-link)
  (let ,gui-make-let)
  (let* ,gui-make-let)
  (with ,gui-make-with)
  (push-focus ,gui-make-push-focus)
  (receive ,gui-make-with)
  (cond ,gui-make-cond)
  (loop ,gui-make-loop)
  (refresh ,gui-make-refresh)
  (refreshable ,gui-make-refreshable)
  (cached ,gui-make-cached)
  (group ,gui-make-group)
  (text ,gui-make-text)
  (invisible ,gui-make-invisible)
  (glue ,gui-make-glue)
  (color ,gui-make-color)
  (texmacs-output ,gui-make-texmacs-output)
  (texmacs-input ,gui-make-texmacs-input)
  (input ,gui-make-input)
  (enum ,gui-make-enum)
  (setting-enum ,gui-make-setting-enum)
  (setting-group ,gui-make-setting-group)
  (choice ,gui-make-choice)
  (choices ,gui-make-choices)
  (tree-view ,gui-make-tree-view)
  (filtered-choice ,gui-make-filtered-choice)
  (color-input ,gui-make-color-input)
  (toggle ,gui-make-toggle)
  (setting-toggle ,gui-make-setting-toggle)
  (icon ,gui-make-icon)
  (replace ,gui-make-replace)
  (concat ,gui-make-concat)
  (verbatim ,gui-make-verbatim)
  (check ,gui-make-check)
  (shortcut ,gui-make-shortcut)
  (balloon ,gui-make-balloon)
  (-> ,gui-make-submenu)
  (=> ,gui-make-top-submenu)
  (horizontal ,gui-make-horizontal)
  (vertical ,gui-make-vertical)
  (hlist ,gui-make-hlist)
  (vlist ,gui-make-vlist)
  (division ,gui-make-division)
  (class ,gui-make-class)
  (aligned ,gui-make-aligned)
  (item ,gui-make-item)
  (meti ,gui-make-meti)
  (tabs ,gui-make-tabs)
  (tab ,gui-make-tab)
  (icon-tabs ,gui-make-icon-tabs)
  (icon-tab ,gui-make-icon-tab)
  (responsive-tabs ,gui-make-responsive-tabs)
  (responsive-tab ,gui-make-responsive-tab)
  (responsive-icon-tabs ,gui-make-responsive-icon-tabs)
  (responsive-icon-tab ,gui-make-responsive-icon-tab)
  (plain-style ,gui-make-plain-style)
  (inert ,gui-make-inert)
  (explicit-buttons ,gui-make-explicit-buttons)
  (bold ,gui-make-bold)
  (grey ,gui-make-grey)
  (mono ,gui-make-monospaced)
  (verb ,gui-make-verb)
  (tile ,gui-make-tile)
  (scrollable ,gui-make-scrollable)
  (resize ,gui-make-resize)
  (hsplit ,gui-make-hsplit)
  (vsplit ,gui-make-vsplit)
  (minibar ,gui-make-minibar)
  (extend ,gui-make-extend)
  (padded ,gui-make-padded)
  (centered ,gui-make-centered)
  (bottom-buttons ,gui-make-bottom-buttons)
  (assuming ,gui-make-assuming)
  (if ,gui-make-if)
  (when ,gui-make-when)
  (for ,gui-make-for)
  (mini ,gui-make-mini)
  (symbol ,gui-make-symbol)
  (promise ,gui-make-promise)
  (ink ,gui-make-ink)
  (form ,gui-make-form)
  (form-input ,gui-make-form-input)
  (form-enum ,gui-make-form-enum)
  (form-choice ,gui-make-form-choice)
  (form-choices ,gui-make-form-choices)
  (form-toggle ,gui-make-form-toggle))

(tm-define (gui-make x)
  ;;(display* "x= " x "\n")
  (cond ((symbol? x)
         (cond ((== x '---) '$---)
               ((== x '===) (gui-make '(glue #f #f 0 5)))
               ((== x '======) (gui-make '(glue #f #f 0 15)))
               ((== x '/) '$/)
               ((== x '//) (gui-make '(glue #f #f 5 0)))
               ((== x '///) (gui-make '(glue #f #f 15 0)))
               ((== x '>>) (gui-make '(glue #t #f 5 0)))
               ((== x '>>>) (gui-make '(glue #t #f 15 0)))
               ((== x (string->symbol "|")) '$/)
               (else
                 (texmacs-error "gui-make" "invalid menu item ~S" x))))
        ((string? x) x)
        ((and (pair? x) (ahash-ref gui-make-table (car x)))
         (apply (car (ahash-ref gui-make-table (car x))) (list x)))
        ((and (pair? x) (or (string? (car x)) (pair? (car x))))
         `($> ,(gui-make (car x)) ,@(cdr x)))
        (else
          (texmacs-error "gui-make" "invalid menu item ~S" x))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; User interface for dynamic menu definitions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define-macro (menu-dynamic . l)
  `($list ,@(map gui-make l)))

(tm-define-macro (define-menu head . body)
  `(define ,head (menu-dynamic ,@body)))

(tm-define-macro (define-widget head . body)
  `(define ,head (menu-dynamic ,@body)))

(tm-define-macro (tm-menu head . l)
  (receive (opts body) (list-break l not-define-option?)
    `(tm-define ,head ,@opts (menu-dynamic ,@body))))

(tm-define-macro (tm-widget head . l)
  (receive (opts body) (list-break l not-define-option?)
    `(tm-define ,head ,@opts (menu-dynamic ,@body))))

(tm-define-macro (menu-bind name . l)
  ;;(display* name " --> " l "\n")
  (receive (opts body) (list-break l not-define-option?)
    `(tm-define (,name) ,@opts (menu-dynamic ,@body))))

(define-public-macro (lazy-menu module . menus)
  `(begin
     (lazy-define ,module ,@menus)
     (delayed
       (:idle 500)
       (module-provide ',module))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Section tabs
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define section-tab-table (make-ahash-table))

(tm-define (section-tab-ref name key)
  (or (ahash-ref section-tab-table (list name key)) 0))

(tm-define (section-tab-set name key val)
  (ahash-set! section-tab-table (list name key) val)
  (refresh-now name)
  (keyboard-focus-on "canvas"))

(define (make-section-tab* name key ts i j)
  (let* ((t (list-ref ts j))
         (title (cadr t)))
    (if (== i j)
        `(class "section-active-tab" (,title (noop)))
        `(,title (section-tab-set ,name ,key ,j)))))

(define (make-section-tab name key ts i)
  (with t (list-ref ts i)
    (require-format t '(section-tab :%1 :*))
    `(assuming (== (section-tab-ref ,name ,key) ,i)
       (division "section-tabs"
         ===
         (hlist
           ,@(map (lambda (j) (make-section-tab* name key ts i j))
                  (.. 0 (length ts)))
           >>>))
       ,@(cddr t))))

(define (gui-make-section-tabs x)
  (require-format x '(section-tabs :%2 :*))
  (with (tag name key . ts) x
    `(menu-dynamic
       (refreshable ,name
         ,@(map (lambda (i) (make-section-tab name key ts i))
                (.. 0 (length ts)))))))

(extend-table gui-make-table
  (section-tabs ,gui-make-section-tabs))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Basic color pickers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (standard-color-list)
  '("dark red" "dark magenta" "dark blue" "dark cyan"
    "dark green" "dark yellow" "dark orange" "dark brown"
    "red" "magenta" "blue" "cyan"
    "green" "yellow" "orange" "brown"
    "#faa" "#faf" "#aaf" "#aff"
    "#afa" "#ffa" "#fa6" "#a66"
    "pastel red" "pastel magenta" "pastel blue" "pastel cyan"
    "pastel green" "pastel yellow" "pastel orange" "pastel brown"))

(define (standard-grey-list)
  '("black" "darker grey" "dark grey" "#a0a0a0"
    "light grey" "pastel grey" "#f0f0f0" "white"))

;; Palettes for typographic design (the preference "typographic palette",
;; Vue), in several sets, the one in use being the preference "typographic
;; palette set". A set has families of hues, the columns (eight in the sets
;; below, a neutral one first), each in five tones, the rows, grouped by
;; their use in a document: the inks and the deep tones are dark enough for
;; text on white paper, the medium ones are accents (headings, emphasis,
;; rules), the soft ones and the tints are backgrounds (boxes, highlighted
;; text, table cells) on which the inks stay legible. Within a column the
;; colours go together; across a row they have the same weight, so that any
;; two of them can be mixed.
;;
;; The sets are defined with define-typographic-palette, or computed from a
;; colour per column with define-typographic-palette-from-colors; a user
;; adds or replaces sets in the same way, from ~/.TeXmacs/progs/
;; my-init-texmacs.scm (see src/docs/typographic-palettes.md).

(define typographic-palettes (list)) ; (name ink deep medium soft tint)

(define (typographic-palette-error name what)
  (texmacs-error "define-typographic-palette" "~S: ~A" name what))

(define-public (define-typographic-palette name ink deep medium soft tint)
  ;; the set @name, given by its five rows of colours (as many columns in
  ;; each, colours of TeXmacs: "#rrggbb", "dark red"...); a set of the same
  ;; name is replaced, where it was in the list
  (let ((rows (list ink deep medium soft tint)))
    (cond ((not (string? name))
           (typographic-palette-error name "the name must be a string"))
          ((not (and (list-and (map list? rows))
                     (list-and (map (lambda (r) (list-and (map string? r))) rows))))
           (typographic-palette-error name "the rows must be lists of colours"))
          ((or (null? ink)
               (list-or (map (lambda (r) (!= (length r) (length ink))) rows)))
           (typographic-palette-error name "the rows must have the same length"))
          ((assoc name typographic-palettes)
           (set! typographic-palettes
                 (map (lambda (p) (if (== (car p) name) (cons name rows) p))
                      typographic-palettes)))
          (else
           (set! typographic-palettes
                 (append typographic-palettes (list (cons name rows))))))))

;; the tones of a colour, from the tones of its hue at some lightnesses and
;; with its saturation scaled (the algorithm of the HLS model)

(define (hex->rgb c)
  (map (lambda (i) (/ (string->number (substring c i (+ i 2)) 16) 255.0))
       (list 1 3 5)))

(define (rgb->hex l)
  (string-downcase
   (apply string-append "#"
         (map (lambda (v)
                (integer->padded-hexadecimal
                 (inexact->exact (round (* 255 (max 0.0 (min 1.0 v))))) 2))
              l))))

(define (fraction x) (- x (floor x)))

(define (rgb->hls r g b)
  (let* ((mx (max r g b)) (mn (min r g b)) (l (/ (+ mx mn) 2)))
    (if (== mx mn) (list 0.0 l 0.0)
        (let* ((d (- mx mn))
               (s (if (<= l 0.5) (/ d (+ mx mn)) (/ d (- 2.0 mx mn))))
               (rc (/ (- mx r) d)) (gc (/ (- mx g) d)) (bc (/ (- mx b) d))
               (h (cond ((== r mx) (- bc gc))
                        ((== g mx) (+ 2.0 (- rc bc)))
                        (else (+ 4.0 (- gc rc))))))
          (list (fraction (/ h 6.0)) l s)))))

(define (hls-value m1 m2 h)
  (let ((h (fraction h)))
    (cond ((< h (/ 1.0 6)) (+ m1 (* (- m2 m1) h 6.0)))
          ((< h 0.5) m2)
          ((< h (/ 2.0 3)) (+ m1 (* (- m2 m1) (- (/ 2.0 3) h) 6.0)))
          (else m1))))

(define (hls->rgb h l s)
  (if (== s 0.0) (list l l l)
      (let* ((m2 (if (<= l 0.5) (* l (+ 1.0 s)) (- (+ l s) (* l s))))
             (m1 (- (* 2.0 l) m2)))
        (list (hls-value m1 m2 (+ h (/ 1.0 3)))
              (hls-value m1 m2 h)
              (hls-value m1 m2 (- h (/ 1.0 3)))))))

(define-public (typographic-color-tones c lightnesses saturations)
  ;; the colour @c ("#rrggbb") at each of the @lightnesses (from 0 to 1),
  ;; with its saturation multiplied by the corresponding factor
  (with (h l s) (apply rgb->hls (hex->rgb c))
    (map (lambda (l2 f) (rgb->hex (hls->rgb h l2 (min 1.0 (* s f)))))
         lightnesses saturations)))

(define-public (define-typographic-palette-from-colors name colors . opt)
  ;; the set @name whose columns are the tones of the @colors ("#rrggbb",
  ;; a neutral one first, say); optionally, the five lightnesses of the
  ;; tones (ink, deep, medium, soft, tint) and five factors for their
  ;; saturation
  (let* ((ls (if (>= (length opt) 1) (car opt) (list 0.17 0.30 0.50 0.78 0.94)))
         (ss (if (>= (length opt) 2) (cadr opt) (list 0.8 0.8 0.85 0.9 1.0)))
         (cols (map (lambda (c) (typographic-color-tones c ls ss)) colors))
         (row (lambda (i) (map (lambda (col) (list-ref col i)) cols))))
    (define-typographic-palette name (row 0) (row 1) (row 2) (row 3) (row 4))))

;; the sets of TeXmacs: muted colours; the inks of printing; the colours of
;; the earth; cold northern ones; pastels; the colours of Solarized (Ethan
;; Schoonover). The columns: neutral, red, brown, ochre, green, teal, blue,
;; violet; the rows: ink, deep, medium, soft, tint

(define-typographic-palette "Muted"
  '("#212529" "#7a1f1f" "#5c3b1e" "#6b5510" "#1f4d2b" "#134e4a" "#1b2f5e" "#3f2358")
  '("#495057" "#a8323a" "#8a5a2b" "#967a17" "#2f7040" "#1d6f6a" "#2a4a8c" "#5e3a82")
  '("#868e96" "#d4575b" "#b98347" "#c9a227" "#4f9a60" "#2f9c94" "#4a72c2" "#8763b0")
  '("#ced4da" "#eba5a3" "#dcb98c" "#e6d08a" "#a3cfa9" "#95d0ca" "#a3bce6" "#c2acdc")
  '("#f1f3f5" "#fbe9e7" "#f6ecdf" "#fbf5dc" "#e8f4ea" "#e3f4f2" "#e8effa" "#f1ebf8"))

(define-typographic-palette "Classic print"
  '("#212121" "#390c0a" "#37200b" "#3f2e04" "#123013" "#00423a" "#0b1b37" "#270a38")
  '("#454545" "#761914" "#734316" "#825f08" "#256528" "#008a79" "#173973" "#511575")
  '("#6b6b6b" "#ac312a" "#a8692e" "#bc8d1a" "#419545" "#1f8f80" "#2e5ba8" "#7b2bab")
  '("#cccccc" "#e7b4b1" "#e6cbb2" "#eedaaa" "#badebc" "#b3dcd6" "#b2c5e6" "#d3b1e7")
  '("#f2f2f2" "#f9edec" "#f8f2ec" "#faf6ea" "#eef6ef" "#eaf5f3" "#ecf1f8" "#f4ecf9"))

(define-typographic-palette "Earth"
  '("#2f2b28" "#3d211a" "#392e1d" "#3b351b" "#2a3423" "#24332f" "#252b31" "#31262b")
  '("#524c47" "#6b3a2e" "#655134" "#695e30" "#4b5b3e" "#3f5a54" "#424c57" "#56434c")
  '("#898076" "#b3614c" "#a88757" "#ae9d51" "#7c9867" "#6a958b" "#6e7f91" "#8f7080")
  '("#ccc7c2" "#e2b7ac" "#dccbb2" "#dfd6ae" "#c5d4ba" "#bbd2cd" "#bec7d0" "#cfbfc7")
  '("#efedec" "#f6e8e4" "#f4efe6" "#f5f2e5" "#edf2e9" "#e9f1ef" "#eaedf0" "#f0eaed"))

(define-typographic-palette "Nordic"
  '("#303337" "#3b2b2d" "#39332d" "#3b382b" "#2b3b34" "#253b41" "#29333d" "#2e2d39")
  '("#4e545a" "#61474a" "#5f5449" "#635e46" "#456356" "#3c636d" "#435465" "#4b495f")
  '("#7b858e" "#997074" "#968573" "#9b936e" "#6e9b88" "#5f9bab" "#6a859f" "#767496")
  '("#c7ccd1" "#d7c1c4" "#d5ccc3" "#d8d4c0" "#c0d8ce" "#b8d8e0" "#beccda" "#c4c3d5")
  '("#eef0f1" "#f3eced" "#f3f0ed" "#f4f2ec" "#ecf4f0" "#e9f4f6" "#ebf0f4" "#ededf3"))

(define-typographic-palette "Pastel"
  '("#3e3f41" "#542c2c" "#543d2b" "#534b2c" "#354a3a" "#334c4b" "#2c3653" "#3f3050")
  '("#595b5f" "#7e3a3a" "#7f5738" "#7d703a" "#496e53" "#46716f" "#3a4b7d" "#5b4077")
  '("#a5a8ac" "#ce8383" "#cfa381" "#cdbe84" "#94bd9e" "#91c0be" "#8396cd" "#a88ac7")
  '("#d5d6d8" "#e8c4c4" "#e9d4c4" "#e8e1c5" "#cde0d1" "#cbe1e0" "#c5cee8" "#d6c8e4")
  '("#f2f2f3" "#f9ecec" "#f9f1ec" "#f8f6ec" "#eff6f1" "#eef6f6" "#eceff8" "#f2edf7"))

(define-typographic-palette "Solarized"
  '("#002b36" "#460d0c" "#4a1b08" "#523e00" "#475200" "#11413d" "#0d2e45" "#17193a")
  '("#073642" "#781917" "#7e3111" "#8b6a04" "#798b04" "#206f69" "#185076" "#2a2e64")
  '("#586e75" "#dc322f" "#cb4b16" "#b58900" "#859900" "#2aa198" "#268bd2" "#6c71c4")
  '("#eee8d5" "#e5b3b3" "#e9c0af" "#f0dea8" "#e6f0a8" "#b7e1de" "#b3d0e5" "#bdbedb")
  '("#fdf6e3" "#f5e6e6" "#f6eae5" "#f8f3e2" "#f5f8e2" "#e7f3f2" "#e6eef5" "#e9e9f2"))

(define-public (typographic-palette-names)
  (map car typographic-palettes))

(tm-define (typographic-palette-set)
  (with name (get-preference "typographic palette set")
    (cond ((assoc name typographic-palettes) name)
          ((null? typographic-palettes) "")
          (else (car (typographic-palette-names))))))

(tm-define (set-typographic-palette-set name)
  (set-preference "typographic palette set" name)
  (refresh-now "typographic-palette"))

(define (typographic-palette-rows from to)
  (with p (assoc (typographic-palette-set) typographic-palettes)
    (if p (apply append (sublist (cdr p) from to)) (list))))


(tm-define (typographic-palette?)
  (and (vue-gui?) (== (get-preference "typographic palette") "on")))

(tm-menu (typographic-color-tiles cmd l)
  ;; eight colours a line (tile wants a number, not an expression): the
  ;; rows of a set of eight columns are lines of the grid
  (tile 8
    (for (col l)
      (explicit-buttons
        ((color col #f #f 32 24)
         (cmd col))))))

(tm-define (set-typographic-palette on?)
  (set-preference "typographic palette" (if on? "on" "off"))
  (refresh-now "typographic-palette"))

(tm-menu (typographic-color-menu cmd)
  (hlist
    // // (text "Set:") //
    (enum (set-typographic-palette-set answer)
          (typographic-palette-names) (typographic-palette-set) "10em")
    >>)
  (group "Text")
  (dynamic (typographic-color-tiles cmd (typographic-palette-rows 0 2)))
  (group "Accents")
  (dynamic (typographic-color-tiles cmd (typographic-palette-rows 2 3)))
  (group "Backgrounds")
  (dynamic (typographic-color-tiles cmd (typographic-palette-rows 3 5))))

(tm-menu (standard-color-tiles cmd)
  (tile 8
    (for (col (append (standard-color-list) (standard-grey-list)))
      (explicit-buttons
        ((color col #f #f 32 24)
         (cmd col))))))

(tm-menu (standard-color-grid cmd)
  (if (typographic-palette?)
      (dynamic (typographic-color-menu cmd)))
  (if (not (typographic-palette?))
      (dynamic (standard-color-tiles cmd)))
  ---
  (hlist
    // //
    (toggle (set-typographic-palette answer) (typographic-palette?))
    // (text "Typographic palette") >>))

(tm-menu (standard-color-menu cmd)
  ;; in Vue, the typographic palette can replace the standard one, and the
  ;; choices of the palette and of its set (a toggle, an enum, which do not
  ;; close the menu) change the grid in place: a promise in a refreshable,
  ;; whose items are made again when it is refreshed
  (if (vue-gui?)
      (refreshable "typographic-palette"
        (promise (cons 'vertical (standard-color-grid cmd)))))
  (if (not (vue-gui?))
      (dynamic (standard-color-tiles cmd))))

(define (gui-make-pick-color x)
  `(menu-dynamic
     (dynamic (standard-color-menu (lambda (answer) ,@(cdr x))))))

(extend-table gui-make-table
  (pick-color ,gui-make-pick-color))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Basic pattern picker
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-public (get-preferred-list type nr)
  (with l (get-preference type)
    (when (string? l) (set! l (string->object l)))
    (cond ((nlist? l) (list))
          ((> (length l) nr) (sublist l 0 nr))
          (else l))))

(define-public (insert-preferred-list type what nr)
  (let* ((l (get-preferred-list type nr))
         (i (list-find-index l (cut == <> what)))
         (r l))
    (if i (set! r (append (sublist r 0 i)
                          (sublist r (+ i 1) (length r)))))
    (set! r (cons what r))
    (when (> (length r) nr)
      (set! r (sublist r 0 nr)))
    (when (!= r l)
      (set-preference type r))))

(define-public (tm-pattern name . args)
  (cond ((url-exists? (url-append "$TEXMACS_PATTERN_PATH" (url-tail name)))
         `(pattern ,(url->unix (url-tail name)) ,@args))
        ((string-starts? (url->unix (url->delta-unix name)) "../")
         (when (url? name) (set! name (url->system name)))
         `(pattern ,name ,@args))
        (else
         `(pattern ,(url->unix (url->delta-unix name)) ,@args))))

(tm-menu (my-pattern-menu cmd)
  (tile 8
    (for (col (get-preferred-list "my patterns" 32))
      (with args (cons* (cadr col) "100%" "100@" (cddddr col))
        (with col2 (apply tm-pattern args)
          (explicit-buttons
            ((color col2 #f #f 32 32)
             (cmd col))))))))

(define (standard-pattern-list dir scale)
  (let* ((l1 (url-read-directory dir "*.png"))
         (l2 (url-read-directory dir "*.jpg"))
         (l3 (url-read-directory dir "*.gif"))
         (l (append l1 l2 l3))
         (d (map (cut url-delta (string-append dir "/x") <>) l))
         (f (map (lambda (x) (string-append dir "/" (url->unix x))) d)))
    (map (lambda (x) (tm-pattern x scale "")) f)))

(tm-menu (standard-pattern-menu cmd dir scale)
  (tile 8
    (for (col (standard-pattern-list dir scale))
      (with col2 (tm-pattern (cadr col) "100%" "100@")
        (explicit-buttons
          ((color col2 #f #f 32 32)
           (cmd col)))))))

(tm-menu (big-pattern-menu cmd dir scale)
  (tile 6
    (for (col (standard-pattern-list dir scale))
      (with col2 (tm-pattern (cadr col) "100%" "100@")
        (explicit-buttons
          ((color col2 #f #f 90 90)
           (cmd col)))))))

(define-public (clipart-list)
  (list-filter
   (list (list "Dot hatches" "$TEXMACS_PATH/misc/patterns/dots-hatches")
         (list "Line hatches" "$TEXMACS_PATH/misc/patterns/lines-default")
         (list "Artistic hatches" "$TEXMACS_PATH/misc/patterns/lines-artistic")
         (list "Textile" "$TEXMACS_PATH/misc/patterns/textile")
         (list "Hatch" "/opt/local/share/openclipart/special/patterns")
         (list "Personal" "~/patterns")
         (list "Simple" "~/simple-tiles"))
   (lambda (p) (url-exists? (cadr p)))))

(tm-menu (clipart-pattern-menu cmd scale)
  (for (p (clipart-list))
    (-> (eval (car p))
        (dynamic (big-pattern-menu cmd (cadr p) scale)))))

(define (gui-make-pick-background x)
  `(menu-dynamic
     (dynamic (standard-color-menu (lambda (answer) ,@(cddr x))))
     ---
     (dynamic (standard-pattern-menu (lambda (answer) ,@(cddr x))
                                     "$TEXMACS_PATH/misc/patterns/vintage"
                                     ,(cadr x)))
     (when (nnull? (get-preferred-list "my patterns" 32))
       ---
       (dynamic (my-pattern-menu (lambda (answer) ,@(cddr x)))))
     ;;(assuming (nnull? (clipart-list))
     ;;  ---
     ;;  (dynamic (clipart-pattern-menu (lambda (answer) ,@(cddr x))
     ;;                                 ,(cadr x))))
     ))

(extend-table gui-make-table
  (pick-background ,gui-make-pick-background))

(tm-define (allow-pattern-colors?)
  (or (qt-gui?) (vue-gui?)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Extra RGB color picker
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (rgb-color-name r g b)
  (string-append "#"
    (integer->padded-hexadecimal r 2)
    (integer->padded-hexadecimal g 2)
    (integer->padded-hexadecimal b 2)))

(tm-menu (rgb-palette cmd r1 r2 g1 g2 b1 b2 n)
  (for (rr (.. r1 r2))
    (for (gg (.. g1 g2))
      (for (bb (.. b1 b2))
        (let* ((r (/ (* 255 rr) (- n 1)))
               (g (/ (* 255 gg) (- n 1)))
               (b (/ (* 255 bb) (- n 1)))
               (col (rgb-color-name r g b)))
          (explicit-buttons
            ((color col #f #f 24 24)
             (cmd col))))))))

(tm-menu (rgb-color-picker cmd)
  (tile 18
    (dynamic (rgb-palette cmd 0 6 0 3 0 6 6)))
  (tile 18
    (dynamic (rgb-palette cmd 0 6 3 6 0 6 6)))
  ---
  (glue #f #f 0 3)
  (hlist
    (glue #t #f 0 17)
    (explicit-buttons
      ("Cancel" (cmd #f)))
    (glue #f #f 3 0))
  (glue #f #f 0 3))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Deprecated functionality
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define-macro (menu-extend name . l)
  (deprecated-function "menu-extend" "tm-menu" "former")
  (receive (opts body) (list-break l not-define-option?)
    `(tm-define (,name) ,@opts (menu-dynamic (former) ,@body))))
