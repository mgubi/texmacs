
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : menu-serial.scm
;; DESCRIPTION : the interface as data, for the page of Tau
;; COPYRIGHT   : (C) 2026  Massimiliano Gubinelli
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Tau has no widgets (docs/tau-design.md): menus, icon bars and dialogs are
;; described to the page as data, in the vocabulary of the menu markup
;; (menu-define.scm) with its dynamic parts evaluated and its attributes
;; resolved. This file walks the markup as menu-widget.scm does, and makes
;; nodes where that file makes widgets.
;;
;; A node is an association list, with its kind under 'kind; a list of
;; nodes is a vector. They are written as JSON.
;;
;; The actions of the entries and the contents of the submenus are closures.
;; They are kept in a table, under numbers which go to the page; the page
;; sends a number back to invoke an action or to ask for the contents of a
;; submenu. The numbers belong to a part of the interface (the menu bar, an
;; icon bar...): they are forgotten when the part is described again.

(texmacs-module (kernel gui menu-serial)
  (:use (kernel gui menu-widget)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; JSON
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (json-char c)
  (with n (char->integer c)
    (cond ((== c #\") "\\\"")
          ((== c #\\) "\\\\")
          ((== n 10) "\\n")
          ((== n 13) "\\r")
          ((== n 9) "\\t")
          ((< n 32) "")
          (else (string c)))))

(define (json-string s)
  (string-append "\"" (apply string-append (map json-char (string->list s)))
                 "\""))

(define (json-join l)
  (if (null? l) ""
      (apply string-append
             (cons (car l)
                   (append-map (lambda (x) (list "," x)) (cdr l))))))

(define (json x)
  (cond ((string? x) (json-string x))
        ((== x #t) "true")
        ((== x #f) "false")
        ((number? x) (number->string x))
        ((symbol? x) (json-string (symbol->string x)))
        ((vector? x)
         (string-append "[" (json-join (map json (vector->list x))) "]"))
        ((list? x)
         (string-append
          "{"
          (json-join (map (lambda (p)
                            (string-append
                             (json-string (symbol->string (car p)))
                             ":" (json (cdr p))))
                          x))
          "}"))
        (else "null")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The actions and the contents which are kept for the page
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define serial-table (make-ahash-table)) ;; number -> (part . closure)
(define serial-parts (make-ahash-table)) ;; part -> list of numbers
(define serial-next 1)
(define serial-part "none")              ;; the part which is being described

(define (serial-keep proc)
  "Keep the closure @proc for the current part, return its number."
  (with n serial-next
    (set! serial-next (+ serial-next 1))
    (ahash-set! serial-table n (cons serial-part proc))
    (ahash-set! serial-parts serial-part
                (cons n (or (ahash-ref serial-parts serial-part) '())))
    n))

(define serial-children (make-ahash-table)) ;; part -> list of parts
(define serial-refresh (make-ahash-table))  ;; number -> (kind . part)

(define (serial-forget part)
  "Forget the closures of @part, and those of the parts inside it."
  (for (sub (or (ahash-ref serial-children part) '()))
    (serial-forget sub))
  (ahash-remove! serial-children part)
  (for (n (or (ahash-ref serial-parts part) '()))
    (ahash-remove! serial-table n)
    (ahash-remove! serial-refresh n))
  (ahash-remove! serial-parts part))

(define (serial-in-part part thunk)
  "The value of @thunk, whose closures are kept for @part."
  (with old serial-part
    (set! serial-part part)
    (with r (thunk)
      (set! serial-part old)
      r)))

(define (from-page x)
  "The value @x of the page (strings in UTF-8) for TeXmacs."
  (cond ((string? x) (utf8->cork x))
        ((pair? x) (map from-page x))
        (else x)))

(define (serial-keep-answer cmd)
  "Keep the command @cmd, which the page calls with the value of an input."
  (serial-keep (lambda args (apply cmd (map from-page args)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; What the core says by itself
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; The messages which Scheme sends (a dialog which opens or closes, a part
;; which is refreshed) wait here; they leave at the end of the turn of the
;; core (tau_turn), in their order.

(define outbox '())

(define-public (tau-post kind part props)
  (set! outbox (cons `((t . ,kind) (part . ,part) ,@props) outbox)))

(tm-define (tau-outbox)
  (:synopsis "The waiting messages for the page as JSON, or the empty string")
  (if (null? outbox) ""
      (with l (reverse outbox)
        (set! outbox '())
        (json `((msgs . ,(list->vector l)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Labels
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (serial-text s)
  "The string @s of TeXmacs for the page."
  (cork->utf8 s))

(define (translatable? s)
  (or (string? s) (func? s 'concat) (func? s 'verbatim) (func? s 'replace)))

(define (active? style)
  (== (logand style widget-style-inert) 0))

(define (icon-file name)
  "The file of the icon @name, or the empty string. As the core finds it
   (mupdf_load_xpm): the markup says .xpm, and the icon is the SVG file of
   that name in the theme (light) of the icon sets along the path of the
   pixmaps, or else next to where the name is found; then PNG files."
  (let* ((base (if (string-ends? name ".xpm")
                   (substring name 0 (- (string-length name) 4))
                   name))
         (path (unix->url "$TEXMACS_PIXMAP_PATH"))
         (try (lambda (dir suffix)
                (with u (url-resolve
                         (url-append dir (unix->url (string-append base suffix)))
                         "r")
                  (and (not (url-none? u)) (url-concretize u))))))
    (or (try (url-append path (unix->url "light")) ".svg")
        (try path ".svg")
        (try path "_x2.png")
        (try path ".png")
        "")))

(define icon-files (make-ahash-table))

(define (icon-file* name)
  (or (ahash-ref icon-files name)
      (with f (icon-file name)
        (ahash-set! icon-files name f)
        f)))

(define (serial-label p style)
  "The properties of the label @p."
  (cond ((translatable? p)
         `((label . ,(serial-text (translate p)))))
        ((tuple? p 'balloon 2)
         (append (serial-label (cadr p) style)
                 (if (translatable? (caddr p))
                     `((help . ,(serial-text (translate (caddr p)))))
                     '())))
        ((tuple? p 'extend)
         (serial-label (cadr p) style))
        ((tuple? p 'style 2)
         (serial-label (caddr p) style))
        ((tuple? p 'text 2)
         `((label . ,(serial-text (caddr p))) (symbol . #t)))
        ((tuple? p 'icon 1)
         `((icon . ,(cadr p)) (file . ,(icon-file* (cadr p)))))
        ((tuple? p 'color 5)
         `((color . ,(if (string? (second p)) (second p) ""))))
        (else '())))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Entries
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (find-shortcut what)
  "The shortcut of the command @what, as it is shown."
  (with r (kbd-find-inv-binding what)
    (cond ((nstring? r) "")
          ((== r "") "")
          (else (serial-text (translate (kbd-system-rewrite r)))))))

(define (entry-shortcut label action opt-key)
  (cond (opt-key (serial-text (translate (kbd-system-rewrite opt-key))))
        ((pair? label) "")
        (else (with source (promise-source action)
                (if source (find-shortcut source) "")))))

(define (entry-check-sub result propose)
  (cond ((string? result) result)
        (result propose)
        (else "")))

(define (entry-check opt-check action)
  (if opt-check
      (entry-check-sub ((cadr opt-check)) (car opt-check))
      (with source (promise-source action)
        (cond ((not (and source (pair? source))) "")
              (else (with prop (property (car source) :check-mark)
                      (entry-check-sub
                       (and prop (apply (cadr prop) (cdr source)))
                       (and prop (car prop)))))))))

(define (label-add-dots l)
  (cond ((string? l) (string-append l "..."))
        ((and (pair? l) (in? (car l) '(concat verbatim)))
         `(,@(cDr l) ,(label-add-dots (cAr l))))
        (else l)))

(define (entry-dots label action)
  (with source (promise-source action)
    (if (and source (pair? source) (property (car source) :interactive))
        (label-add-dots label)
        label)))

(define (entry-enabled? style action)
  (and (active? style)
       (with source (promise-source action)
         (or (not (pair? source))
             (with prop (property (car source) :applicable)
               (or (not prop) (apply (car prop) (list))))))))

(define (entry-help action)
  (and-with source (promise-source action)
    (and (pair? source)
         (or (and-with prop (property (car source) :balloon)
               (with txt (apply (car prop) (cdr source))
                 (and (string? txt) txt)))
             (and-with prop (property (car source) :synopsis)
               (and (pair? prop) (string? (car prop))
                    (not (string-occurs? "@" (car prop)))
                    (translate (car prop))))))))

(define (entry-attrs label action opt-key opt-check)
  (cond ((match? label '(check :%1 :string? :%1))
         (entry-attrs (cadr label) action opt-key (cddr label)))
        ((match? label '(shortcut :%1 :string?))
         (entry-attrs (cadr label) action (caddr label) opt-check))
        (else (values label action opt-key opt-check))))

(define (serial-entry p style)
  "The node of the entry @p: its label and its action."
  (receive (label action opt-key opt-check)
      (entry-attrs (car p) (cAr p) #f #f)
    (let* ((enabled? (entry-enabled? style action))
           (props (serial-label (entry-dots label action) style))
           (help (or (assoc-ref props 'help)
                     (and-with h (entry-help action) (serial-text h)))))
      `((kind . entry)
        ,@(assoc-remove! (list-copy props) 'help)
        (shortcut . ,(entry-shortcut label action opt-key))
        (check . ,(entry-check opt-check action))
        (enabled . ,enabled?)
        ,@(if (!= (logand style widget-style-button) 0) '((button . #t)) '())
        ,@(if help `((help . ,help)) '())
        (action . ,(serial-keep action))))))

(define (serial-symbol p style)
  "The node of the symbol button @p."
  (with (tag symstring . opt) p
    (let* ((opt-cmd (and (nnull? opt) (procedure? (car opt)) (car opt)))
           (source (and opt-cmd (promise-source opt-cmd)))
           (sh (find-shortcut (if source source symstring))))
      `((kind . entry)
        (label . ,(serial-text symstring))
        (symbol . #t)
        (shortcut . ,sh)
        (check . "")
        (enabled . ,(active? style))
        (help . ,(if (== sh "") symstring
                     (string-append symstring ",  keyboard equivalent: " sh)))
        (action . ,(serial-keep (or opt-cmd
                                    (lambda () (insert symstring)))))))))

(define (serial-submenu p style)
  "The node of the submenu @p: its label, and a number for its contents."
  (with (tag label . items) p
    `((kind . submenu)
      (pull . ,(if (== tag '=>) "down" "right"))
      ,@(serial-label label style)
      (enabled . ,(active? style))
      (contents . ,(serial-keep
                    (lambda () (serial-items-list items style #f)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Items
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (new-style style x)
  (if (> x 0) (logior style x) (logand style (lognot (- x)))))

(define (serial-container kind items style bar? . props)
  (list `((kind . ,kind)
          ,@props
          (items . ,(list->vector (serial-items-list items style bar?))))))

(define (serial-nodes items style)
  (list->vector (serial-items-list items style #f)))

(define (serial-input p style)
  "The node of the text field @p."
  (with (tag cmd type props width) p
    (with l (list-filter (props) string?)
      `((kind . input) (type . ,type)
        (value . ,(if (null? l) "" (serial-text (car l))))
        (proposals . ,(list->vector (map serial-text l)))
        (width . ,width)
        (enabled . ,(active? style))
        (answer . ,(serial-keep-answer cmd))))))

(define (serial-enum cmd vals val width style)
  "The properties of a choice in a list which unfolds, as make-enum."
  (let* ((translate* (if (!= (logand style widget-style-verb) 0)
                         identity translate))
         (xval (val))
         (xvals (vals))
         (edit? (and (nnull? xvals) (== (cAr xvals) "")))
         (nvals (if edit? `(,@(cDr xvals) ,xval) `(,@xvals ,xval)))
         (xvals* (list-remove-duplicates nvals))
         (dec (map (lambda (v) (cons (translate* v) v)) xvals*))
         (cmd* (lambda (r) (cmd (or (assoc-ref dec r) r)))))
    `((value . ,(serial-text (translate* xval)))
      (values . ,(list->vector (map serial-text (map car dec))))
      (editable . ,edit?)
      (width . ,width)
      (enabled . ,(active? style))
      (answer . ,(serial-keep-answer cmd*)))))

(define (serial-setting x)
  (with s (if (procedure? x) (x) x)
    (if (translatable? s) (serial-text (translate s)) "")))

(define (serial-choice p style many?)
  "The node of the list @p, of which one or several are chosen."
  (with (tag cmd vals val) p
    (with v (val)
      `((kind . choice) (multiple . ,many?)
        (values . ,(list->vector (map serial-text (vals))))
        (chosen . ,(list->vector
                    (map serial-text
                         (cond ((string? v) (list v))
                               ((list? v) (list-filter v string?))
                               (else '())))))
        (enabled . ,(active? style))
        (answer . ,(serial-keep-answer cmd))))))

(define (serial-tabs p style icons?)
  "The node of the tabs @p: for each a label and a page."
  (with tab
      (lambda (x)
        (let* ((key (if icons? (caddr x) (cadr x)))
               (items (if icons? (cdddr x) (cddr x))))
          `(,@(if icons?
                  `((icon . ,(cadr x)) (file . ,(icon-file* (cadr x))))
                  '())
            (label . ,(serial-nodes (list key) style))
            (items . ,(serial-nodes items style)))))
    (list `((kind . tabs) (tabs . ,(list->vector (map tab (cdr p))))))))

(define (serial-size x)
  "The size wished among @x, as make-resize."
  (cond ((string? x) x)
        ((and (list? x) (>= (length x) 2) (string? (cadr x))) (cadr x))
        (else "")))

(define (serial-refreshable kind make)
  "A node whose contents, the nodes of @make, are sent again by tau-refresh."
  (let* ((n (serial-keep make))
         (sub (string-append serial-part "/" (number->string n))))
    (ahash-set! serial-refresh n (cons kind sub))
    (ahash-set! serial-children serial-part
                (cons sub (or (ahash-ref serial-children serial-part) '())))
    (list `((kind . refreshable) (name . ,kind) (number . ,n)
            (items . ,(list->vector (serial-in-part sub make)))))))

(define (serial-dialog-item p style bar?)
  "The nodes of the item @p of a dialog: an input, tabs, a layout."
  (with tag (car p)
    (cond ((== tag 'input) (list (serial-input p style)))
          ((== tag 'enum)
           (with (tag cmd vals val width) p
             (list `((kind . enum)
                     ,@(serial-enum cmd vals val width style)))))
          ((== tag 'setting-enum)
           (with (tag cmd setting vals val width) p
             (list `((kind . enum) (label . ,(serial-setting setting))
                     ,@(serial-enum cmd vals val width style)))))
          ((== tag 'choice) (list (serial-choice p style #f)))
          ((== tag 'choices) (list (serial-choice p style #t)))
          ((== tag 'filtered-choice)
           (with (tag cmd vals val filter) p
             (list `(,@(serial-choice (list tag cmd vals val) style #f)
                     (filter . ,(serial-text (filter)))))))
          ((== tag 'toggle)
           (with (tag cmd on) p
             (list `((kind . toggle) (on . ,(if (on) #t #f))
                     (enabled . ,(active? style))
                     (answer . ,(serial-keep-answer cmd))))))
          ((== tag 'setting-toggle)
           (with (tag cmd setting on) p
             (list `((kind . toggle) (label . ,(serial-setting setting))
                     (on . ,(if (on) #t #f))
                     (enabled . ,(active? style))
                     (answer . ,(serial-keep-answer cmd))))))
          ((== tag 'setting-group)
           (serial-container 'box (cddr p) style #f
                             `(label . ,(serial-setting (cadr p)))))
          ((== tag 'aligned)
           (list `((kind . aligned)
                   (rows . ,(list->vector
                             (map (lambda (x)
                                    `((left . ,(serial-nodes (list (cadr x))
                                                             style))
                                      (right . ,(serial-nodes (list (caddr x))
                                                              style))))
                                  (cdr p)))))))
          ((in? tag '(tabs responsive-tabs)) (serial-tabs p style #f))
          ((in? tag '(icon-tabs responsive-icon-tabs))
           (serial-tabs p style #t))
          ((in? tag '(scrollable hsplit vsplit))
           (serial-container tag (cdr p) style #f))
          ((in? tag '(division class))
           (with name (cadr p)
             (serial-container tag (cddr p) style #f
                               `(name . ,(with s (if (procedure? name)
                                                     (name) name)
                                           (if (string? s) s ""))))))
          ((== tag 'resize)
           (serial-container tag (cdddr p) style #f
                             `(width . ,(serial-size ((cadr p))))
                             `(height . ,(serial-size ((caddr p))))))
          ((== tag 'refresh)
           (with (tag s kind) p
             (with name (if (string? s) (string->symbol s) s)
               (serial-refreshable
                kind
                (lambda () (serial-items (list 'link name) style bar?))))))
          ;; a view in a dialog, a tree, a colour: later
          (else (serial-other p)))))

(define (serial-other p)
  "A node for what the page does not show yet."
  (list `((kind . ,(car p)) (unsupported . #t))))

(define (serial-tagged p style bar?)
  "The nodes of the item @p, whose head is a symbol."
  (with tag (car p)
    (cond ((in? tag '(-> =>)) (list (serial-submenu p style)))
          ((== tag 'symbol) (list (serial-symbol p style)))
          ((== tag 'group)
           (list `((kind . group) ,@(serial-label (cadr p) style))))
          ((== tag 'text)
           (list `((kind . text) ,@(serial-label (cadr p) style))))
          ((== tag 'glue)
           (list `((kind . glue) (hext . ,(second p)) (vext . ,(third p))
                   (width . ,(fourth p)) (height . ,(fifth p)))))
          ((== tag 'color)
           (list `((kind . color))))
          ((== tag 'invisible) (list))
          ;; containers
          ((in? tag '(horizontal vertical hlist vlist))
           (serial-container tag (cdr p) style
                             (if (in? tag '(horizontal hlist)) #t #f)))
          ((== tag 'minibar)
           (serial-container tag (cdr p)
                             (logior style widget-style-mini) #t))
          ((== tag 'tile)
           (serial-container tag (cddr p) style #f `(columns . ,(cadr p))))
          ((== tag 'extend)
           (serial-items-list (list (cadr p)) style bar?))
          ((== tag 'style)
           (serial-items-list (cddr p) (new-style style (cadr p)) bar?))
          ;; what computes
          ((== tag 'if)
           (if ((cadr p)) (serial-items-list (cddr p) style bar?) '()))
          ((== tag 'when)
           (let* ((ok? (and (active? style) ((cadr p))))
                  (st (logior style
                              (if ok? 0 (+ widget-style-inert
                                           widget-style-grey)))))
             (serial-items-list (cddr p) st bar?)))
          ((== tag 'for)
           (with (tag gen-func vals-promise) p
             (serial-items-list (append-map gen-func (vals-promise))
                                style bar?)))
          ((== tag 'mini)
           (let* ((maxi (logand style (lognot widget-style-mini)))
                  (mini (logior maxi widget-style-mini)))
             (serial-items-list (cddr p) (if ((cadr p)) mini maxi) bar?)))
          ((== tag 'link)
           (with linked ((eval (cadr p)))
             (if linked (serial-items linked style bar?) '())))
          ((== tag 'dynamic)
           (with dyn (eval (cadr p))
             (if dyn (serial-items dyn style bar?) '())))
          ((== tag 'promise)
           (with value ((cadr p))
             (if (match? value ':menu-item)
                 (serial-items value style bar?)
                 '())))
          ((== tag 'refreshable)
           (serial-refreshable
            ((cadr p))
            (lambda () (serial-items-list (cddr p) style bar?))))
          ((== tag 'cached)
           (serial-items-list (cdddr p) style bar?))
          ((in? tag '(input enum choice choices filtered-choice toggle
                      color-input tree-view setting-enum setting-toggle
                      setting-group texmacs-input texmacs-output
                      aligned tabs icon-tabs responsive-tabs
                      responsive-icon-tabs scrollable resize hsplit vsplit
                      refresh ink division class))
           (serial-dialog-item p style bar?))
          (else (serial-items-list p style bar?)))))

(define (serial-items-list l style bar?)
  (append-map (lambda (p) (serial-items p style bar?)) l))

(define (serial-items p style bar?)
  "The nodes of the menu items @p, in a bar if @bar?, with a given @style."
  (if (pair? p)
      (cond ((translatable? (car p))
             (list (serial-entry p style)))
            ((symbol? (car p))
             (serial-tagged p style bar?))
            ((match? (car p) ':menu-wide-label)
             (list (serial-entry p style)))
            (else (serial-items-list p style bar?)))
      (cond ((== p '---) (list `((kind . separator))))
            ((== p '|) (list `((kind . separator) (vertical . #t))))
            (else '()))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; What the core calls
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (tau-serialize-part part menu)
  (:synopsis "The menu @menu as JSON, for the part @part of the interface")
  (serial-forget part)
  (set! serial-part part)
  (json `((items . ,(list->vector (serial-items menu 0 #t))))))

(tm-define (tau-expand n)
  (:synopsis "The contents kept under the number @n, as JSON")
  (with x (ahash-ref serial-table n)
    (if (not x) "{\"items\":[]}"
        (begin
          (set! serial-part (car x))
          (json `((items . ,(list->vector ((cdr x))))))))))

(tm-define (tau-invoke n)
  (:synopsis "Run the action kept under the number @n")
  (and-with x (ahash-ref serial-table n)
    (with action (cdr x)
      (exec-delayed (lambda () (protected-call action))))))

(tm-define (tau-answer n . args)
  (:synopsis "Give the value @args of the input kept under the number @n")
  (and-with x (ahash-ref serial-table n)
    (with cmd (cdr x)
      (exec-delayed
        (lambda () (protected-call (lambda () (apply cmd args))))))))

(tm-define (tau-refresh kind)
  (:synopsis "Describe again the parts of the interface of the kind @kind")
  (for (x (ahash-table->list serial-refresh))
    (let* ((n (car x)) (k (cadr x)) (sub (cddr x)))
      ;; (a part inside another one which was just described is forgotten)
      (when (and (== k kind) (ahash-ref serial-refresh n))
        (with make (cdr (ahash-ref serial-table n))
          (serial-forget sub)
          (tau-post "refresh" sub
                    `((number . ,n)
                      (items . ,(list->vector
                                 (serial-in-part sub make))))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Dialogs
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; A dialog is a part of the interface as a bar is, which the page shows in
;; a window of its own. It has a number; the page says when the user closes
;; it (tau-dialog-closed), and takes it away when the core says so.

(define dialog-next 1)
(define dialog-quits (make-ahash-table)) ;; number -> what is done on closing

(define (dialog-part id)
  (string-append "dialog-" (number->string id)))

(tm-define (tau-dialog-new)
  (:synopsis "A number for a new dialog")
  (with id dialog-next
    (set! dialog-next (+ id 1))
    id))

(tm-define (tau-dialog-show id menu name quit . props)
  (:synopsis "Show the dialog @id with the contents @menu and the title @name")
  (with part (dialog-part id)
    (serial-forget part)
    (ahash-set! dialog-quits id quit)
    (tau-post "dialog" part
              `((id . ,id)
                (title . ,(serial-text (translate name)))
                ,@props
                (items . ,(list->vector
                           (serial-in-part
                            part (lambda () (serial-items menu 0 #f)))))))))

(tm-define (tau-dialog-close id)
  (:synopsis "Take the dialog @id away")
  (when (ahash-ref dialog-quits id)
    (ahash-remove! dialog-quits id)
    (serial-forget (dialog-part id))
    (tau-post "close" (dialog-part id) `((id . ,id)))))

(tm-define (tau-dialog-closed id)
  (:synopsis "The user closed the dialog @id")
  (and-with quit (ahash-ref dialog-quits id)
    (exec-delayed (lambda () (protected-call quit)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Questions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (interactive-call fun vals types)
  (learn-interactive
   fun (map (lambda (i v t) (cons (number->string i) (if (== t "password") "" v)))
            (.. 0 (length vals)) vals types))
  (apply fun vals))

(tm-define (tau-interactive fun args)
  (:synopsis "Ask the arguments @args of @fun in a dialog and call it")
  (if (null? args) (fun)
      (let* ((id (tau-dialog-new))
             (close (lambda () (tau-dialog-close id)))
             (types (map cadr args))
             (vals (list->vector
                    (map (lambda (a) (if (pair? (cddr a)) (caddr a) "")) args)))
             (ok (lambda ()
                   (close)
                   (interactive-call fun (vector->list vals) types)))
             (row (lambda (a i)
                    `(item (text ,(car a))
                           (input ,(lambda (s)
                                     (when (string? s) (vector-set! vals i s)))
                                  ,(cadr a) ,(lambda () (cddr a)) "24em"))))
             (question? (and (list-1? args) (== (car types) "question")))
             (answer (lambda (v)
                       (list v (lambda ()
                                 (close)
                                 (interactive-call fun (list v) types)))))
             (buttons (if question?
                          (map answer (cddar args))
                          `(("Cancel" ,close) ("Ok" ,ok))))
             (body (if question?
                       `(text ,(caar args))
                       `(aligned ,@(map row args (.. 0 (length args))))))
             (menu `(vertical
                      (glue #f #f 0 10)
                      (hlist (glue #f #f 16 0) ,body (glue #f #f 16 0))
                      (glue #f #f 0 12)
                      (hlist (glue #t #f 16 0)
                             (style ,widget-style-button ,@buttons)
                             (glue #f #f 16 0))
                      (glue #f #f 0 10))))
        (tau-dialog-show id menu
                         (if (string-ends? (caar args) "?")
                             "Question" "Enter data")
                         close
                         `(submit . ,(not question?))))))
