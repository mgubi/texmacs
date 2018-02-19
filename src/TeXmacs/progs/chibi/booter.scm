(import (except (chibi) define) (rename (chibi) (define prim-define))
        (scheme list) (scheme hash-table)
        (only (scheme process-context) get-environment-variable)
        (chibi filesystem)
        (srfi 33) (scheme charset) (scheme cxr)
        (only (chibi process) current-process-id)
        (srfi 27) ;; random numbers
)

(define-syntax define
  (syntax-rules ()
    ((define ((x a1 ...) a ...) body ...) (define (x a1 ...) (lambda (a ...) body ...)))
    ((define x body ...) (prim-define x body ...))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; an implementation of define-macro with rsc-macro-transformer

;; from https://stackoverflow.com/questions/15552057/is-it-possible-to-implement-define-macro-in-mit-scheme?rq=1
;; see also https://ds26gte.github.io/tyscheme/index-Z-H-20.html

;; rsc-macro-transformer is not r7rs, imported from the (chibi) module

#;(define-syntax define-macro
  (syntax-rules ()
    ((define-macro (name . args) body ...)
     (define-syntax name
       (rsc-macro-transformer
         (let ((transformer (lambda args body ...)))
           (lambda (exp env)
              (apply transformer (cdr exp)))))))))

(define-syntax define-macro
  (syntax-rules ()
    ((define-macro (name . args) body ...) (define-macro name (lambda args body ...)))
    ((define-macro name def)
      (define-syntax name
       (rsc-macro-transformer
         (let ((transformer def))
           (lambda (exp env)
              (apply transformer (cdr exp)))))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; brings in texmacs module system with selected exports

(define *texmacs-environment* (current-environment))
(define *initial-exports*
    (let p ((e (current-environment)) (l '())) (if e (p (env-parent e) (append (env-exports e) l)) l)))


;;(define *texmacs-env* (current-environment))

(let ((env (make-environment))
       (file (url-concretize "$TEXMACS_PATH/progs/chibi/module-loader.scm")))
  (display "Loading ") (display file) (newline)
  (%import env *texmacs-environment* *initial-exports* #t)
  (%import env *texmacs-environment* '(*texmacs-environment* *texmacs-primitives*) #t)
  (load file env)
;;  (%import #f env '(*tm-modules*) #t)
  (display (eval '*texmacs-module-bindings* env)) (newline)
  (%import *texmacs-environment* env (eval '*texmacs-module-bindings* env) #t)
  (display "Done\n")
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; compatibility layer

(define boot-start (texmacs-time))
(define-public remote-client-list (list))

(define-public (noop . x) (if (pair? x) (car x) #f))
(define-public (plus1 x) (+ x 1))
(define-public (minus1 x) (- x 1))
(define-public (list-head l k) (if (> k 0) (cons (car l) (list-head (cdr l) (- k 1))) '()))
;(define-public map-in-order map)
(define-public scm-error error)
(define-public has-look-and-feel? (lambda (x) (== x "emacs")))

(define-public (getpid) (current-process-id))

(define-public (symbol-append . l) (string->symbol (apply string-append  (map symbol->string l))))
(define-public getenv get-environment-variable)

(define-public (copy-tree t)
  (if (pair? t) (cons (copy-tree (car t))
                      (copy-tree (cdr t)))
                t))

;; stub
(define-public (procedure-source name) #f)

(define-public (assoc-set! alist key value)
  (let lp ((l alist))
    (if (pair? l)
     (if (equal? key (caar l))
           (begin (set-cdr! (car l) value) alist)
           (lp (cdr l)))
     (cons (cons key value) alist))))

#;(define-public (object->string obj)
      (let ((out (open-output-string)))
         (write obj out)
          (get-output-string out)))

(define-public (texmacs-version) (%%texmacs-version%%))
(define-public (display-to-string obj)
  (call-with-output-string
    (lambda (port) (display obj port))))
(define-public (object->string obj)
  (call-with-output-string
    (lambda (port) (write obj port))))

(define-public logand bitwise-and)
(define-public logior bitwise-ior)
(define-public module-provide require-tm-module)

(define-public developer-mode?
    (equal? (cpp-get-preference "developer tool" "off") "on"))

(define-public (acons key value alist) (cons (cons key  value) alist))

(define-public gensym
  (let ((counter 0))
    (lambda ()
      (set! counter (+ 1 counter))
      (string->symbol (string-append "tm-gensym-" (number->string counter))))))



(define-public (quit-TeXmacs-scheme) (noop))

(define-public-macro (on-entry . cmd)
  `(begin ,@cmd))

(define-public-macro (on-exit . cmd)
  `(set! quit-TeXmacs-scheme (lambda () ,@cmd (,quit-TeXmacs-scheme))))



(tm-inherit-module (chibi keywords))


(define-public (keyword->symbol k) (string->symbol (keyword->string k)))
(define-keywords or and not repeat group quote exclude range and-not match replace up down first last next previous)

(define-keywords mode require type synopsis returns note argument arguments default proposals secure check-mark interactive balloon)
(define-keywords all)
(define-keywords pause every idle)

(define-keywords
|%0| |%1| |%2| |%3| |%4| |%5| |*| |->| |1%| |2| |2%| |3| |4| |5| |9| |:|  |a| |a>| |abbr| |abovedot| |abovering|
|acronym| |acute| |align| |all| |always| |and| |and-not| |any| |appcast| |approx| |args| |argument| |arguments|
|atomic| |attach-tracking-info| |attached| |auto| |automatic| |automatic-checks| |b| |b>| |background| |balloon|
|bar| |bbb| |beamer-theme| |beamer-title-theme| |black| |block| |body| |bold| |bold:cal| |bold:greek| |bold:up|
|boolean?| |box-color| |box-shape| |br| |breve| |button-color| |button-shape| |cal| |cedilla| |cell| |cell-halign|
|cell-lsep| |center| |char| |check| |check-mark| |circled| |class| |clean| |cmd| |code| |col| |colgroup| |collapse|
|collect-languages| |collect-timestamp| |column| |commander| |comment| |commit| |completes| |compress| |condensed|
|conservative| |constant| |constant_char| |constant_number| |constant_string| |constant_type| |contains| |contextual|
|cpp:comment| |cpp:constant_number| |cpp:constant_string| |cpp:constant_type| |cpp:error| |cpp:keyword| |cpp:none|
|cpp:preprocessor| |cpp:preprocessor_directive| |cr2?| |created| |css| |current| |cursor| |dd| |dead| |deadhat|
|declaration| |declare_category| |declare_function| |declare_type| |default| |dfn| |dides| |directory| |dist| |dist*|
|distill| |div| |dl| |do| |doc-macro-cache| |doc-scm-cache| |doubleacute| |down| |dt| |elem| |element| |em| |embed|
|empty| |encoding| |end| |environment| |error| |every| |ex| |exclude| |expand| |expand-macros| |expand-user-macros|
|export| |fallback-on-pictures| |file| |fillp| |filter-in| |first| |focus| |fold| |font| |font-shape| |foo| |footnotes|
|format| |frak| |frame| |free| |function| |function-with-options| |gecos| |geometry| |gpg-decrypted-style| |grave|
|greek| |group| |gs| |h1| |h2| |h3| |h4| |h5| |h6| |hN| |handler| |hat| |head| |help-win-viewing| |help-window-geometry|
|help-window-viewing| |help-window-visible| |hidden| |highlight| |hover| |hr| |href| |html| |i| |idle| |images| |img|
|in-row| |in-row-group| |indirect-bib| |inherit| |initialize| |inline| |insert| |integer?| |interactive| |interrupted|
|interval| |invbreve| |italic| |kbd| |keep-folded| |keep-unfolded| |keyword| |keyword_conditional| |keyword_control|
|label| |lasprat| |last| |launch| |leaf| |left| |left-margin| |lhw| |li| |light| |limits| |link| |literal| |loaded|
|loasc| |loc| |local| |lodes| |loh| |low| |ltac| |lvw| |m| |macpath| |mag| |match| |math| |math-correct| |math-input|
|mathml| |menclose| |menu-item| |menu-item-list| |menu-label| |menu-wide-item| |menu-wide-label| |meta| |mfrac| |mi|
|middle| |mixed| |mmultiscripts| |mn| |mo| |mode| |modified| |mono| |move| |mover| |mprescripts| |mroot| |mrow| |msqrt|
|mstyle| |msub| |msubsup| |msup| |mtable| |mtd| |mtext| |mtime| |mtr| |multicol| |munder| |munderover| |must-recognize|
|my-keyword| |name| |nat| |new| |new-window| |next| |none| |not| |not-whitespace| |note| |notes| |now| |ns0| |ns1|
|object| |oblique| |ogonek| |ol| |one| |operator| |operator_field| |operator_openclose| |operator_special| |option| |or|
|order| |ornament-| |ornament-border| |ornament-color| |ornament-extra-color| |ornament-hpadding| |ornament-shadow-color|
|ornament-shape| |ornament-sunny-color| |ornament-swell| |ornament-title-style| |ornament-vpadding| |other| |out-row|
|out-row-group| |out-table| |over| |overwrite| |p| |p.| |p>| |parent| |pasprat| |pause| |pen| |penalty| |permanent|
person| |pre| |preamble| |prefix| |preformatted| |preprocessor| |preprocessor_directive| |pretty| |previous| |print|
|prioritary| |procedure| |profile| |program-theme| |promptrepl| |proposals| |pseudos| |public-dsa-key| |python:comment|
|python:constant| |python:constant_char| |python:constant_number| |python:constant_string| |python:declare_function|
|python:declare_type| |python:error| |python:keyword| |python:keyword_conditional| |python:keyword_control| |python:none|
|python:operator| |python:operator_field| |python:operator_openclose| |python:operator_special| |quote| |range|
|raster-resolution| |raw| |recognize| |recurse| |refresh| |remove-folded| |remove-unfolded| |removed| |repeat| |replace|
|replace-style| |require| |required| |returns| |right| |right-margin| |root| |rotate-backward| |rotate-forward|
|row| |samp| |sansserif| |save-directory| |save-file| |saved| |scale| |scheme:comment| |scheme:constant_char|
|scheme:constant_number| |scheme:constant_string| |scheme:declare_category| |scheme:error| |scheme:keyword|
|scheme:none| |scheme:variable_identifier| |script| |scripts| |secure| |select| |selectable| |serializer|
|session| |session-theme| |setup| |several| |shell| |short-width| |simplify-output| |slant| |small| |smallcaps|
|socket| |source| |source-tracking| |space| |spacing| |span| |start| |state| |state-slots| |stopmark| |strict|
|string| |string?| |strong| |style| |sub| |suffix| |sup| |symbol| |symbol:circled| |symbol:limits| |synopsis|
|syntax| |tab-completion| |table| |table-id| |tabstop| |tbody| |td| |terms| |test-input-done| |texmacs|
|texmacscode| |texmacstyle| |text| |tfoot| |th| |thead| |theorem-decorations| |thin| |tilde| |timings|
|title| |toplvl| |tr| |translatable?| |transparent| |transparent-source-tracking| |transparent-tracking|
|tt| |tuple?| |type| |typewriter| |u| |uhw| |ul| |umlaut| |under| |unfold| |up| |up-to-date| |upc| |update|
|uph| |upw| |use| |use-macros| |users| |uvw| |value| |var| |var-compress| |var-expand| |var-first| |var-last|
|var-next| |var-previous| |variable_identifier| |vcnt| |verbatim:tabstop| |vernac| |version| |versions| |video|
|visible| |while| |white| |whitespace| |wide| |winpath| |wrap| |x| |xh| |y|

)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-inherit-modules (kernel boot compat) (kernel boot abbrevs)
                    (kernel boot debug) (kernel boot srfi)
                    (kernel boot ahash-table) (kernel boot prologue))

(tm-inherit-modules (kernel library base) (kernel library list)
                    (kernel library tree) (kernel library content)
                    (kernel library patch))

(tm-inherit-modules (kernel regexp regexp-match) (kernel regexp regexp-select))

(tm-inherit-modules (kernel logic logic-rules) (kernel logic logic-query)
                    (kernel logic logic-data))

(tm-inherit-modules (kernel texmacs tm-define)
                    (kernel texmacs tm-preferences) (kernel texmacs tm-modes)
                    (kernel texmacs tm-plugins) (kernel texmacs tm-secure)
                    (kernel texmacs tm-convert) (kernel texmacs tm-dialogue)
                    (kernel texmacs tm-language) (kernel texmacs tm-file-system)
                    (kernel texmacs tm-states))

(tm-inherit-modules (kernel gui gui-markup)
                    (kernel gui menu-define) (kernel gui menu-widget)
                    (kernel gui kbd-define) (kernel gui kbd-handlers)
                    (kernel gui menu-test)
                    (kernel old-gui old-gui-widget)
                    (kernel old-gui old-gui-factory)
                    (kernel old-gui old-gui-form)
                    (kernel old-gui old-gui-test))

(display "=================================================\n")
(display "Booting all modules\n")
(display "=================================================\n")

;; (utils cas cas-out)

(tm-import-modules (utils library cpp-wrap) (utils library cursor)  (utils plugins plugin-cmd) (utils library smart-table) (utils plugins plugin-convert) (utils misc markup-funcs) (utils handwriting handwriting) (bibtex bib-utils) (bibtex bib-complete) (bibtex bib-widgets) (texmacs texmacs tm-server) (texmacs texmacs tm-view) (texmacs texmacs tm-files) (texmacs texmacs tm-print) (texmacs keyboard config-kbd) (texmacs keyboard prefix-kbd) (texmacs keyboard latex-kbd) (texmacs menus file-menu) (texmacs menus edit-menu) (texmacs menus view-menu) (texmacs menus tools-menu) (texmacs menus preferences-menu) (texmacs menus preferences-widgets) (texmacs menus main-menu) (texmacs menus view-menu) (generic generic-kbd) (generic generic-menu) (generic format-menu) (generic document-menu) (generic document-part) (generic insert-menu) (generic document-edit) (generic generic-edit) (generic generic-doc) (generic generic-widgets) (generic format-widgets) (generic document-widgets) (text text-kbd) (text text-menu) (math math-kbd) (math math-sem-edit) (math math-menu) (math math-edit) (prog prog-kbd) (prog prog-menu) (source source-kbd) (source source-menu) (source macro-edit) (source macro-widgets) (table table-kbd) (table table-menu) (table table-edit) (table table-widgets) (graphics graphics-kbd) (graphics graphics-menu) (graphics graphics-object) (graphics graphics-utils) (graphics graphics-edit) (graphics graphics-main) (graphics graphics-markup) (language natural) (dynamic fold-kbd) (dynamic scripts-kbd) (dynamic calc-kbd) (dynamic fold-menu) (dynamic session-menu) (dynamic scripts-menu) (dynamic calc-menu) (dynamic animate-menu) (dynamic fold-edit) (dynamic session-edit) (dynamic calc-edit) (doc tmdoc-kbd) (doc apidoc-kbd) (doc tmdoc-menu) (doc help-menu) (doc tmdoc) (doc docgrep) (doc tmdoc-search) (doc tmweb) (doc apidoc) (doc apidoc-menu) (doc docgrep) (doc tmdoc) (doc apidoc) (convert images tmimage) (convert rewrite init-rewrite) (convert html tmhtml-expand) (convert latex latex-drd) (convert latex tmtex) (convert latex latex-tools) (convert latex tmtex-widgets) (part part-shared) (part part-menu) (part part-tmfs) (database db-widget) (database db-menu) (database db-convert) (database bib-db) (database bib-manage) (database bib-local) (database db-menu) (database db-tmfs) (database bib-kbd) (security wallet wallet-menu) (security wallet wallet-base) (security wallet wallet-menu) (security gpg gpg-edit) (security gpg gpg-widgets) (security gpg gpg-base) (security gpg gpg-menu) (security gpg gpg-widgets) (client client-tmfs) (server server-menu) (client client-menu) (client client-tmfs) (link link-menu) (link link-kbd) (link link-edit) (link link-navigate) (link link-extern) (version version-menu) (version version-kbd) (version version-tmfs) (debug debug-menu) (texmacs menus developer-menu) (debug debug-widgets) (fonts fonts-ec) (fonts fonts-adobe) (fonts fonts-x) (fonts fonts-math) (fonts fonts-foreign) (fonts fonts-misc) (fonts fonts-composite) (fonts fonts-truetype) (fonts font-old-menu) (fonts font-new-widgets) (check check-master))

(display "-------------------------------------------------\n")


;(display "Booting utilities\n")
(tm-import-modules (utils library cpp-wrap))
(lazy-define (utils library cursor) notify-cursor-moved)
(lazy-define (utils cas cas-out) cas->stree)
(lazy-define (utils plugins plugin-cmd) pre-serialize verbatim-serialize)
(tm-import-modules (utils library smart-table))
(tm-import-modules (utils plugins plugin-convert))
(tm-import-modules (utils misc markup-funcs))
(tm-import-modules (utils handwriting handwriting))
(define supports-email? (url-exists-in-path? "mmail"))
(if supports-email? (tm-import-modules (utils email email-tmfs)))
;(display* "time: " (- (texmacs-time) boot-start) "\n")
;(display* "memory: " (texmacs-memory) " bytes\n")

;(display "Booting BibTeX style modules\n")
(tm-import-modules (bibtex bib-utils))
(lazy-define (bibtex bib-complete) current-bib-file citekey-completions)
(lazy-menu (bibtex bib-widgets) open-bibliography-inserter)
;(display* "time: " (- (texmacs-time) boot-start) "\n")
;(display* "memory: " (texmacs-memory) " bytes\n")

;(display "Booting main TeXmacs functionality\n")
(tm-import-modules (texmacs texmacs tm-server) (texmacs texmacs tm-view)
             (texmacs texmacs tm-files) (texmacs texmacs tm-print))
(tm-import-modules (texmacs keyboard config-kbd))
(lazy-keyboard (texmacs keyboard prefix-kbd) always?)
(lazy-keyboard (texmacs keyboard latex-kbd) always?)
(lazy-menu (texmacs menus file-menu) file-menu go-menu
           new-file-menu load-menu save-menu
           print-menu print-menu-inline close-menu)
(lazy-menu (texmacs menus edit-menu) edit-menu)
(lazy-menu (texmacs menus view-menu) view-menu texmacs-bottom-toolbars)
(lazy-menu (texmacs menus tools-menu) tools-menu)
(lazy-menu (texmacs menus preferences-menu) preferences-menu page-setup-menu)
(lazy-menu (texmacs menus preferences-widgets) open-preferences)
(tm-import-modules (texmacs menus main-menu))
(lazy-define (texmacs menus view-menu) set-bottom-bar test-bottom-bar?)
(tm-define (notify-set-attachment name key val) (noop))
;(display* "time: " (- (texmacs-time) boot-start) "\n")
;(display* "memory: " (texmacs-memory) " bytes\n")

(display "Booting generic mode\n")
(lazy-keyboard (generic generic-kbd) always?)
(lazy-menu (generic generic-menu) focus-menu texmacs-focus-icons)
(lazy-menu (generic format-menu) format-menu
           font-size-menu color-menu horizontal-space-menu
           transform-menu specific-menu
           text-font-effects-menu text-effects-menu
           vertical-space-menu indentation-menu line-break-menu
           page-header-menu page-footer-menu page-numbering-menu
           page-break-menu)
(lazy-menu (generic document-menu) document-menu
           project-menu document-style-menu global-language-menu)
(lazy-menu (generic document-part)
           preamble-menu document-part-menu project-manage-menu)
(lazy-menu (generic insert-menu) insert-menu texmacs-insert-menu
           texmacs-insert-icons insert-link-menu insert-image-menu)
(lazy-define (generic document-edit) update-document
             get-init-page-rendering init-page-rendering)
(lazy-define (generic generic-edit) notify-activated notify-disactivated)
(lazy-define (generic generic-doc) focus-help)
(lazy-define (generic generic-widgets) search-toolbar replace-toolbar
             open-search toolbar-search-start interactive-search
             open-replace toolbar-replace-start interactive-replace
             search-next-match)
(lazy-define (generic format-widgets) open-paragraph-format open-page-format
             open-pattern-selector)
(lazy-define (generic document-widgets) open-source-tree-preferences
             open-document-paragraph-format open-document-page-format
             open-document-metadata open-document-colors)
(tm-property (open-search) (:interactive #t))
(tm-property (open-replace) (:interactive #t))
(tm-property (open-paragraph-format) (:interactive #t))
(tm-property (open-page-format) (:interactive #t))
(tm-property (open-source-tree-preferences) (:interactive #t))
(tm-property (open-document-paragraph-format) (:interactive #t))
(tm-property (open-document-page-format) (:interactive #t))
(tm-property (open-document-metadata) (:interactive #t))
(tm-property (open-document-colors) (:interactive #t))
(tm-property (open-pattern-selector cmd w) (:interactive #t))
;(display* "time: " (- (texmacs-time) boot-start) "\n")
;(display* "memory: " (texmacs-memory) " bytes\n")

(display "Booting text mode\n")
(lazy-keyboard (text text-kbd) in-text?)
(lazy-menu (text text-menu) text-format-menu text-format-icons
	   text-menu text-block-menu text-inline-menu
           text-icons text-block-icons text-inline-icons)
;(display* "time: " (- (texmacs-time) boot-start) "\n")
;(display* "memory: " (texmacs-memory) " bytes\n")

(display "Booting math mode\n")
(lazy-keyboard (math math-kbd) in-math?)
(lazy-keyboard (math math-sem-edit) in-sem-math?)
(lazy-menu (math math-menu) math-format-menu math-format-icons
	   math-menu math-insert-menu
           math-icons math-insert-icons
           math-correct-menu semantic-math-preferences-menu
           context-preferences-menu insert-math-menu)
(lazy-initialize (math math-menu) (in-math?))
(lazy-define (math math-edit) brackets-refresh)
;(display* "time: " (- (texmacs-time) boot-start) "\n")
;(display* "memory: " (texmacs-memory) " bytes\n")

(display "Booting programming modes\n")
(lazy-keyboard (prog prog-kbd) in-prog?)
(lazy-menu (prog prog-menu) prog-format-menu prog-format-icons
	   prog-menu prog-icons)
;(display* "time: " (- (texmacs-time) boot-start) "\n")
;(display* "memory: " (texmacs-memory) " bytes\n")

(display "Booting source mode\n")
(lazy-keyboard (source source-kbd) always?)
(lazy-menu (source source-menu) source-macros-menu source-menu source-icons
           source-transformational-menu source-executable-menu)
(lazy-define (source macro-edit) has-macro-source? edit-macro-source)
(lazy-define (source macro-widgets) editable-macro? open-macros-editor
	     open-macro-editor create-table-macro)
(tm-property (open-macro-editor l) (:interactive #t))
(tm-property (create-table-macro l) (:interactive #t))
(tm-property (open-macros-editor) (:interactive #t))
;(display* "time: " (- (texmacs-time) boot-start) "\n")
;(display* "memory: " (texmacs-memory) " bytes\n")

(display "Booting table mode\n")
(lazy-keyboard (table table-kbd) in-table?)
(lazy-menu (table table-menu) insert-table-menu)
(lazy-define (table table-edit) table-resize-notify)
(lazy-define (table table-widgets) open-cell-properties open-table-properties)
(tm-property (open-cell-properties) (:interactive #t))
(tm-property (open-table-properties) (:interactive #t))
;(display* "time: " (- (texmacs-time) boot-start) "\n")
;(display* "memory: " (texmacs-memory) " bytes\n")

(display "Booting graphics mode\n")
(lazy-keyboard (graphics graphics-kbd) in-active-graphics?)
(lazy-menu (graphics graphics-menu) graphics-menu graphics-icons)
(lazy-define (graphics graphics-object)
             graphics-reset-state graphics-decorations-update)
(lazy-define (graphics graphics-utils) make-graphics)
(lazy-define (graphics graphics-edit)
             graphics-busy?
             graphics-reset-context graphics-undo-enabled
             graphics-release-left graphics-release-middle
             graphics-release-right graphics-start-drag-left
             graphics-dragging-left graphics-end-drag-left)
(lazy-define (graphics graphics-main) graphics-update-proviso
             graphics-get-proviso graphics-set-proviso)
(lazy-define (graphics graphics-markup) arrow-with-text arrow-with-text*)
(define-secure-symbols arrow-with-text arrow-with-text*)
;(display* "time: " (- (texmacs-time) boot-start) "\n")
;(display* "memory: " (texmacs-memory) " bytes\n")

(display "Booting formal and natural languages\n")
(lazy-language (language minimal) minimal)
(lazy-language (language std-math) std-math)
(lazy-define (language natural) replace)
;(display* "time: " (- (texmacs-time) boot-start) "\n")
;(display* "memory: " (texmacs-memory) " bytes\n")

;(display "Booting dynamic features\n")
(lazy-keyboard (dynamic fold-kbd) always?)
(lazy-keyboard (dynamic scripts-kbd) always?)
(lazy-keyboard (dynamic calc-kbd) always?)
(lazy-menu (dynamic fold-menu) insert-fold-menu dynamic-menu dynamic-icons
           graphics-overlays-menu graphics-screens-menu
           graphics-focus-overlays-menu graphics-focus-overlays-icons)
(lazy-menu (dynamic session-menu) insert-session-menu session-help-icons)
(lazy-menu (dynamic scripts-menu) scripts-eval-menu scripts-plot-menu
           plugin-eval-menu plugin-eval-toggle-menu plugin-plot-menu)
(lazy-menu (dynamic calc-menu) calc-table-menu calc-insert-menu
           calc-icourse-menu)
(lazy-menu (dynamic animate-menu) insert-animation-menu animate-toolbar)
(lazy-define (dynamic fold-edit)
             screens-switch-to dynamic-make-slides overlays-context?)
(lazy-define (dynamic session-edit) scheme-eval)
(lazy-define (dynamic calc-edit) calc-ready? calc-table-renumber)
(lazy-initialize (dynamic session-menu) (in-session?))
;(display* "time: " (- (texmacs-time) boot-start) "\n")
;(display* "memory: " (texmacs-memory) " bytes\n")

(display "Booting documentation\n")
(lazy-keyboard (doc tmdoc-kbd) in-manual?)
(lazy-keyboard (doc apidoc-kbd) developer-mode?)
(lazy-menu (doc tmdoc-menu) tmdoc-menu tmdoc-icons)
(lazy-menu (doc help-menu) help-menu)
(lazy-define (doc tmdoc) tmdoc-expand-help tmdoc-expand-help-manual
             tmdoc-expand-this tmdoc-include)
(lazy-define (doc docgrep) docgrep-in-doc docgrep-in-src docgrep-in-texts)
(lazy-define (doc tmdoc-search) tmdoc-search-style tmdoc-search-tag
             tmdoc-search-parameter tmdoc-search-scheme)
(lazy-define (doc tmweb) tmweb-convert-dir tmweb-update-dir
             tmweb-convert-dir-keep-texmacs tmweb-update-dir-keep-texmacs
             tmweb-interactive-build tmweb-interactive-update)
(lazy-define (doc apidoc) apidoc-all-modules apidoc-all-symbols)
(lazy-menu (doc apidoc-menu) apidoc-menu)
(lazy-tmfs-handler (doc docgrep) grep)
(lazy-tmfs-handler (doc tmdoc) help)
(lazy-tmfs-handler (doc apidoc) apidoc)
(define-secure-symbols tmdoc-include)
;(display* "time: " (- (texmacs-time) boot-start) "\n")
;(display* "memory: " (texmacs-memory) " bytes\n")

(display "Booting converters\n")
(lazy-format (convert rewrite init-rewrite) texmacs scheme cpp verbatim)
(lazy-format (convert tmml init-tmml) tmml)
(lazy-format (convert latex init-latex) latex)
(lazy-format (convert html init-html) html)
(lazy-format (convert bibtex init-bibtex) bibtex)
(lazy-format (convert images init-images)
             postscript pdf xfig xmgrace svg xpm jpeg ppm gif png pnm)
(lazy-define (convert images tmimage)
             export-selection-as-graphics clipboard-copy-image)
(lazy-define (convert rewrite init-rewrite) texmacs->code texmacs->verbatim)
(lazy-define (convert html tmhtml-expand) tmhtml-env-patch)
(lazy-define (convert latex latex-drd) latex-arity latex-type)
(lazy-define (convert latex tmtex) tmtex-env-patch)
(lazy-define (convert latex latex-tools) latex-set-virtual-packages
             latex-has-style? latex-has-package?
             latex-has-texmacs-style? latex-has-texmacs-package?)
(lazy-menu (convert latex tmtex-widgets) tmtex-menu)
;(display* "time: " (- (texmacs-time) boot-start) "\n")
;(display* "memory: " (texmacs-memory) " bytes\n")

;(display "Booting partial document facilities\n")
(lazy-define (part part-shared) buffer-initialize buffer-notify)
(lazy-menu (part part-menu) document-master-menu)
(lazy-tmfs-handler (part part-tmfs) part)
;(display* "time: " (- (texmacs-time) boot-start) "\n")
;(display* "memory: " (texmacs-memory) " bytes\n")

(display "Booting database facilities\n")
(lazy-define (database db-widget) open-db-chooser)
(lazy-define (database db-menu) db-show-toolbar)
(lazy-define (database db-convert) db-url?)
(lazy-define (database bib-db) zealous-bib-import zealous-bib-export)
(lazy-define (database bib-manage)
             bib-import-bibtex bib-compile bib-attach open-bib-chooser)
(lazy-define (database bib-local) open-biblio)
(lazy-menu (database db-menu) db-menu db-toolbar)
(lazy-tmfs-handler (database db-tmfs) db)
(lazy-keyboard (database bib-kbd) in-bib?)
(tm-property (open-biblio) (:interactive #t))
;(display* "time: " (- (texmacs-time) boot-start) "\n")
;(display* "memory: " (texmacs-memory) " bytes\n")

(display "Booting security tools\n")
(lazy-define (security wallet wallet-menu) with-wallet)
(lazy-define (security wallet wallet-base)
	     supports-wallet? wallet-initialized?
	     wallet-on? wallet-off?)
(lazy-menu (security wallet wallet-menu) wallet-preferences-widget)
(lazy-define (security gpg gpg-edit) tree-export-encrypted
	     tm-gpg-dialogue-passphrase-decrypt-buffer)
(lazy-define (security gpg gpg-widgets) open-gpg-key-manager)
(lazy-define (security gpg gpg-base) supports-gpg?)
(lazy-menu (security gpg gpg-menu) gpg-menu document-encryption-menu)
(lazy-menu (security gpg gpg-widgets) gpg-preferences-widget)

;(display* "time: " (- (texmacs-time) boot-start) "\n")
;(display* "memory: " (texmacs-memory) " bytes\n")

(display "Booting remote facilities\n")
(lazy-define (client client-tmfs) remote-home-directory)
(lazy-menu (server server-menu) start-server-menu server-menu)
(lazy-menu (client client-menu) start-client-menu client-menu)
(lazy-tmfs-handler (client client-tmfs) remote-file)
;(display* "time: " (- (texmacs-time) boot-start) "\n")
;(display* "memory: " (texmacs-memory) " bytes\n")

(display "Booting linking facilities\n")
(lazy-menu (link link-menu) link-menu)
(lazy-keyboard (link link-kbd) with-linking-tool?)
(lazy-define (link link-edit) create-unique-id)
(lazy-define (link link-navigate) link-active-upwards link-active-ids
             link-follow-ids)
(lazy-define (link link-extern) get-constellation
             get-link-locations register-link-locations)
;(display* "time: " (- (texmacs-time) boot-start) "\n")
;(display* "memory: " (texmacs-memory) " bytes\n")

;(display "Booting versioning facilities\n")
(lazy-menu (version version-menu) version-menu)
(lazy-keyboard (version version-kbd) with-versioning-tool?)
(lazy-define (version version-tmfs) update-buffer commit-buffer)
;(display* "time: " (- (texmacs-time) boot-start) "\n")
;(display* "memory: " (texmacs-memory) " bytes\n")

;(display "Booting debugging and developer facilities\n")
(lazy-menu (debug debug-menu) debug-menu)
(lazy-menu (texmacs menus developer-menu) developer-menu)
(lazy-define (debug debug-widgets) notify-debug-message
             open-debug-console open-error-messages)
;(display* "time: " (- (texmacs-time) boot-start) "\n")
;(display* "memory: " (texmacs-memory) " bytes\n")

;(display "Booting plugins\n")
;(for-each lazy-plugin-initialize (plugin-list))
;(display* "time: " (- (texmacs-time) boot-start) "\n")
;(display* "memory: " (texmacs-memory) " bytes\n")

(display "Booting fonts\n")
(tm-import-modules (fonts fonts-ec) (fonts fonts-adobe) (fonts fonts-x)
             (fonts fonts-math) (fonts fonts-foreign) (fonts fonts-misc)
             (fonts fonts-composite) (fonts fonts-truetype))
(lazy-define (fonts font-old-menu)
	     text-font-menu math-font-menu prog-font-menu)
(lazy-define (fonts font-new-widgets)
             open-font-selector open-document-font-selector)
(tm-property (open-font-selector) (:interactive #t))
(tm-property (open-document-font-selector) (:interactive #t))
;(display* "time: " (- (texmacs-time) boot-start) "\n")
;(display* "memory: " (texmacs-memory) " bytes\n")

;(display "Booting regression testing\n")
(lazy-define (check check-master) check-all run-checks)
;(display* "time: " (- (texmacs-time) boot-start) "\n")
;(display* "memory: " (texmacs-memory) " bytes\n")

;(display "Booting autoupdater\n")
(when (updater-supported?)
  (tm-import-modules (utils misc updater))
  (delayed (:idle 2000) (updater-initialize)))
;(display* "time: " (- (texmacs-time) boot-start) "\n")
;(display* "memory: " (texmacs-memory) " bytes\n")

;(display "------------------------------------------------------\n")
(delayed (:idle 10000) (autosave-delayed))
(texmacs-banner)
;(display "Initialization done\n")


