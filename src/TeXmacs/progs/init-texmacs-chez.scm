;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : init-texmacs-chez.scm
;; DESCRIPTION : Chez Scheme runtime bootstrap (normal startup only)
;; COPYRIGHT   : (C) 2026
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This software falls under the GNU general public license version 3 or
;; later. It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the
;; file LICENSE in the root directory or
;; <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(when (top-level-bound? '*init-texmacs-chez-loaded*)
  (display "WARNING: init-texmacs-chez.scm loaded more than once!\n"))
(define *init-texmacs-chez-loaded* #t)

;; --- Module Load Tracing & Verification --------------------------------------
(define *traced-so-files* '())
(define *traced-so-table* (make-hashtable string-hash string=?))

(define (has-prefix? prefix s)
  (and (string? s) (string? prefix)
       (let ((plen (string-length prefix))
             (slen (string-length s)))
         (and (>= slen plen)
              (string=? (substring s 0 plen) prefix)))))

(define (has-suffix? suffix s)
  (and (string? s) (string? suffix)
       (let ((suflen (string-length suffix))
             (slen (string-length s)))
         (and (>= slen suflen)
              (string=? (substring s (- slen suflen) slen) suffix)))))

(define (normalize-traced-path path)
  (let* ((boot-env (getenv "TEXMACS_BOOT_PATH"))
         (candidates (list (if boot-env (string-append boot-env "/") "")
                           "build-texmacos/boot/"
                           "boot/")))
    (let loop ((c candidates))
      (cond ((null? c) path)
            ((and (not (string=? (car c) ""))
                  (has-prefix? (car c) path))
             (substring path (string-length (car c)) (string-length path)))
            (else (loop (cdr c)))))))

(define (register-traced-so! path)
  (let ((rel (normalize-traced-path path)))
    (unless (hashtable-ref *traced-so-table* rel #f)
      (hashtable-set! *traced-so-table* rel #t)
      ;; Record in completion order (LOAD-FINISH: dependencies finish before callers)
      (set! *traced-so-files* (cons rel *traced-so-files*))
      (when (getenv "TEXMACS_TRACE_VERBOSE")
        (display (string-append "[TRACE-SO] " rel "\n"))))))

(define orig-primitive-load primitive-load)
(set! primitive-load
  (lambda (path)
    (let ((res (orig-primitive-load path)))
      (when (and (string? path) (has-suffix? ".so" path))
        (register-traced-so! path))
      res)))

;; This script is the default tm_init_file under TEXMACS_ENGINE_CHEZ.  Compile
;; mode uses init-chez-compile.scm and lint mode uses init-linter.scm, so every
;; form below is unconditional runtime initialization.

;; --- Chain to the standard initialization ------------------------------------
;;
;; Route every subsequent load through the engine's guarded loader, so a
;; failing file reports LOAD-ERROR with its condition and backtrace instead of
;; aborting the whole subtree silently.

(define load
  (lambda (f . ignore-env)
    (tm-load-file f)))

;; --- Bootstrap kernel modules directly from compiled .so binaries ------------

(unless (top-level-bound? 'notify-preferences-booted)
  (define-top-level-value 'notify-preferences-booted (lambda () (noop))))


;; Note: In this ahead-of-time compiled architecture, module inheritance and
;; scope relationships are resolved statically during compilation into the .so
;; modules (via internal module imports and topological compilation order).
;; Therefore, runtime `inherit-modules` is not needed; we simply load all
;; prerequisites into the runtime environment via `use-modules`.
(use-modules (kernel boot chez-compat)
             (kernel boot compat) (kernel boot abbrevs)
             (kernel boot debug) (kernel boot srfi)
             (kernel boot ahash-table) (kernel boot prologue))
(use-modules (kernel library base) (kernel library list)
             (kernel library tree) (kernel library content)
             (kernel library patch))
(use-modules (kernel regexp regexp-match) (kernel regexp regexp-select))
(use-modules (kernel logic logic-bind) (kernel logic logic-unify)
             (kernel logic logic-rules) (kernel logic logic-query)
             (kernel logic logic-data))
(use-modules (kernel texmacs tm-define)
             (kernel texmacs tm-preferences) (kernel texmacs tm-modes)
             (kernel texmacs tm-plugins) (kernel texmacs tm-secure)
             (kernel texmacs tm-convert) (kernel texmacs tm-dialogue)
             (kernel texmacs tm-language) (kernel texmacs tm-file-system)
             (kernel texmacs tm-states))
(use-modules (kernel gui gui-markup)
             (kernel gui menu-define) (kernel gui menu-widget)
             (kernel gui kbd-define)
             (kernel gui speech-define)
             (kernel gui kbd-handlers)
             (kernel gui menu-test)
             (kernel old-gui old-gui-widget)
             (kernel old-gui old-gui-factory)
             (kernel old-gui old-gui-form)
             (kernel old-gui old-gui-test))

;; texmacs-user is already bound to the interaction environment above; the
;; kernel bundle only needs its own name for the aggregate module's import.
(unless (top-level-bound? 'texmacs-user-kernel)
  (define-top-level-value 'texmacs-user-kernel (interaction-environment)))


;; --- Runtime overrides for the export-bearing definition macros ---------------
;;
;; boot-chez.scm's define-public / define-public-macro run through
;; with-export-if-active, which emits (export ...) while compile-export-mode?
;; is active.  `export' is only legal inside a module or library, so source
;; loaded at runtime (plugins, -x scripts, the REPL) must see these
;; export-free versions instead.  They are installed after the kernel imports
;; and before any editor registration, as plain top-level syntax.

(define-syntax define-public
  (syntax-rules ()
    ((_ (name . args) body ...)
     (define (name . args) body ...))
    ((_ name val)
     (define name val))))

(define-syntax indirect-export
  (syntax-rules ()
    ((_ id ...) (noop))))

(define-syntax define-public-macro
  (syntax-rules ()
    ((_ (nm . formals) body ...)
     (define-macro (nm . formals) body ...))
    ((_ nm val)
     (define-macro nm val))))

;; --- Register the editor program ---------------------------------------------

(lazy-define (kernel gui menu-convert) make-menu-widget**)

;; Booting utilities
(use-modules (utils library cpp-wrap))
(lazy-define (utils library cursor) notify-cursor-moved)
(lazy-define (utils edit variants) make-inline-tag-list make-wrapped-tag-list)
(lazy-define (utils cas cas-out) cas->stree)
(lazy-define (utils plugins plugin-cmd) pre-serialize verbatim-serialize)
(lazy-define (utils test test-convert) delayed-quit
             build-manual build-ref-suite run-test-suite)
(use-modules (utils library tree))
(use-modules (utils library smart-table))
(use-modules (utils plugins plugin-convert))
(use-modules (utils misc markup-funcs))
(use-modules (utils misc artwork))
(use-modules (utils misc tooltip))
(use-modules (utils handwriting handwriting))
(lazy-tmfs-handler (utils automate auto-tmfs) automate)
(lazy-define (utils automate auto-tmfs) auto-load-help)
(lazy-define (utils misc gui-keyboard) get-keyboard)
(lazy-keyboard (utils automate auto-kbd) in-auto?)
(define supports-email? (url-exists-in-path? "mmail"))
(if supports-email? (use-modules (utils email email-tmfs)))

;; Booting BibTeX style modules
(use-modules (bibtex bib-utils))
(lazy-define (bibtex bib-complete) current-bib-file citekey-completions)
(lazy-menu (bibtex bib-widgets) open-bibliography-inserter)

;; Booting main TeXmacs functionality
(use-modules (texmacs texmacs tm-server) (texmacs texmacs tm-view)
             (texmacs texmacs tm-files) (texmacs texmacs tm-print))
(lazy-define (texmacs texmacs tm-files)
             buffer-missing-style? buffer-set-default-style)
(use-modules (texmacs keyboard config-kbd))
(lazy-keyboard (texmacs keyboard prefix-kbd) always?)
(lazy-keyboard (texmacs keyboard latex-kbd) always?)
(lazy-menu (texmacs menus file-menu)
           file-menu go-menu buffer-go-menu
           new-file-menu load-menu save-menu
           print-menu print-menu-inline close-menu)
(lazy-menu (texmacs menus edit-menu) edit-menu)
(lazy-menu (texmacs menus view-menu) view-menu texmacs-bottom-toolbars)
(lazy-menu (texmacs menus tools-menu) tools-menu)
(lazy-menu (texmacs menus preferences-menu) preferences-menu page-setup-menu)
(lazy-menu (texmacs menus preferences-widgets)
           open-preferences open-plugin-preferences open-plugins-preferences)
(use-modules (texmacs menus main-menu))
(lazy-define (texmacs menus file-menu) recent-file-list recent-directory-list)
(lazy-define (texmacs menus view-menu) set-bottom-bar test-bottom-bar?)
(lazy-tool (texmacs menus preferences-tools) preferences-tool)
(tm-define (notify-set-attachment name key val) (noop))

;; Booting generic mode
(lazy-keyboard (generic generic-kbd) always?)
(lazy-keyboard (generic generic-speech-en) always?)
(lazy-keyboard (generic generic-speech-fr) always?)
(lazy-menu (generic generic-menu) focus-menu texmacs-focus-icons)
(lazy-menu (generic format-menu) format-menu
           font-size-menu color-menu horizontal-space-menu
           transform-menu specific-menu
           text-font-effects-menu text-effects-menu
           vertical-space-menu indentation-menu line-break-menu
           page-header-menu page-footer-menu page-numbering-menu
           page-break-menu)
(lazy-menu (generic document-menu) document-menu
           cite-texmacs-menu cite-texmacs-short-menu
           project-menu document-style-menu global-language-menu)
(lazy-menu (generic document-part)
           preamble-menu document-part-menu project-manage-menu)
(lazy-menu (generic insert-menu) insert-menu texmacs-insert-menu
           texmacs-insert-icons insert-link-menu insert-image-menu)
(lazy-define (generic document-edit) update-document set-document-language
             get-init-page-rendering init-page-rendering)
(lazy-define (generic generic-edit) notify-activated notify-disactivated
             wheel-capture?)
(lazy-define (generic generic-doc) focus-help)
(lazy-define (generic search-widgets) search-toolbar replace-toolbar
             open-search toolbar-search-start interactive-search
             open-replace toolbar-replace-start interactive-replace
             search-next-match)
(lazy-define (generic spell-widgets) spell-toolbar
             open-spell toolbar-spell-start interactive-spell)
(lazy-define (generic format-widgets) open-paragraph-format open-page-format)
(lazy-define (generic pattern-selector) open-pattern-selector
             open-gradient-selector open-background-picture-selector)
(lazy-define (generic document-widgets) open-source-tree-preferences
             open-document-paragraph-format open-document-page-format
             open-document-metadata open-document-colors)
(lazy-tool (generic format-tools)
           format-paragraph-tool format-page-tool
           document-paragraph-tool document-page-tool
           sections-tool subsections-tool)
(lazy-tool (generic document-tools)
           source-tree-preferences-tool
           document-metadata-tool document-colors-tool)
(lazy-tool (generic pattern-tools)
           color-tool pattern-tool gradient-tool picture-tool)
(tm-property (open-search) (:interactive #t))
(tm-property (open-replace) (:interactive #t))
(tm-property (open-paragraph-format) (:interactive #t))
(tm-property (open-page-format) (:interactive #t)
                                (:applicable (not (selection-active?))))
(tm-property (open-source-tree-preferences) (:interactive #t))
(tm-property (open-document-paragraph-format) (:interactive #t))
(tm-property (open-document-page-format) (:interactive #t))
(tm-property (open-document-metadata) (:interactive #t))
(tm-property (open-document-colors) (:interactive #t))
(tm-property (open-pattern-selector cmd w) (:interactive #t))
(tm-property (open-gradient-selector cmd) (:interactive #t))
(tm-property (open-background-picture-selector cmd) (:interactive #t))

;; Booting text mode
(lazy-keyboard (text text-kbd) in-text?)
(lazy-keyboard (text text-speech-en) in-text?)
(lazy-keyboard (text text-speech-fr) in-text?)
(lazy-keyboard (text chinese chinese) in-chinese?)
(lazy-menu (text text-menu) text-format-menu text-format-icons
           text-menu text-block-menu text-inline-menu
           text-icons text-block-icons text-inline-icons)
(lazy-define (text text-drd) tm-register-new-list-tag)

;; Booting math mode
(lazy-keyboard (math math-kbd) in-math?)
(lazy-keyboard (math math-sem-edit) in-sem-math?)
(lazy-keyboard (math math-speech-en) in-math?)
(lazy-keyboard (math math-adjust-en) in-math?)
(lazy-keyboard (math math-speech-fr) in-math?)
(lazy-keyboard (math math-adjust-fr) in-math?)
(lazy-menu (math math-menu) math-format-menu math-format-icons
           math-menu math-insert-menu
           math-icons math-insert-icons
           math-correct-menu semantic-math-preferences-menu
           context-preferences-menu insert-math-menu)
(lazy-initialize (math math-menu) (in-math?))
(lazy-define (math math-edit) brackets-refresh)

;; Booting programming modes
(lazy-format (prog prog-format) scheme)
(lazy-format (code-format) cpp julia scala java json csv)
(lazy-format (mathemagix-format) mathemagix)
(lazy-format (caas-format) caas)
(lazy-format (python-format) python)
(lazy-format (scilab-format) scilab)
(lazy-keyboard (prog prog-kbd) in-prog?)
(lazy-menu (prog prog-menu) prog-format-menu prog-format-icons
           prog-menu prog-icons)

;; Booting source mode
(lazy-keyboard (source source-kbd) always?)
(lazy-menu (source source-menu) source-macros-menu source-menu source-icons
           source-transformational-menu source-executable-menu)
(lazy-define (source macro-edit)
             has-macro-source? edit-macro-source edit-focus-macro-source)
(lazy-menu (source macro-menu) insert-macro-menu)
(lazy-define (source macro-widgets)
             editable-macro? open-macros-editor
             open-macro-editor create-table-macro
             edit-focus-macro edit-previous-macro)
(lazy-define (source shortcut-edit) init-user-shortcuts has-user-shortcut?)
(lazy-define (source shortcut-widgets) open-shortcuts-editor)
(tm-property (open-macro-editor l mode) (:interactive #t))
(tm-property (create-table-macro l mode) (:interactive #t))
(tm-property (open-macros-editor mode) (:interactive #t))
(tm-property (edit-focus-macro) (:interactive #t))
(tm-property (open-shortcuts-editor . opt) (:interactive #t))
(when (url-exists? "$TEXMACS_HOME_PATH/system/shortcuts.scm")
  (delayed (:idle 100) (init-user-shortcuts)))

;; Booting table mode
(lazy-keyboard (table table-kbd) in-table?)
(lazy-menu (table table-menu) insert-table-menu)
(lazy-define (table table-edit) table-resize-notify)
(lazy-define (table table-widgets) open-cell-properties open-table-properties)
(lazy-tool (table table-tools) cell-properties-tool table-properties-tool)
(tm-property (open-cell-properties) (:interactive #t))
(tm-property (open-table-properties) (:interactive #t))

;; Booting graphics mode
(lazy-keyboard (graphics graphics-kbd) in-active-graphics?)
(lazy-define (graphics graphics-kbd) graphics-wheel)
(lazy-menu (graphics graphics-menu) graphics-menu graphics-icons
           graphics-focus-icons)
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

;; Booting formal and natural languages
(lazy-language (language minimal) minimal)
(lazy-language (language std-math) std-math)
(lazy-define (language natural) replace)

;; Booting educational features
(lazy-keyboard (education edu-kbd) in-edu-text?)
(lazy-menu (education edu-menu) edu-insert-menu)

;; Booting dynamic features
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
(lazy-define (dynamic scripts-plot) open-plots-editor)
(lazy-initialize (dynamic session-menu) (in-session?))

;; Booting documentation
(lazy-keyboard (doc tmdoc-kbd) in-manual?)
(lazy-keyboard (doc apidoc-kbd) developer-mode?)
(lazy-menu (doc tmdoc-menu) tmdoc-menu tmdoc-icons)
(lazy-menu (doc help-menu) help-menu)
(lazy-define (doc help-funcs)
  load-help-article load-help-book load-help-buffer url-exists-in-help? help-file-title)
(lazy-define (doc tmdoc) tmdoc-expand-help tmdoc-expand-help-manual
             tmdoc-expand-this tmdoc-include)
(lazy-define (doc docgrep) docgrep-in-doc docgrep-in-src
             docgrep-in-texts docgrep-in-recent)
(lazy-define (doc tmdoc-search) tmdoc-search-style tmdoc-search-tag
             tmdoc-search-parameter tmdoc-search-scheme)
(lazy-define (doc tmweb) youtube-select
             tmweb-convert-dir tmweb-update-dir
             tmweb-convert-dir-keep-texmacs tmweb-update-dir-keep-texmacs
             tmweb-interactive-build tmweb-interactive-update
             open-website-builder)
(lazy-define (doc apidoc) apidoc-all-modules apidoc-all-symbols)
(lazy-menu (doc apidoc-menu) apidoc-menu)
(lazy-tmfs-handler (doc docgrep) grep)
(lazy-tmfs-handler (doc tmdoc) help)
(lazy-tmfs-handler (doc apidoc) apidoc)
(define-secure-symbols tmdoc-include youtube-select)
(tm-property (open-website-builder) (:interactive #t))

;; Booting converters
(lazy-format (convert rewrite init-rewrite) texmacs verbatim)
(lazy-format (convert tmml init-tmml) tmml)
(lazy-format (convert latex init-latex) latex)
(lazy-format (convert html init-html) html)
(lazy-format (convert bibtex init-bibtex) bibtex)
(lazy-format (convert images init-images)
             postscript pdf xfig xmgrace svg xpm jpeg ppm gif png pnm)
(lazy-define (convert images tmimage)
             export-selection-as-graphics clipboard-copy-image)
(lazy-define (convert rewrite init-rewrite) texmacs->code texmacs->verbatim)
(lazy-define (convert html tmhtml) ext-tmhtml-eqnarray*)
(define-secure-symbols ext-tmhtml-eqnarray*)
(lazy-define (convert html tmhtml-expand) tmhtml-env-patch)
(lazy-define (convert latex latex-drd) latex-arity latex-type)
(lazy-define (convert latex tmtex) tmtex-env-patch)
(lazy-define (convert latex latex-tools) latex-set-virtual-packages
             latex-has-style? latex-has-package?
             latex-has-texmacs-style? latex-has-texmacs-package?)
(lazy-menu (convert latex tmtex-widgets) tmtex-menu)

;; Booting partial document facilities
(lazy-define (part part-shared) buffer-initialize buffer-notify)
(lazy-menu (part part-menu) document-master-menu)
(lazy-tmfs-handler (part part-tmfs) part)

;; Booting database facilities
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

;; Booting security tools
;; `with-wallet' is a boot-level macro (boot-chez.scm:377).  It must be bound
;; before AOT compilation, so it is deliberately not redefined here; redefining
;; it shadows the working boot version with a syntax-rules form that fails to
;; expand in runtime-loaded source.
(lazy-define (security wallet wallet-menu) expand-with-wallet)
(lazy-define (security wallet wallet-base)
             supports-wallet? wallet-initialized?
             wallet-on? wallet-off? wallet-get)
(lazy-menu (security wallet wallet-menu) wallet-preferences-widget)
(lazy-define (security gpg gpg-edit) tree-export-encrypted
             tm-gpg-dialogue-passphrase-decrypt-buffer)
(lazy-define (security gpg gpg-widgets) open-gpg-key-manager)
(lazy-define (security gpg gpg-base) supports-gpg?)
(lazy-menu (security gpg gpg-menu) gpg-menu document-encryption-menu)
(lazy-menu (security gpg gpg-widgets) gpg-preferences-widget)

;; Booting remote facilities
(lazy-define (client client-base) client-login-then)
(lazy-define (client client-remote-config) client-public-preferences-then)
(lazy-define (client client-tmfs) remote-home-directory)
(lazy-menu (server server-menu) server-start-menu server-menu)
(lazy-menu (client client-menu) client-start-menu client-menu
           remote-menu remote-icons)
(lazy-tmfs-handler (client client-tmfs) remote-file)
(lazy-define (client client-notifications)
             notifiable-icon notifiable-entry notif-count-label
             client-sync-remote-notifications)
(lazy-define (notification notification-base)
             has-notifications? add-notification notification-count)

;; Booting linking facilities
(lazy-menu (link link-menu) link-menu)
(lazy-keyboard (link link-kbd) with-linking-tool?)
(lazy-define (link link-edit) create-unique-id)
(lazy-define (link link-navigate) link-active-upwards link-active-ids
             link-follow-ids link-mouse-ids)
(lazy-define (link link-extern) get-constellation
             get-link-locations register-link-locations)
(lazy-menu (link ref-menu) ref-menu)
(lazy-define (link ref-edit) preview-reference)
(define-secure-symbols preview-reference)

;; Booting versioning facilities
(lazy-menu (version version-menu) version-menu)
(lazy-keyboard (version version-kbd) with-versioning-tool?)
(lazy-keyboard (tools spell spell-kbd) always?)
(lazy-define (version version-tmfs) update-buffer commit-buffer)

;; Booting debugging and developer facilities
(lazy-menu (debug debug-menu) debug-menu)
(lazy-menu (texmacs menus developer-menu)
           developer-menu custom-keyboard-toolbar)
(lazy-define (debug debug-widgets) notify-debug-message
             open-debug-console open-error-messages)

;; Booting editing modes for various special styles
(lazy-menu (tools poster poster-menu) poster-block-menu)
(lazy-menu (tools theme theme-menu) basic-theme-menu)
(lazy-define (tools theme theme-edit) current-basic-theme)
(lazy-define (tools theme theme-menu) basic-theme-name)
(lazy-define (tools spell spell-edit) spell-user-words continuous-spell-check)
(lazy-define (tools spell spell-lantool) supports-lantool? lantool-check)
(use-modules (tools ai ai-batch))
(lazy-define (tools ai ai-translate) ai-translate* ai-abort-translate)

;; Booting plugins
(for-each lazy-plugin-initialize (plugin-list))

;; Booting fonts
(use-modules (fonts fonts-ec) (fonts fonts-adobe) (fonts fonts-x)
             (fonts fonts-math) (fonts fonts-foreign) (fonts fonts-misc)
             (fonts fonts-composite) (fonts fonts-truetype))
(lazy-define (fonts font-old-menu)
             text-font-menu math-font-menu prog-font-menu)
(lazy-define (fonts font-new-widgets)
             open-font-selector open-document-font-selector
             open-document-other-font-selector)
(tm-property (open-font-selector) (:interactive #t))
(tm-property (open-document-font-selector) (:interactive #t))

;; Booting regression testing
(lazy-define (check check-master) check-all run-checks run-all-tests
             run-integration-tests)

;; Booting autoupdater
(when (updater-supported?)
  (use-modules (utils misc updater))
  (delayed (:idle 2000) (updater-initialize)))

(define (dump-boot-order-manifest! out-path)
  (let* ((boot-chez "kernel/boot/boot-chez.so")
         (ordered-list (cons boot-chez (reverse *traced-so-files*))))
    (display (string-append "\n[TRACE] Writing boot loading order manifest ("
                            (number->string (length ordered-list))
                            " modules) to " out-path "...\n"))
    (call-with-output-file out-path
      (lambda (port)
        (display ";;; Auto-generated sequential boot loading order (DFS post-order)\n" port)
        (write ordered-list port)
        (newline port))
      'replace)
    (display "[TRACE] Boot order manifest written successfully.\n")))

(when (or (getenv "TEXMACS_TRACE_MODULES")
          (getenv "TEXMACS_DUMP_BOOT_ORDER"))
  (let ((dump-file (or (getenv "TEXMACS_DUMP_BOOT_ORDER")
                       (let ((b (getenv "TEXMACS_BOOT_PATH")))
                         (if b (string-append b "/boot-order.scm") "build-texmacos/boot/boot-order.scm")))))
    (dump-boot-order-manifest! dump-file)))

(delayed (:idle 10000) (autosave-delayed))
(texmacs-banner)

