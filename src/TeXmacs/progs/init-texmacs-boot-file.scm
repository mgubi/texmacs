;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : init-texmacs-boot-file.scm
;; DESCRIPTION : TeXmacs Chez Scheme boot-file / AOT bootstrap
;;               (curated startup heap + lazy-loading of remaining modules)
;; COPYRIGHT   : (C) 2026
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(when (top-level-bound? '*init-texmacs-boot-file-loaded*)
  (display "WARNING: init-texmacs-boot-file.scm loaded more than once!\n"))
(define *init-texmacs-boot-file-loaded* #t)

(define load (lambda (f . ignore-env) (tm-load-file f)))

(unless (top-level-bound? 'notify-preferences-booted)
  (define-top-level-value 'notify-preferences-booted (lambda () (noop))))

;; 1. Import all 177 resident application modules (matches *startup-boot-modules* minus boot-chez.so)
;; into the interaction environment and mark them loaded in *texmacs-modules-loaded*.
;; The 178 startup modules are already loaded directly into the static generation via
;; texmacs.boot during Sbuild_heap(); remaining modules lazy-load on demand.
(define-macro (import-boot-resident-modules . specs)
  `(begin
     (eval-when (compile load eval)
       (import ,@(map module-spec->symbol specs)))
     (for-each
       (lambda (spec)
         (hashtable-set! *texmacs-modules-loaded* spec #t))
       ',specs)))

(import-boot-resident-modules
  (kernel boot chez-compat) (kernel boot compat)
  (kernel boot abbrevs) (kernel boot debug)
  (kernel boot srfi) (kernel boot ahash-table)
  (kernel boot prologue)
  (kernel library base) (kernel library list)
  (kernel library tree) (kernel library content)
  (kernel library patch)
  (kernel regexp regexp-match) (kernel regexp regexp-select)
  (kernel logic logic-bind) (kernel logic logic-unify)
  (kernel logic logic-rules) (kernel logic logic-query)
  (kernel logic logic-data)
  (kernel texmacs tm-define) (kernel texmacs tm-preferences)
  (kernel texmacs tm-modes) (kernel texmacs tm-dialogue)
  (kernel texmacs tm-plugins) (kernel texmacs tm-secure)
  (kernel texmacs tm-convert) (kernel texmacs tm-language)
  (kernel texmacs tm-file-system) (kernel texmacs tm-states)
  (kernel gui gui-markup) (kernel gui menu-define)
  (kernel gui kbd-define) (kernel gui menu-widget)
  (kernel gui speech-define) (kernel gui kbd-handlers)
  (kernel gui menu-test)
  (kernel old-gui old-gui-widget) (kernel old-gui old-gui-factory)
  (kernel old-gui old-gui-form) (kernel old-gui old-gui-test)
  (utils library cpp-wrap) (utils library tree)
  (utils library smart-table)
  (convert rewrite tmtm-brackets)
  (utils plugins plugin-convert)
  (utils misc markup-funcs) (utils misc artwork)
  (utils misc tooltip)
  (utils handwriting handwriting)
  (bibtex bib-utils)
  (utils base environment)
  (texmacs texmacs tm-tools)
  (utils library length) (utils library cursor)
  (utils edit variants)
  (utils library ptrees)
  (bibtex bib-complete)
  (source macro-search)
  (generic generic-edit) (generic document-style)
  (generic document-edit)
  (texmacs texmacs tm-server) (texmacs texmacs tm-view)
  (texmacs texmacs tm-files) (texmacs texmacs tm-print)
  (texmacs keyboard config-kbd) (texmacs menus main-menu)
  (tools ai ai-batch)
  (fonts fonts-ec) (fonts fonts-adobe)
  (fonts fonts-x) (fonts fonts-math)
  (fonts fonts-foreign) (fonts fonts-misc)
  (fonts fonts-composite) (fonts fonts-truetype)
  (database title-transform) (database title-markup)
  (utils edit selections) (texmacs keyboard prefix-kbd)
  (utils edit auto-close) (generic format-edit)
  (text text-drd) (text text-edit)
  (texmacs keyboard latex-kbd)
  (generic embedded-edit) (generic format-drd)
  (generic format-geometry-edit)
  (source source-drd) (source source-edit)
  (doc help-funcs)
  (generic generic-kbd) (generic generic-speech)
  (generic generic-speech-en) (generic generic-speech-fr)
  (text text-kbd) (math math-drd)
  (convert tools tmconcat) (math math-edit)
  (table table-edit)
  (math math-kbd) (math math-stats)
  (math math-speech) (text text-speech)
  (math math-speech-en) (text text-speech-en)
  (math math-speech-fr) (text text-speech-fr)
  (source source-kbd)
  (dynamic dynamic-drd) (dynamic fold-edit)
  (dynamic fold-kbd)
  (utils plugins plugin-eval) (utils plugins plugin-cmd)
  (dynamic scripts-drd) (dynamic scripts-edit)
  (dynamic scripts-kbd)
  (text text-structure) (link locus-edit)
  (version version-drd) (version version-edit)
  (dynamic session-drd) (dynamic session-edit)
  (dynamic calc-drd) (dynamic calc-edit)
  (dynamic calc-table) (dynamic calc-kbd)
  (utils library process)
  (tools spell spell-edit) (tools spell spell-lantool)
  (tools spell spell-kbd)
  (texmacs menus print-widgets) (texmacs menus file-menu)
  (generic generic-menu) (generic document-menu)
  (text text-menu) (generic insert-menu)
  (tools theme theme-edit) (tools theme theme-menu)
  (convert rewrite init-rewrite)
  (doc apidoc-collect) (doc apidoc-funcs)
  (doc apidoc-widgets)
  (prog scheme-tools) (prog scheme-menu)
  (doc apidoc-markup) (doc apidoc)
  (language natural) (utils misc gui-utils)
  (texmacs menus developer-menu) (texmacs menus view-widgets)
  (texmacs menus view-menu)
  (database db-base) (database db-format)
  (database db-users) (database db-version)
  (database db-edit) (database db-convert)
  (database db-tmfs) (database db-widgets)
  (database db-menu)
  (graphics graphics-drd) (graphics graphics-utils)
  (graphics graphics-env) (graphics graphics-object)
  (graphics graphics-main) (graphics graphics-single)
  (dynamic animate-edit)
  (graphics graphics-group) (graphics graphics-animate)
  (graphics graphics-edit)
  (utils misc gui-keyboard) (texmacs menus edit-menu)
  (generic embedded-menu) (generic format-menu)
  (doc help-menu) (math math-menu))

(unless (top-level-bound? 'texmacs-user-kernel)
  (define-top-level-value 'texmacs-user-kernel (interaction-environment)))

;; 3. Lazy-loading registrations and runtime initializations (matches init-texmacs-chez.scm)
(lazy-define (kernel gui menu-convert) make-menu-widget**)

;; Booting utilities
(lazy-define (utils cas cas-out) cas->stree)
(lazy-define (utils test test-convert) delayed-quit
             build-manual build-ref-suite run-test-suite)
(lazy-tmfs-handler (utils automate auto-tmfs) automate)
(lazy-define (utils automate auto-tmfs) auto-load-help)
(lazy-keyboard (utils automate auto-kbd) in-auto?)
(define supports-email? (url-exists-in-path? "mmail"))
(if supports-email? (use-modules (utils email email-tmfs)))

;; Booting BibTeX style modules
(lazy-menu (bibtex bib-widgets) open-bibliography-inserter)

;; Booting main TeXmacs functionality
(lazy-menu (texmacs menus tools-menu) tools-menu)
(lazy-menu (texmacs menus preferences-menu) preferences-menu page-setup-menu)
(lazy-menu (texmacs menus preferences-widgets)
           open-preferences open-plugin-preferences open-plugins-preferences)
(lazy-tool (texmacs menus preferences-tools) preferences-tool)
(tm-define (notify-set-attachment name key val) (noop))

;; Booting generic mode
(lazy-menu (generic document-part)
           preamble-menu document-part-menu project-manage-menu)
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
(lazy-keyboard (text chinese chinese) in-chinese?)

;; Booting math mode
(lazy-keyboard (math math-sem-edit) in-sem-math?)
(lazy-keyboard (math math-adjust-en) in-math?)
(lazy-keyboard (math math-adjust-fr) in-math?)

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
(lazy-define (table table-widgets) open-cell-properties open-table-properties)
(lazy-tool (table table-tools) cell-properties-tool table-properties-tool)
(tm-property (open-cell-properties) (:interactive #t))
(tm-property (open-table-properties) (:interactive #t))

;; Booting graphics mode
(lazy-keyboard (graphics graphics-kbd) in-active-graphics?)
(lazy-define (graphics graphics-kbd) graphics-wheel)
(lazy-menu (graphics graphics-menu) graphics-menu graphics-icons
           graphics-focus-icons)
(lazy-define (graphics graphics-markup) arrow-with-text arrow-with-text*)
(define-secure-symbols arrow-with-text arrow-with-text*)

;; Booting formal and natural languages
(lazy-language (language minimal) minimal)
(lazy-language (language std-math) std-math)

;; Booting educational modes
(lazy-keyboard (education edu-kbd) in-edu-text?)
(lazy-menu (education edu-menu) edu-insert-menu)

;; Booting dynamic modes
(lazy-menu (dynamic fold-menu) insert-fold-menu dynamic-menu dynamic-icons
           graphics-overlays-menu graphics-screens-menu
           graphics-focus-overlays-menu graphics-focus-overlays-icons)
(lazy-menu (dynamic session-menu) insert-session-menu session-help-icons)
(lazy-menu (dynamic scripts-menu) scripts-eval-menu scripts-plot-menu
           plugin-eval-menu plugin-eval-toggle-menu plugin-plot-menu)
(lazy-menu (dynamic calc-menu) calc-table-menu calc-insert-menu
           calc-icourse-menu)
(lazy-menu (dynamic animate-menu) insert-animation-menu animate-toolbar)
(lazy-define (dynamic scripts-plot) open-plots-editor)
(lazy-initialize (dynamic session-menu) (in-session?))

;; Booting documentation
(lazy-keyboard (doc tmdoc-kbd) in-manual?)
(lazy-keyboard (doc apidoc-kbd) developer-mode?)
(lazy-menu (doc tmdoc-menu) tmdoc-menu tmdoc-icons)
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
(lazy-menu (doc apidoc-menu) apidoc-menu)
(lazy-tmfs-handler (doc docgrep) grep)
(lazy-tmfs-handler (doc tmdoc) help)
(define-secure-symbols tmdoc-include youtube-select)
(tm-property (open-website-builder) (:interactive #t))

;; Booting converters
(lazy-format (convert tmml init-tmml) tmml)
(lazy-format (convert latex init-latex) latex)
(lazy-format (convert html init-html) html)
(lazy-format (convert bibtex init-bibtex) bibtex)
(lazy-format (convert images init-images)
             postscript pdf xfig xmgrace svg xpm jpeg ppm gif png pnm)
(lazy-define (convert images tmimage)
             export-selection-as-graphics clipboard-copy-image)
(lazy-define (convert html tmhtml) ext-tmhtml-eqnarray*)
(define-secure-symbols ext-tmhtml-eqnarray*)
(lazy-define (convert html tmhtml-expand) tmhtml-env-patch)
(lazy-define (convert latex latex-drd) latex-arity latex-type)
(lazy-define (convert latex tmtex) tmtex-env-patch)
(lazy-define (convert latex latex-tools) latex-set-virtual-packages
             latex-has-style? latex-has-package?
             latex-has-texmacs-style? latex-has-texmacs-package?)
(lazy-menu (convert latex tmtex-widgets) tmtex-menu)

;; Booting document parts
(lazy-define (part part-shared) buffer-initialize buffer-notify)
(lazy-menu (part part-menu) document-master-menu)
(lazy-tmfs-handler (part part-tmfs) part)

;; Booting database and bibliography
(lazy-define (database db-widget) open-db-chooser)
(lazy-define (database bib-db) zealous-bib-import zealous-bib-export)
(lazy-define (database bib-manage)
             bib-import-bibtex bib-compile bib-attach open-bib-chooser)
(lazy-define (database bib-local) open-biblio)
(lazy-keyboard (database bib-kbd) in-bib?)
(tm-property (open-biblio) (:interactive #t))

;; Booting security
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

;; Booting remote client / server / notifications
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

;; Booting linking and versioning
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
(lazy-menu (version version-menu) version-menu)
(lazy-keyboard (version version-kbd) with-versioning-tool?)
(lazy-define (version version-tmfs) update-buffer commit-buffer)

;; Booting debugging, themes, spelling, AI
(lazy-menu (debug debug-menu) debug-menu)
(lazy-define (debug debug-widgets) notify-debug-message
             open-debug-console open-error-messages)
(lazy-menu (tools poster poster-menu) poster-block-menu)
(lazy-define (tools ai ai-translate) ai-translate* ai-abort-translate)

;; Booting plugins
(for-each lazy-plugin-initialize (plugin-list))

;; Booting fonts
(lazy-define (fonts font-old-menu)
             text-font-menu math-font-menu prog-font-menu)
(lazy-define (fonts font-new-widgets)
             open-font-selector open-document-font-selector
             open-document-other-font-selector)
(tm-property (open-font-selector) (:interactive #t))
(tm-property (open-document-font-selector) (:interactive #t))

;; Booting tests and updater
(lazy-define (check check-master) check-all run-checks run-all-tests
             run-integration-tests)
(when (updater-supported?)
  (use-modules (utils misc updater))
  (delayed (:idle 2000) (updater-initialize)))

(delayed (:idle 10000) (autosave-delayed))
(texmacs-banner)
