;;; compiler-scan.scm -- Scan TeXmacs Scheme sources into compiler-db records.
;;;
;;; This library depends only on compiler-db and Chez Scheme. It does not load
;;; the TeXmacs runtime.

(library (compiler-scan)
  (export read-all-forms
          scan-interface-hash
          scan-source-file
          scan-source-files
          scan-source-files->db
          unused-name-list-entries)
  (import (chezscheme) (compiler-db) (chez-reader))

  ;; The reader and the dialect folding are in the library (chez-reader),
  ;; TeXmacs/progs/kernel/boot/chez-reader.scm, which boot-chez.scm loads:
  ;; the loader of sources at run time reads them the same way.

  (define (defined-public-name f)
    (and (pair? f) (pair? (cdr f))
         (memq (car f) '(define-public define-public-macro provide-public))
         (let ((h (cadr f)))
           (let head ((h h)) (if (pair? h) (head (car h)) (and (symbol? h) h))))))

  ;; Definitions the Guile branch makes of what Chez has natively, or
  ;; boot-chez.scm (module-load).
  (define chez-native-names '(when unless case-lambda module-load))

  ;; (path . names): the names chez-compat.scm defines, read once per path
  (define chez-compat-names #f)

  (define (compat-file? file-path)
    (let ((n (string-length file-path)) (suffix "kernel/boot/chez-compat.scm"))
      (and (>= n (string-length suffix))
           (string=? (substring file-path (- n (string-length suffix)) n) suffix))))

  (define (chez-compat-path file-path)
    ;; FILE-PATH is .../progs/<rel>; find .../progs from the module name
    (let loop ((i (- (string-length file-path) 1)))
      (cond ((< i 5) #f)
            ((string=? (substring file-path (- i 5) (+ i 1)) "progs/")
             (string-append (substring file-path 0 (+ i 1))
                            "kernel/boot/chez-compat.scm"))
            (else (loop (- i 1))))))

  (define (compat-names file-path)
    (let ((p (chez-compat-path file-path)))
      (and p (file-exists? p)
           (begin
             (unless (and chez-compat-names (equal? (car chez-compat-names) p))
               (set! chez-compat-names
                     (cons p
                           (filter (lambda (x) x)
                                   (map (lambda (f)
                                          (or (defined-public-name f)
                                              (and (pair? f) (eq? (car f) 'define-syntax)
                                                   (pair? (cdr f)) (symbol? (cadr f))
                                                   (cadr f))))
                                        (fold-dialect-forms (read-raw-forms p)))))))
             (cdr chez-compat-names)))))

  ;; Private helpers that macros of other modules call when they expand.
  ;; The expansion happens outside the defining module here, so Chez needs
  ;; them public; the other interpreters keep them private.
  (define chez-promoted-names
    '(cur-props lambda* property-rewrite lazy-define-table
      tm-define-overloaded-chez lazy-define-one-chez
      process-translate
      kbd-add-condition kbd-map-one kbd-map-body kbd-remove-one kbd-remove-body
      is-assume? quote-all logic-table-rule
      save-learned retrieve-learned
      define-preference define-preference-call-back
      lazy-input-converter-force
      plugin-serializer))  ; read by check/plugins-test.scm

  ;; Private definitions which the tests reach in their module, by module
  ;; (two modules may have private definitions of the same name) (priv,
  ;; module-ref, eval in resolve-module, next-serial, (eval '(set! x ...)
  ;; (resolve-module ...)): the patterns of TeXmacs/progs/check/*.scm): global
  ;; definitions on Chez, whose modules export no private binding and
  ;; cannot assign an imported one.
  (define chez-global-names
    '(
      ("bibtex/zotero.scm" asking-key? extra-citation-key js-string key-author-year web-path)
      ("check/ai-test.scm" attach-session?)
      ("check/git-test.scm" keep-dialogues?)
      ("check/kbd-menu-test.scm" kbd-menu-test-closing? kbd-test-on?)
      ("check/remote-test.scm" remote-active?)
      ("check/zotero-test.scm" fake-error? fake-library-answer fake? sim-browser? test-db?)
      ("client/client-base.scm" client-beat client-heard client-notify-heard client-ping-sent client-serial client-start-errno->string split-server-name-and-port)
      ("client/client-chat.scm" chat-room-name chat-room-table-entry fix-link fix-links message->document message->share messages->document shared-table-entry)
      ("client/client-db-sync.scm" db-change-status db-dub-in-sync db-last-sync)
      ("client/client-live.scm" live-cursor-color live-cursor-lids live-cursor-sent live-cursor-servers live-cursors live-table-entry)
      ("client/client-sync.scm" dont-sync? get-sync-status requalify-deleted)
      ("client/client-tmfs.scm" build-dir-breadcrumbs sort-directory-entries)
      ("convert/html/tmhtml.scm" tmhtml-formula-env-of)
      ("database/bib-manage.scm" bib-sources)
      ("database/db-widgets.scm" db-search-results)
      ("doc/docgrep.scm" docgrep docgrep-files)
      ("doc/help-funcs.scm" url-resolve-help)
      ("doc/tmdoc.scm" tmdoc-add-aux tmdoc-demote tmdoc-down tmdoc-expand tmdoc-internal-label tmdoc-internalize tmdoc-language tmdoc-relative tmdoc-remove-hyper-links)
      ("generic/document-edit.scm" zotero-update)
      ("generic/format-widgets.scm" page-this-bg-color-val)
      ("generic/search-widgets.scm" adjust-by in-leading-text? perform-search* range-unchanged? replace-skip-message replace-trace search-filter-accepts-all? search-filter-table)
      ("graphics/graphics-single.scm" object_commit)
      ("kernel/texmacs/tm-modes.scm" versioning-directory-delay)
      ("link/ref-edit.scm" in-inactive-reference? keyboard-preview-update)
      ("part/part-shared.scm" mirror-black-list mirror-invalidate mirror-treat-pending)
      ("server/server-base.scm" credentials->authentications generate-preferences-form is-on? load-preferences-in-stree server-mailer-instantiate server-serial)
      ("server/server-live.scm" live-cursor-refused live-cursor-table live-load live-name live-save)
      ("server/server-tmfs.scm" version->file-name version-get-info version-get-versions)
      ("tools/comment/comment-edit.scm" volatile-cache-stamp)
      ("utils/misc/tooltip.scm" tooltips)
      ("version/version-compare.scm" version-grain)
      ("version/version-kbd.scm" shortcut-compare shortcut-root)))

  ;; the names of the file at file-path (.../progs/<rel>) which are global
  (define (file-global-names file-path)
    (let loop ((l chez-global-names))
      (cond ((null? l) '())
            ((let* ((rel (string-append "/" (caar l)))
                    (n (string-length file-path)) (m (string-length rel)))
               (and (>= n m) (string=? (substring file-path (- n m) n) rel)))
             (cdar l))
            (else (loop (cdr l))))))

  ;; The entries of the two lists above which were applied, so that the
  ;; driver can report the stale ones (unused-name-list-entries)
  (define used-names (make-hashtable equal-hash equal?))

  (define (globalize-private f names file-path)
    (if (and (pair? f) (eq? (car f) 'define) (pair? (cdr f))
             (let ((h (cadr f)))
               (memq (if (pair? h) (car h) h) names)))
        (let* ((h (cadr f))
               (name (if (pair? h) (car h) h))
               (val (if (pair? h) `(lambda ,(cdr h) ,@(cddr f)) (caddr f))))
          (hashtable-set! used-names (cons file-path name) #t)
          `(define-top-level-value ',name ,val))
        f))

  (define (promote-private f)
    (if (and (pair? f) (eq? (car f) 'define) (pair? (cdr f))
             (let ((h (cadr f)))
               (memq (if (pair? h) (car h) h) chez-promoted-names)))
        (let ((h (cadr f)))
          (hashtable-set! used-names (if (pair? h) (car h) h) #t)
          (cons 'define-public (cdr f)))
        f))

  ;; The names of chez-promoted-names and of chez-global-names (as
  ;; "<file> <name>") that no scanned definition used: after a scan of all
  ;; the sources, entries which no longer name a private definition
  (define (unused-name-list-entries)
    (define (used-global? rel name)
      (exists (lambda (k)
                (and (pair? k) (eq? (cdr k) name)
                     (let* ((p (car k)) (r (string-append "/" rel))
                            (n (string-length p)) (m (string-length r)))
                       (and (>= n m) (string=? (substring p (- n m) n) r)))))
              (vector->list (hashtable-keys used-names))))
    (append
     (filter (lambda (n) (not (hashtable-ref used-names n #f)))
             chez-promoted-names)
     (apply append
            (map (lambda (entry)
                   (map (lambda (n) (string-append (car entry) " "
                                                   (symbol->string n)))
                        (filter (lambda (n) (not (used-global? (car entry) n)))
                                (cdr entry))))
                 chez-global-names))))

  ;; A name defined again in the same module: Guile assigns it, Chez does
  ;; not allow it.  The next definitions become assignments; provide-public,
  ;; which defines only what is not defined yet, is dropped for a name the
  ;; module or Chez (car...) already defines.
  (define (defined-head f)
    (and (pair? f) (pair? (cdr f))
         (memq (car f) '(define define-public provide-public))
         (let ((h (cadr f))) (if (pair? h) (car h) h))))

  (define (chez-defines? name)
    (guard (e (#t #f)) (top-level-bound? name (scheme-environment))))

  (define (single-definitions forms)
    (let loop ((fs forms) (seen '()) (acc '()))
      (if (null? fs)
          (reverse acc)
          (let* ((f (car fs)) (n (defined-head f)))
            (cond ((not (symbol? n)) (loop (cdr fs) seen (cons f acc)))
                  ((and (eq? (car f) 'provide-public)
                        (or (memq n seen) (chez-defines? n)))
                   (loop (cdr fs) seen acc))
                  ((memq n seen)
                   (let ((val (if (pair? (cadr f))
                                  `(lambda ,(cdadr f) ,@(cddr f))
                                  (caddr f)))
                         ;; a define-public of a name first defined privately
                         ;; makes the first definition public
                         (acc (if (eq? (car f) 'define-public)
                                  (map (lambda (g)
                                         (if (and (pair? g) (eq? (car g) 'define)
                                                  (eq? (defined-head g) n))
                                             (cons 'define-public (cdr g))
                                             g))
                                       acc)
                                  acc)))
                     (loop (cdr fs) seen (cons `(set! ,n ,val) acc))))
                  (else (loop (cdr fs) (cons n seen) (cons f acc))))))))

  (define (read-all-forms file-path)
    (let* ((globals (file-global-names file-path))
           (forms (map (lambda (f)
                         (globalize-private (promote-private f) globals file-path))
                      (single-definitions
                        (tm-read-guile-file file-path)))))
      (if (compat-file? file-path)
          forms
          (let ((names (append chez-native-names
                               (or (compat-names file-path) '()))))
            (filter (lambda (f)
                      (let ((n (defined-public-name f)))
                        (not (and n (memq n names)))))
                    forms)))))

  ;;; -------------------------------------------------------------------------
  ;;; File and form helpers
  ;;; -------------------------------------------------------------------------


  (define (scan-read-text path)
    (guard (e (else #f))
      (call-with-input-file path
        (lambda (port)
          (get-string-all port)))))

  ;; The texmacs-module form, which must come first, as the transpiler
  ;; requires (module-header, convert-to-r6rs.scm)
  (define (scan-header forms)
    (and (pair? forms)
         (pair? (car forms))
         (eq? (caar forms) 'texmacs-module)
         (car forms)))

  (define (scan-header->deps header mod-name)
    (let ((deps '()))
      (for-each
       (lambda (opt)
         (when (and (pair? opt) (memq (car opt) '(:use :inherit)))
           (for-each
            (lambda (s)
              (unless (or (equal? s mod-name) (member s deps))
                (set! deps (cons s deps))))
            (cdr opt))))
       (cddr header))
      (reverse deps)))

  (define (scan-leftmost-symbol x)
    (cond ((symbol? x) x)
          ((pair? x) (scan-leftmost-symbol (car x)))
          (else #f)))

  (define (scan-defined-name form head)
    (or (scan-leftmost-symbol head)
        (error 'compiler-scan
               "declaration has no valid name"
               form)))

  (define (scan-macro-impl-name name)
    (string->symbol (string-append (symbol->string name) "$impl")))

  ;; `bib-define-style' does not name its export directly: the transpiler
  ;; synthesises bib-<style>? from the style string (prepare-forms in
  ;; compile-texmacs.scm). Mirror that here so the static index agrees with the
  ;; emitted module header.
  (define (scan-bib-predicate-name form)
    (let ((style (and (pair? (cdr form)) (cadr form))))
      (if (string? style)
          (string->symbol (string-append "bib-" style "?"))
          (error 'compiler-scan
                 "bib-define-style has no valid style name"
                 form))))

  (define (scan-flatten-begins forms)
    (let loop ((rest forms) (acc '()))
      (cond ((null? rest) (reverse acc))
            ((and (pair? (car rest)) (eq? (caar rest) 'begin))
             (loop (append (cdr (car rest)) (cdr rest)) acc))
            (else
             (loop (cdr rest) (cons (car rest) acc))))))

  ;;; -------------------------------------------------------------------------
  ;;; Declaration collection
  ;;; -------------------------------------------------------------------------

  (define (scan-collect forms)
    (let loop ((rest (scan-flatten-begins forms))
               (exports '())
               (globals '())
               (macros '())
               (impls '()))
      (if (null? rest)
          ;; a tm-define may be overloaded in the module which defines it
          ;; (check/define-test.scm): each name is exported once
          (values (let dedup ((l (reverse exports)) (seen '()))
                    (cond ((null? l) (reverse seen))
                          ((memq (car l) seen) (dedup (cdr l) seen))
                          (else (dedup (cdr l) (cons (car l) seen)))))
                  (reverse globals)
                  (reverse macros)
                  (reverse impls))
          (let* ((form (car rest))
                 (head (and (pair? form) (car form)))
                 (arg (and (pair? form) (pair? (cdr form)) (cadr form))))
            (case head
              ;; define-module-macro and define-export-macro are public macro
              ;; definers too. prepare-forms counts the former as public and
              ;; rewrites the latter into define-public-macro, so the static
              ;; index must record the same names as exports.
              ((define-public provide-public define-public-macro
                define-module-macro define-export-macro)
               (loop (cdr rest)
                     (cons (scan-defined-name form arg) exports)
                     globals macros impls))
              ((bib-define-style)
               (loop (cdr rest)
                     (cons (scan-bib-predicate-name form) exports)
                     globals macros impls))
              ((export)
               (loop (cdr rest)
                     (append (cdr form) exports)
                     globals macros impls))
              ((tm-define)
               (loop (cdr rest)
                     exports
                     (cons (scan-defined-name form arg) globals)
                     macros impls))
              ((tm-define-macro)
               (let* ((name (scan-defined-name form arg))
                      (impl (scan-macro-impl-name name)))
                 (loop (cdr rest)
                       exports
                       globals
                       (cons name macros)
                       (cons impl impls))))
              (else
               (loop (cdr rest) exports globals macros impls)))))))

  ;;; -------------------------------------------------------------------------
  ;;; Canonical hashes
  ;;; -------------------------------------------------------------------------

  (define (scan-sort-symbols syms)
    (sort (lambda (a b)
            (string<? (symbol->string a) (symbol->string b)))
          syms))

  (define (scan-canonical-symbols syms)
    (let loop ((rest (scan-sort-symbols syms)) (acc ""))
      (cond ((null? rest) acc)
            (else
             (loop (cdr rest)
                   (string-append acc (symbol->string (car rest)) "\n"))))))

  (define (scan-interface-hash exports global-macros macro-impls)
    (content-hash
     (string-append "E\n" (scan-canonical-symbols exports)
                    "G\n" (scan-canonical-symbols global-macros)
                    "M\n" (scan-canonical-symbols macro-impls))))

  (define (scan-source-hash text)
    (and text (content-hash text)))

  ;;; -------------------------------------------------------------------------
  ;;; One file
  ;;; -------------------------------------------------------------------------

  ;; A file that the reader cannot read raises (read-all-forms): the build
  ;; stops where the problem is, instead of silently dropping a module from
  ;; the database. A file without a texmacs-module header is skipped.
  (define (scan-source-file progs-dir rel-path)
    (let* ((full (string-append progs-dir "/" rel-path))
           (forms (read-all-forms full))
           (header (scan-header forms)))
      (cond
       ((not header)
        (display "SCAN-SKIP no-header " (current-error-port))
        (display rel-path (current-error-port))
        (newline (current-error-port))
        #f)
       (else
        (let* ((mod-name (cadr header))
               (deps (scan-header->deps header mod-name))
               (text (scan-read-text full)))
          (call-with-values
           (lambda () (scan-collect forms))
           (lambda (exports globals macros impls)
             (make-module-info
              mod-name
              rel-path
              exports
              globals
              macros
              impls
              deps
              '()                          ; compile-uses
              '()                          ; runtime-uses
              (scan-interface-hash exports macros impls)
              (scan-source-hash text)
              #f                           ; artifact-hash
              (time-second (current-time))))))))))

  ;;; -------------------------------------------------------------------------
  ;;; Multiple files and trees
  ;;; -------------------------------------------------------------------------

  (define (scan-source-files progs-dir rel-paths)
    (let loop ((rest rel-paths) (acc '()))
      (cond ((null? rest) (reverse acc))
            (else
             (let ((info (scan-source-file progs-dir (car rest))))
               (loop (cdr rest)
                     (if info (cons info acc) acc)))))))

  (define (scan-source-files->db progs-dir rel-paths)
    (let ((db (make-empty-compiler-db)))
      (for-each
       (lambda (rel-path)
         (let ((info (scan-source-file progs-dir rel-path)))
           (when info
             (compiler-db-set! db info))))
       rel-paths)
      db))
)
