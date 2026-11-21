;;; TODO: UNDER REVIEWED AI GENERATED CODE.
;;;
;;; compiler-scan.scm -- Scan TeXmacs Scheme sources into compiler-db records.
;;;
;;; This library depends only on compiler-db and Chez Scheme. It does not load
;;; the TeXmacs runtime.

(library (compiler-scan)
  (export read-all-forms
          scan-interface-hash
          scan-source-file
          scan-source-files
          scan-source-files->db)
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

  (define chez-compat-names #f)

  (define (compat-file? file-path)
    (let ((n (string-length file-path)) (suffix "kernel/boot/chez-compat.scm"))
      (and (>= n (string-length suffix))
           (string=? (substring file-path (- n (string-length suffix)) n) suffix))))

  (define (chez-compat-path file-path)
    ;; FILE-PATH is .../progs/<rel>; find .../progs from the module name
    (let loop ((i (- (string-length file-path) 1)))
      (cond ((< i 6) #f)
            ((string=? (substring file-path (- i 5) (+ i 1)) "progs/")
             (string-append (substring file-path 0 (+ i 1))
                            "kernel/boot/chez-compat.scm"))
            (else (loop (- i 1))))))

  (define (compat-names file-path)
    (or chez-compat-names
        (let ((p (chez-compat-path file-path)))
          (and p (file-exists? p)
               (begin
                 (set! chez-compat-names
                       (filter (lambda (x) x)
                               (map (lambda (f)
                                      (or (defined-public-name f)
                                          (and (pair? f) (eq? (car f) 'define-syntax)
                                               (pair? (cdr f)) (symbol? (cadr f))
                                               (cadr f))))
                                    (fold-dialect-forms (read-raw-forms p)))))
                 chez-compat-names)))))

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

  ;; Private definitions which the tests reach in their module (priv,
  ;; module-ref, eval in resolve-module, next-serial, (eval '(set! x ...)
  ;; (resolve-module ...)): the patterns of TeXmacs/progs/check/*.scm): global
  ;; definitions on Chez, whose modules export no private binding and
  ;; cannot assign an imported one.
  (define chez-global-names
    '(volatile-cache-stamp version-grain chat-room-name chat-room-table-entry client-serial
      client-start-errno->string credentials->authentications
      db-change-status db-dub-in-sync db-last-sync docgrep dont-sync?
      fix-link fix-links get-sync-status is-on? live-load live-name
      live-save live-table-entry load-preferences-in-stree message->document
      message->share messages->document object_commit requalify-deleted
      server-mailer-instantiate server-serial shared-table-entry
      sort-directory-entries split-server-name-and-port tmdoc-add-aux
      tmdoc-down tmdoc-expand tmdoc-internal-label tmdoc-internalize
      tmdoc-language tmdoc-relative tmdoc-remove-hyper-links
      url-resolve-help version->file-name version-get-info
      version-get-versions))

  (define (globalize-private f)
    (if (and (pair? f) (eq? (car f) 'define) (pair? (cdr f))
             (let ((h (cadr f)))
               (memq (if (pair? h) (car h) h) chez-global-names)))
        (let* ((h (cadr f))
               (name (if (pair? h) (car h) h))
               (val (if (pair? h) `(lambda ,(cdr h) ,@(cddr f)) (caddr f))))
          `(define-top-level-value ',name ,val))
        f))

  (define (promote-private f)
    (if (and (pair? f) (eq? (car f) 'define) (pair? (cdr f))
             (let ((h (cadr f)))
               (memq (if (pair? h) (car h) h) chez-promoted-names)))
        (cons 'define-public (cdr f))
        f))

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
                                  (caddr f))))
                     (loop (cdr fs) seen (cons `(set! ,n ,val) acc))))
                  (else (loop (cdr fs) (cons n seen) (cons f acc))))))))

  (define (read-all-forms file-path)
    (let ((forms (map (lambda (f) (globalize-private (promote-private f)))
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

  (define (scan-header forms)
    (let loop ((rest forms))
      (cond ((null? rest) #f)
            ((and (pair? (car rest))
                  (eq? (caar rest) 'texmacs-module))
             (car rest))
            (else (loop (cdr rest))))))

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
    (string-hash
     (string-append "E\n" (scan-canonical-symbols exports)
                    "G\n" (scan-canonical-symbols global-macros)
                    "M\n" (scan-canonical-symbols macro-impls))))

  (define (scan-source-hash text)
    (and text (string-hash text)))

  ;;; -------------------------------------------------------------------------
  ;;; One file
  ;;; -------------------------------------------------------------------------

  ;; A file that cannot be read raises: the build stops where the problem is,
  ;; instead of silently dropping a module from the database.
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
