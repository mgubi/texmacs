;;; TODO: UNDER REVIEWED AI GENERATED CODE.
;;;
;;; compiler-db.scm -- Persistent compiler database for the Chez TeXmacs build.
;;;
;;; This file is loaded by compile-texmacs.scm. It has no dependency on the
;;; TeXmacs runtime; it only needs Chez Scheme.
;;;
;;; In memory the database is a hash table keyed by module name. On disk it is
;;; a list-based snapshot record, so fasl files stay simple and ordered.

;; The record accessor compiler-db-table is generated below; the earmuffed
;; constant avoids shadowing it.

(library (compiler-db)
  (export
    *compiler-db-schema-version*
    module-info?
    module-info-name
    module-info-path
    module-info-exports
    module-info-global-values
    module-info-global-macros
    module-info-macro-impls
    module-info-deps
    module-info-compile-uses
    module-info-runtime-uses
    module-info-interface-hash
    module-info-source-hash
    module-info-artifact-hash
    module-info-built-at
    make-module-info
    compiler-db?
    compiler-db-chez-version
    compiler-db-generated-at
    compiler-db-table
    compiler-db-modules
    make-compiler-db
    make-empty-compiler-db
    compiler-db-ref
    compiler-db-compatible?
    compiler-db-set!
    read-compiler-db
    read-compiler-db-or-empty
    write-compiler-db)
  (import (chezscheme))

  (define *compiler-db-schema-version* 1)

  ;;; ---------------------------------------------------------------------------
  ;;; Records
  ;;; ---------------------------------------------------------------------------

  (define-record-type (module-info %make-module-info module-info?)
    (fields
      (immutable name)             ; module name, e.g. (database db-base)
      (immutable path)             ; source path relative to progs
      (immutable exports)          ; define-public names
      (immutable global-values)    ; tm-define names; duplicates allowed (overloads)
      (immutable global-macros)    ; tm-define-macro names
      (immutable macro-impls)      ; $impl helper names
      (immutable deps)             ; declared texmacs-module dependencies
      (immutable compile-uses)     ; global names needed during expansion
      (immutable runtime-uses)     ; global names needed at load/run time
      (immutable interface-hash)   ; compile-time interface digest, or #f
      (immutable source-hash)      ; source content digest, or #f
      (immutable artifact-hash)    ; .so content digest, or #f
      (immutable built-at))        ; build timestamp, or #f
    (nongenerative module-info-v1))

  (define-record-type (compiler-db %make-compiler-db compiler-db?)
    (fields
      (immutable chez-version)     ; string, e.g. "Chez Scheme Version 10.5.0"
      (immutable generated-at)     ; integer timestamp
      (immutable table compiler-db-table))
    (nongenerative compiler-db-v2))

  ;; On-disk shape. The table is flattened to a list before writing.
  (define-record-type (compiler-db-file %make-compiler-db-file compiler-db-file?)
    (fields
      (immutable format-version)
      (immutable chez-version)
      (immutable generated-at)
      (immutable modules))
    (nongenerative compiler-db-file-v1))

  ;;; ---------------------------------------------------------------------------
  ;;; Small predicates and list helpers
  ;;; ---------------------------------------------------------------------------

  (define (compiler-db-exact-integer? x)
    (and (integer? x) (exact? x)))

  (define (compiler-db-non-empty-string? x)
    (and (string? x) (> (string-length x) 0)))

  (define (compiler-db-list-of-symbols? x)
    (and (list? x) (andmap symbol? x)))

  (define (compiler-db-mod-name? x)
    (and (list? x) (pair? x) (andmap symbol? x)))

  (define (compiler-db-list-of-mod-names? x)
    (and (list? x) (andmap compiler-db-mod-name? x)))

  (define (compiler-db-duplicates-symbols lst)
    (let loop ((rest lst) (seen '()) (dups '()))
      (cond ((null? rest) (reverse dups))
            ((memq (car rest) seen)
             (loop (cdr rest) seen (cons (car rest) dups)))
            (else
             (loop (cdr rest) (cons (car rest) seen) dups)))))

  (define (compiler-db-duplicates-mod-names lst)
    (let loop ((rest lst) (seen '()) (dups '()))
      (cond ((null? rest) (reverse dups))
            ((member (car rest) seen)
             (loop (cdr rest) seen (cons (car rest) dups)))
            (else
             (loop (cdr rest) (cons (car rest) seen) dups)))))

  (define (compiler-db-mod-name<? a b)
    (cond ((null? a) (pair? b))
          ((null? b) #f)
          ((eq? (car a) (car b))
           (compiler-db-mod-name<? (cdr a) (cdr b)))
          (else
           (string<? (symbol->string (car a))
                     (symbol->string (car b))))))

  ;;; ---------------------------------------------------------------------------
  ;;; Validation
  ;;; ---------------------------------------------------------------------------

  (define (compiler-db-check-symbol-list field value)
    (unless (compiler-db-list-of-symbols? value)
      (error 'compiler-db
             (string-append field " must be a list of symbols")
             value))
    (let ((dups (compiler-db-duplicates-symbols value)))
      (when (pair? dups)
        (error 'compiler-db
               (string-append field " contains duplicate symbols")
               dups))))

  ;; tm-define supports overloads and conditional definitions, so the same name
  ;; may legitimately appear more than once in one module.
  (define (compiler-db-check-symbol-list/allow-duplicates field value)
    (unless (compiler-db-list-of-symbols? value)
      (error 'compiler-db
             (string-append field " must be a list of symbols")
             value)))

  (define (compiler-db-check-mod-names field value)
    (unless (compiler-db-list-of-mod-names? value)
      (error 'compiler-db
             (string-append field " must be a list of module names")
             value))
    (let ((dups (compiler-db-duplicates-mod-names value)))
      (when (pair? dups)
        (error 'compiler-db
               (string-append field " contains duplicate module names")
               dups))))

  (define (compiler-db-check-hash field value)
    (unless (or (not value) (compiler-db-exact-integer? value))
      (error 'compiler-db
             (string-append field " must be #f or an exact integer")
             value)))

  (define (validate-module-info info)
    (unless (module-info? info)
      (error 'compiler-db "not a module-info record" info))

    (unless (compiler-db-mod-name? (module-info-name info))
      (error 'compiler-db
             "module name must be a non-empty list of symbols"
             (module-info-name info)))

    (unless (compiler-db-non-empty-string? (module-info-path info))
      (error 'compiler-db
             "module path must be a non-empty string"
             (module-info-path info)))

    (compiler-db-check-symbol-list 'exports (module-info-exports info))
    (compiler-db-check-symbol-list/allow-duplicates
     'global-values (module-info-global-values info))
    (compiler-db-check-symbol-list 'global-macros (module-info-global-macros info))
    (compiler-db-check-symbol-list 'macro-impls (module-info-macro-impls info))
    (compiler-db-check-mod-names 'deps (module-info-deps info))
    (compiler-db-check-symbol-list 'compile-uses (module-info-compile-uses info))
    (compiler-db-check-symbol-list 'runtime-uses (module-info-runtime-uses info))

    (compiler-db-check-hash 'interface-hash (module-info-interface-hash info))
    (compiler-db-check-hash 'source-hash (module-info-source-hash info))
    (compiler-db-check-hash 'artifact-hash (module-info-artifact-hash info))

    (let ((built-at (module-info-built-at info)))
      (unless (or (not built-at) (compiler-db-exact-integer? built-at))
        (error 'compiler-db
               "built-at must be #f or an exact integer"
               built-at)))

    info)

  (define (validate-compiler-db db)
    (unless (compiler-db? db)
      (error 'compiler-db "not a compiler-db record" db))

    (unless (compiler-db-non-empty-string? (compiler-db-chez-version db))
      (error 'compiler-db
             "chez-version must be a non-empty string"
             (compiler-db-chez-version db)))

    (unless (compiler-db-exact-integer? (compiler-db-generated-at db))
      (error 'compiler-db
             "generated-at must be an exact integer"
             (compiler-db-generated-at db)))

    (let ((table (compiler-db-table db)))
      (unless (hashtable? table)
        (error 'compiler-db "compiler database table is not a hash table" table))
      (vector-for-each
       (lambda (cell)
         (let ((key (car cell))
               (info (cdr cell)))
           (validate-module-info info)
           (unless (equal? key (module-info-name info))
             (error 'compiler-db
                    "compiler database key does not match module name"
                    key
                    (module-info-name info)))))
       (hashtable-cells table)))

    db)

  ;;; ---------------------------------------------------------------------------
  ;;; Constructors
  ;;; ---------------------------------------------------------------------------

  (define (make-module-table modules)
    (let ((table (make-hashtable equal-hash equal?)))
      (for-each
       (lambda (info)
         (validate-module-info info)
         (let ((name (module-info-name info)))
           (when (hashtable-ref table name #f)
             (error 'compiler-db
                    "compiler database contains duplicate module names"
                    name))
           (hashtable-set! table name info)))
       modules)
      table))

  (define (make-module-info name path exports global-values global-macros
                            macro-impls deps compile-uses runtime-uses
                            interface-hash source-hash artifact-hash built-at)
    (validate-module-info
     (%make-module-info name path exports global-values global-macros
                        macro-impls deps compile-uses runtime-uses
                        interface-hash source-hash artifact-hash built-at)))

  (define (make-compiler-db chez-version generated-at modules)
    (validate-compiler-db
     (%make-compiler-db chez-version generated-at
                        (make-module-table modules))))

  (define (make-empty-compiler-db)
    (make-compiler-db (scheme-version)
                      (time-second (current-time))
                      '()))

  ;;; ---------------------------------------------------------------------------
  ;;; Queries
  ;;; ---------------------------------------------------------------------------

  (define (compiler-db-ref db name)
    (hashtable-ref (compiler-db-table db) name #f))

  (define (compiler-db-compatible? db)
    (and (compiler-db? db)
         (string=? (compiler-db-chez-version db) (scheme-version))))

  (define (compiler-db-modules db)
    (sort (lambda (a b)
            (compiler-db-mod-name<? (module-info-name a)
                                (module-info-name b)))
          (vector->list (hashtable-values (compiler-db-table db)))))

  ;; Insert a module, or replace the existing record with the same name.
  (define (compiler-db-set! db info)
    (validate-module-info info)
    (hashtable-set! (compiler-db-table db) (module-info-name info) info)
    info)

  ;;; ---------------------------------------------------------------------------
  ;;; Fasl persistence
  ;;; ---------------------------------------------------------------------------

  (define (read-compiler-db path)
    (if (not (file-exists? path))
        #f
        (let ((port (open-file-input-port path)))
          (dynamic-wind
            (lambda () #f)
            (lambda ()
              (let ((snapshot (fasl-read port)))
                (unless (compiler-db-file? snapshot)
                  (error 'compiler-db
                         "compiler database file has the wrong record type"
                         snapshot))
                (unless (= (compiler-db-file-format-version snapshot)
                           *compiler-db-schema-version*)
                  (error 'compiler-db
                         "compiler database schema version mismatch"
                         (compiler-db-file-format-version snapshot)))
                (make-compiler-db
                 (compiler-db-file-chez-version snapshot)
                 (compiler-db-file-generated-at snapshot)
                 (compiler-db-file-modules snapshot))))
            (lambda ()
              (close-port port))))))

  (define (read-compiler-db-or-empty path)
    (guard (e (else
               (display "WARNING: ignoring compiler database " (current-error-port))
               (display path (current-error-port))
               (display ": " (current-error-port))
               (display-condition e (current-error-port))
               (newline (current-error-port))
               (make-empty-compiler-db)))
      (let ((db (read-compiler-db path)))
        (if (and db (compiler-db-compatible? db))
            db
            (make-empty-compiler-db)))))

  (define (write-compiler-db db path)
    (validate-compiler-db db)
    (let* ((snapshot
            (%make-compiler-db-file
             *compiler-db-schema-version*
             (compiler-db-chez-version db)
             (compiler-db-generated-at db)
             (compiler-db-modules db)))
           (tmp (string-append path ".tmp")))
      (when (file-exists? tmp)
        (delete-file tmp))
      (guard (e (else
                 (when (file-exists? tmp)
                   (delete-file tmp))
                 (raise e)))
        (let ((port (open-file-output-port tmp (file-options no-fail))))
          (dynamic-wind
            (lambda () #f)
            (lambda ()
              (fasl-write snapshot port))
            (lambda ()
              (close-port port))))
        ;; Chez's rename-file maps to MoveFile on Windows, which fails when
        ;; the destination already exists; POSIX rename(2) replaces it.
        ;; Delete the old cache first so both platforms behave the same.
        (when (file-exists? path)
          (delete-file path))
        (rename-file tmp path)
        path)))
)
