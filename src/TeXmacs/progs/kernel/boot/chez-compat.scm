;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : chez-compat.scm
;; DESCRIPTION : Guile compatibility primitives for Chez Scheme
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (kernel boot chez-compat))

;; Everything here is a Guile-ism that the rest of the tree still relies on.
;; This module is compiled first (Tier 1) and loaded before any other module,
;; so a shim may live here rather than in kernel/boot/boot-chez.scm provided the
;; bootstrap itself does not need it before the module system exists.  What must
;; stay in boot-chez.scm: the engine globals, `eval', `equal?', the hash-table
;; constructors the module registry uses, the io hooks (`display'/`write'/
;; `tm-output'), `quit-TeXmacs-scheme' (called from C++), and the definition /
;; export machinery (`define-public', `define-public-macro', `define-macro',
;; `provide-public') that every module needs in order to be compiled at all.

;;; Numeric helpers
;;
;; No `1+' / `1-' here: `(chezscheme)' already exports both with the same
;; meaning, so a shim would only shadow a builtin with itself.

;;; Character sets
;;
;; `char-set:letter' and `char-set:letter+digit' are not defined: the tree only
;; ever asks for whitespace, digit, lower-case and upper-case.
(define-public char-set:whitespace (string->list " \t\n\r\f\v"))
(define-public char-set:digit (string->list "0123456789"))
(define-public char-set:lower-case (string->list "abcdefghijklmnopqrstuvwxyz"))
(define-public char-set:upper-case (string->list "ABCDEFGHIJKLMNOPQRSTUVWXYZ"))

;;; List mutators
;;
;; Only `delq!' is used (kernel/boot/srfi.scm).  `delete!', `delq' and `delv!'
;; had no callers and are gone.
(define-public (delq! x l) (remq! x l))

(define-public (acons k v l) (cons (cons k v) l))

;;; List traversal & recursion helpers
;;
;; Moved from kernel/library/list.scm to break the circular dependency with
;; kernel/library/base.scm (which requires list-drop-while, list-intersperse,
;; list-fold, and list-fold-right for string trimming and iteration). Because
;; list.scm depends on ahash-table.scm and srfi.scm, it must remain loaded
;; after them; providing these primitive list recursors early in chez-compat.scm
;; allows base.scm to resolve them without altering pristine module tier order.

(define-public (list-intersperse l x)
  "Insert @x between each element of @l."
  (if (or (null? l) (null? (cdr l))) l
      (cons (car l) (cons x (list-intersperse (cdr l) x)))))

(define-public (list-drop-while l pred?)
  "Drop the first elements of @l while @pred? is satisfied."
  (let next ((l l))
    (cond ((null? l) '())
          ((pred? (car l)) (next (cdr l)))
          (else l))))

(define-public (list-fold kons knil list1 . rest)
  "Fundamental list iterator."
  (if (null? rest)
      (let f ((knil knil) (list1 list1))
        (if (null? list1) knil
            (f (kons (car list1) knil) (cdr list1))))
      (let f ((knil knil) (lists (cons list1 rest)))
        (if (let any-null? ((ls lists))
              (and (pair? ls) (or (null? (car ls)) (any-null? (cdr ls)))))
            knil
            (let ((cars (map car lists))
                  (cdrs (map cdr lists)))
              (f (apply kons (append! cars (list knil))) cdrs))))))

(define-public (list-fold-right kons knil clist1 . rest)
  "Fundamental list recursion operator."
  (if (null? rest)
      (let f ((list1 clist1))
        (if (null? list1) knil
            (kons (car list1) (f (cdr list1)))))
      (let f ((lists (cons clist1 rest)))
        (if (let any-null? ((ls lists))
              (and (pair? ls) (or (null? (car ls)) (any-null? (cdr ls)))))
            knil
            (apply kons (append! (map car lists)
                                 (list (f (map cdr lists)))))))))


;;; Association lists
;;
;; `assq-ref' / `assq-set!' / `assq-remove!' had no callers; every alist in the
;; tree is keyed by string or by a value compared with `equal?', so only the
;; `assoc-*' family is kept.
(define-public (assoc-ref alist key) (cond ((assoc key alist) => cdr) (else #f)))
(define-public (assoc-set! alist key val)
  (let ((entry (assoc key alist)))
    (if entry (begin (set-cdr! entry val) alist) (cons (cons key val) alist))))
(define-public (assoc-remove! alist key)
  (let loop ((l alist) (prev #f))
    (cond ((null? l) alist)
          ((equal? (caar l) key)
           (if prev (begin (set-cdr! prev (cdr l)) alist) (cdr l)))
          (else (loop (cdr l) l)))))

;;; Tree operations
(define-public (copy-tree obj)
  (cond ((pair? obj) (cons (copy-tree (car obj)) (copy-tree (cdr obj))))
        ((vector? obj) (vector-map copy-tree obj))
        (else obj)))

;;; Sorting (Guile argument order: (sort seq less?))
(define-public (sort a b)
  (let ((seq (if (procedure? a) b a))
        (pred (if (procedure? a) a b)))
    (if (vector? seq)
        (vector-sort pred seq)
        (chez:sort pred seq))))

;;; I/O primitives
;;
;; `write*' is not defined: nothing in the tree calls it (`display*' is the one
;; that is used, 293 times).
(define-public (display* . l)
  "Display all objects in @l."
  (for-each (top-level-value 'display) l))

(define-public (read-line . opt)
  (get-line (if (null? opt) (current-input-port) (car opt))))

(define-public (force-output . opt)
  (flush-output-port (if (null? opt) (current-output-port) (car opt))))

(define-public (make-soft-port . _) #f)

;;; Introspection
;; `procedure-name' lives in boot-chez.scm: kernel/texmacs/tm-define.scm:95
;; mutates it at top level with `(set! procedure-name ...)', which requires a
;; mutable interaction-environment binding rather than an R6RS library export.
(define-public (procedure-documentation p) #f)

;;; POSIX compatibility
(define-public (getpid)
  (cond ((top-level-bound? 'get-process-id) (get-process-id))
        (else 1000)))

(define-public (current-time)
  (time-second (chez:current-time)))

(define-public (getpwnam id)
  (vector (or (getenv "USER") "user") "" 1000 1000
          (or (getenv "USER") "Default User")
          (or (getenv "HOME") "") "/bin/sh"))

(define-public (getpwuid id) (getpwnam id))

;; Guile provides getlogin; Chez does not.  On Windows "USER" is usually
;; unset and the account name lives in USERNAME.  db-users.scm calls this
;; unconditionally on every platform except macOS, so without the shim
;; loading gpg-base.scm aborts with "variable getlogin is not bound".
(define-public (getlogin)
  (or (getenv "USER") (getenv "USERNAME") (getenv "LOGNAME") "user"))

(define-public (passwd:name pw)
  (if (vector? pw) (vector-ref pw 0) (or (getenv "USER") "Default User")))

(define-public (passwd:gecos pw)
  (if (vector? pw) (vector-ref pw 4) ""))

(define-public (gecos:name g)
  (if (string? g)
      (let ((parts (string-split g #\,)))
        (if (null? parts) "" (car parts)))
      "Default User"))

;;; String and character set compatibility
(define-public (char-in-set? c cs)
  (cond ((char? cs) (char=? c cs))
        ((pair? cs) (and (memv c cs) #t))
        ((string? cs)
         (let ((len (string-length cs)))
           (let loop ((i 0))
             (cond ((>= i len) #f)
                   ((char=? (string-ref cs i) c) #t)
                   (else (loop (+ i 1)))))))
        ((procedure? cs) (and (cs c) #t))
        (else #f)))

(define (string-index-char s ch start end)
  (let loop ((i start))
    (cond ((>= i end) #f)
          ((char=? (string-ref s i) ch) i)
          (else (loop (+ i 1))))))

(define (string-rindex-char s ch start end)
  (let loop ((i (- end 1)))
    (cond ((< i start) #f)
          ((char=? (string-ref s i) ch) i)
          (else (loop (- i 1))))))

(define (string-split-char s ch len)
  (let loop ((i 0) (start 0) (acc '()))
    (cond ((>= i len)
           (if (and (= start 0) (null? acc))
               (list s)
               (reverse (cons (substring s start len) acc))))
          ((char=? (string-ref s i) ch)
           (loop (+ i 1) (+ i 1) (cons (substring s start i) acc)))
          (else
           (loop (+ i 1) start acc)))))

(define (string-count-char s ch start end)
  (let loop ((i start) (cnt 0))
    (cond ((>= i end) cnt)
          ((char=? (string-ref s i) ch) (loop (+ i 1) (+ cnt 1)))
          (else (loop (+ i 1) cnt)))))

(define-public (string-index s cs . opt)
  (let* ((start (if (null? opt) 0 (car opt)))
         (end (if (or (null? opt) (null? (cdr opt))) (string-length s) (cadr opt))))
    (cond
      ((char? cs)
       (string-index-char s cs start end))
      ((and (string? cs) (= (string-length cs) 1))
       (string-index-char s (string-ref cs 0) start end))
      (else
       (let loop ((i start))
         (cond ((>= i end) #f)
               ((char-in-set? (string-ref s i) cs) i)
               (else (loop (+ i 1)))))))))

(define-public (string-rindex s cs . opt)
  (let* ((start (if (null? opt) 0 (car opt)))
         (end (if (or (null? opt) (null? (cdr opt))) (string-length s) (cadr opt))))
    (cond
      ((char? cs)
       (string-rindex-char s cs start end))
      ((and (string? cs) (= (string-length cs) 1))
       (string-rindex-char s (string-ref cs 0) start end))
      (else
       (let loop ((i (- end 1)))
         (cond ((< i start) #f)
               ((char-in-set? (string-ref s i) cs) i)
               (else (loop (- i 1)))))))))

(define-public (string-split s cs)
  (let ((len (string-length s)))
    (cond
      ((char? cs)
       (string-split-char s cs len))
      ((and (string? cs) (= (string-length cs) 1))
       (string-split-char s (string-ref cs 0) len))
      (else
       (let loop ((i 0) (start 0) (acc '()))
         (cond ((>= i len)
                (if (and (= start 0) (null? acc))
                    (list s)
                    (reverse (cons (substring s start len) acc))))
               ((char-in-set? (string-ref s i) cs)
                (loop (+ i 1) (+ i 1) (cons (substring s start i) acc)))
               (else
                (loop (+ i 1) start acc))))))))

(define-public (string-count s cs . opt)
  (let* ((start (if (null? opt) 0 (car opt)))
         (end (if (or (null? opt) (null? (cdr opt))) (string-length s) (cadr opt))))
    (cond
      ((char? cs)
       (string-count-char s cs start end))
      ((and (string? cs) (= (string-length cs) 1))
       (string-count-char s (string-ref cs 0) start end))
      (else
       (let loop ((i start) (cnt 0))
         (cond ((>= i end) cnt)
               ((char-in-set? (string-ref s i) cs) (loop (+ i 1) (+ cnt 1)))
               (else (loop (+ i 1) cnt))))))))

(define-public (string-null? s)
  (string=? s ""))

(define-public (string-prefix? prefix str)
  (and (string? prefix) (string? str)
       (let ((n (string-length prefix)))
         (and (<= n (string-length str))
              (let loop ((i 0))
                (or (= i n)
                    (and (char=? (string-ref prefix i) (string-ref str i))
                         (loop (+ i 1)))))))))

(define-public (string-suffix? suffix str)
  (and (string? suffix) (string? str)
       (let ((n (string-length suffix))
             (m (string-length str)))
         (and (<= n m)
              (let ((offset (- m n)))
                (let loop ((i 0))
                  (or (= i n)
                      (and (char=? (string-ref suffix i) (string-ref str (+ offset i)))
                           (loop (+ i 1))))))))))

;;; String searching
;;
;; These used to be glue procedures over the C++ `search_forwards` and
;; `search_backwards`, which count UTF-8 bytes. Chez strings count characters,
;; so the two disagree as soon as a payload contains a character above U+00FF:
;; the byte-oriented FFI bridge turns one UTF-8 byte into one Scheme character
;; and truncates anything above U+00FF to its low byte. Searching the Chez
;; string directly keeps the returned index in the units that `string-ref` and
;; `substring` expect. The C++ glue installs these two names only for Guile
;; (`#ifndef TEXMACS_ENGINE_CHEZ` in `src/Scheme/Glue/glue_basic.cpp`), where
;; byte offsets are what strings index; `compile-texmacs.scm` does not shadow
;; them, so the compiler and the editor share this implementation.

(define (string-match-at? s in i k)
  (let loop ((j 0))
    (or (= j k)
        (and (char=? (string-ref s j) (string-ref in (+ i j)))
             (loop (+ j 1))))))

(define-public (string-search-forwards s pos in)
  "Search for @s in @in from character index @pos; return its index or -1."
  (let ((k (string-length s)))
    (if (zero? k)
        pos
        (let ((n (string-length in)))
          (let loop ((i (max pos 0)))
            (cond ((> (+ i k) n) -1)
                  ((string-match-at? s in i k) i)
                  (else (loop (+ i 1)))))))))

(define-public (string-search-backwards s pos in)
  "Search backwards for @s in @in from character index @pos; return its index or -1."
  (let ((k (string-length s))
        (n (string-length in)))
    (let loop ((i (min pos (- n k))))
      (cond ((< i 0) -1)
            ((string-match-at? s in i k) i)
            (else (loop (- i 1)))))))

(define-public (sanitize-escapes str)
  (cond ((not (string? str)) str)
        ((string=? str "\\") "\\x5C;")
        ((string=? str "|")  "\\x7C;")
        ((not (string-index str #\\)) str)
        (else
         (let ((len (string-length str))
               (out (open-output-string)))
           (let loop ((i 0) (in-string? #f))
             (if (>= i len)
                 (get-output-string out)
                 (let ((c (string-ref str i)))
                   (cond
                     ((char=? c #\")
                      (write-char c out)
                      (loop (+ i 1) (not in-string?)))
                     ((and in-string? (char=? c #\\) (< (+ i 1) len))
                      (let ((next (string-ref str (+ i 1))))
                        (when (memv next '(#\a #\b #\t #\n #\v #\f #\r #\" #\\ #\x #\newline #\space))
                          (write-char #\\ out))
                        (write-char next out)
                        (loop (+ i 2) #t)))
                     (else
                      (write-char c out)
                      (loop (+ i 1) in-string?))))))))))

;;; Hash tables (Guile compatibility)
(define-public hash-ref
  (case-lambda
    ((t k) (hashtable-ref t k #f))
    ((t k default)
     (if (hashtable-contains? t k)
         (hashtable-ref t k #f)
         default))))

(define-public (hash-set! t k v)
  (hashtable-set! t k v)
  v)

(define-public (hash-remove! t k)
  (hashtable-delete! t k))

(define-public (hash-get-handle t k)
  (if (hashtable-contains? t k)
      (cons k (hashtable-ref t k #f))
      #f))

(define-public (hash-fold proc init t)
  (let-values (((ks vs) (hashtable-entries t)))
    (let loop ((i 0) (acc init))
      (if (= i (vector-length ks))
          acc
          (loop (+ i 1) (proc (vector-ref ks i) (vector-ref vs i) acc))))))

(define-public (hash-map->list proc t)
  (let-values (((ks vs) (hashtable-entries t)))
    (let loop ((i (- (vector-length ks) 1)) (acc '()))
      (if (< i 0)
          acc
          (loop (- i 1) (cons (proc (vector-ref ks i) (vector-ref vs i)) acc))))))

;;; Functions
(define-public (non pred?)
  "Return the negation of @pred?."
  (lambda args (not (apply pred? args))))

;;; Keyword compatibility
;;
;; `string->keyword' had no callers (the tree only ever goes the other way,
;; keyword -> string/number).
(define-public (keyword->string x)
  (let ((s (symbol->string x)))
    (substring s 1 (string-length s))))
(define-public (keyword->number x)
  (let ((s (symbol->string x)))
    (string->number (substring s 2 (string-length s)))))
(define-public (number->keyword x)
  (string->symbol (string-append ":%" (number->string x))))

;;; Output capture
;;
;; `display' and `write' are rebound by boot-chez.scm so that single-object
;; output is routed through the C++ `tm-output' hook, and `tm-output' is itself
;; replaced by the glue.  In R6RS library mode, `chez:display' and `chez:write'
;; are bound via `import' `rename' from `(chezscheme)` so capture always writes
;; into the port we hand it.

(define-public (display-to-string x)
  "Display @x into a fresh string."
  (call-with-string-output-port (lambda (p) (chez:display x p))))

(define-public (object->string x)
  "Write @x into a fresh string."
  (call-with-string-output-port (lambda (p) (chez:write x p))))

;;; Ports and files (Guile argument order)
(define-public OPEN_READ "r")
(define-public OPEN_WRITE "w")

(define-public (open-file file mode)
  (cond ((or (string=? mode "r") (string=? mode "rb"))
         (open-input-file file))
        ((or (string=? mode "w") (string=? mode "wb"))
         (open-output-file file 'replace))
        (else (open-input-file file))))

(define-public (call-with-input-string str proc)
  (proc (open-input-string str)))

(define-public (call-with-output-string proc)
  (let ((p (open-output-string)))
    (proc p)
    (get-output-string p)))

;;; List helpers
;;
;; `filter' and `last-pair' are not defined here: `(chezscheme)' exports both
;; with identical semantics (proper list only, error on `()'), so a shim would
;; only shadow a builtin with itself.  `kernel/boot/compat.scm' still guards
;; `filter' with `(defined? 'filter)', which the builtin satisfies.
(define-public (map-in-order f . ls)
  (if (or (null? ls) (null? (car ls))) '()
      (cons (apply f (map car ls))
            (apply map-in-order f (map cdr ls)))))

(define-public (symbol-append . syms)
  (string->symbol
   (apply string-append (map symbol->string syms))))

;;; Character sets represented as lists
(define-public (string->char-set str)
  (let loop ((l (string->list str)) (acc '()))
    (cond ((null? l) (reverse acc))
          ((memv (car l) acc) (loop (cdr l) acc))
          (else (loop (cdr l) (cons (car l) acc))))))

(define-public (char-set-adjoin cs . chars)
  (let loop ((chars chars) (res (if (list? cs) cs (list cs))))
    (cond ((null? chars) res)
          ((memv (car chars) res) (loop (cdr chars) res))
          (else (loop (cdr chars) (cons (car chars) res))))))

;;; Process and error reporting
(define-public (server-mode?)
  (if (top-level-bound? 'server-started?)
      ((top-level-value 'server-started?))
      #f))

(define-public (scm-error key subr message args rest)
  (if (and (string? message) (list? args))
      (apply error subr message args)
      (error subr (format "~a: ~a" key message))))

;;; Random numbers and hashing
(define-public (random n . state)
  (chez-sys-random n))

(define-public (seed->random-state . s)
  (if (null? s) 1 (car s)))

;; Guile builtin: hash value of obj bounded by n.  Only used for hash tables,
;; so any deterministic well-distributed value works.
(define-public (hash obj n)
  (if (= n 0) 0
      (modulo (abs (equal-hash obj)) n)))

;;; Records addressed by field name (Guile extension)
;;
;; `record-accessor' is used by convert/tools/tmcolor.scm and old-tmtable.scm
;; (8 calls) with a symbol field name.  The Guile mutator spelling
;; `record-modifier' had no callers -- Chez calls it `record-mutator' -- so it
;; is not shimmed.
(define-public (record-accessor rtd field)
  (if (integer? field)
      (old-record-accessor rtd field)
      (let* ((fields (record-type-field-names rtd))
             (idx (let loop ((i 0))
                    (cond ((>= i (vector-length fields)) #f)
                          ((eq? (vector-ref fields i) field) i)
                          (else (loop (+ i 1)))))))
        (if idx
            (old-record-accessor rtd idx)
            (error 'record-accessor "invalid field specifier" field)))))

;;; Macro-expansion helpers
;;
;; These are the classical Lisp destructuring helpers that the TeXmacs macro
;; layer (kernel/texmacs/tm-define.scm, kernel/gui/menu-convert.scm) calls while
;; expanding a form, so they are needed at compile time as well as at run time.
;; tm-define.scm defines its own private copies; the ones below exist because
;; consumers such as menu-convert.scm reach them through the top-level binding
;; rather than through an import.  `lambda*' is deliberately absent: tm-define
;; exports it, and a second export would make the kernel aggregate ambiguous.
(define-public (ca*r x) (if (pair? x) (ca*r (car x)) x))
(define-public (ca*adr x) (ca*r (cadr x)))

(define-public (apply* fun head)
  (cond ((list? head)
         `(,(apply* fun (car head)) ,@(cdr head)))
        ((pair? head)
         `(apply ,(apply* fun (car head)) (cons* ,@(listify (cdr head)))))
        (else fun)))

(define-public (listify args)
  (if (pair? args)
      (cons (car args) (listify (cdr args)))
      (list args)))

(define-public (and* conds)
  (if (and (pair? conds) (null? (cdr conds))) (car conds) `(and ,@conds)))

(define-public (begin* conds)
  (if (and (pair? conds) (null? (cdr conds))) (car conds) `(begin ,@conds)))

(define-public (unlambda pred?)
  (if (and (pair? pred?) (eq? (car pred?) 'lambda))
      (caddr pred?)
      (list pred?)))

;;; Dummy definitions
;;
;; `fresh-tm-dummy' hands out a fresh `_tm_def_<n>' symbol when the transpiler
;; has to name a definition whose name is not known statically.
(define *tm-dummy-counter* 0)

(define-public (fresh-tm-dummy)
  (set! *tm-dummy-counter* (+ *tm-dummy-counter* 1))
  (string->symbol (string-append "_tm_def_" (number->string *tm-dummy-counter*))))

;;; Initialization hooks
;;
;; `quit-TeXmacs-scheme' itself stays in boot-chez.scm: the C++ server calls it
;; by name (src/Texmacs/Server/tm_server.cpp, tm_debug.cpp), so the cell it
;; mutates is part of the bootstrap contract.
(define-public-macro (on-entry . cmd)
  `(begin ,@cmd))

(define-public-macro (on-exit . cmd)
  `(let ((old-quit (top-level-value 'quit-TeXmacs-scheme)))
     (set-top-level-value! 'quit-TeXmacs-scheme
       (lambda () ,@cmd (old-quit)))))

;;; SRFI-8 binding syntax
;;
;; Provided here so that expand-time macro transformers (such as `lazy-define`)
;; can use `receive` in `(compilation-environment)`.
(define-public-macro (receive vars vals . body)
  `(call-with-values (lambda () ,vals)
     (lambda ,vars ,@body)))

;;; Core control-flow & exception shims (also in boot-chez.scm for pre-module bootstrap)
(define-public noop
  (case-lambda
    (() #f)
    ((x) x)
    ((x . _) x)))

(define-public (defined? sym)
  (top-level-bound? sym))

(define-public (tm-catch-runtime key handler thunk)
  (guard (e (else
             (let* ((msg (if (condition? e) (condition-message e) e))
                    (irr (if (and (condition? e) (condition-irritants e))
                             (condition-irritants e)
                             '()))
                    (payload (list key #f msg irr '())))
               (if (procedure? handler) (apply handler payload) #f))))
    (thunk)))

(export catch lazy-catch with while)

(define-syntax catch
  (syntax-rules ()
    ((_ key thunk handler) (tm-catch-runtime key handler thunk))))

(define-syntax lazy-catch
  (syntax-rules ()
    ((_ key thunk handler) (tm-catch-runtime key handler thunk))))

(define-public (throw key . args)
  (raise (condition
           (make-error)
           (make-message-condition
             (if (and (pair? args) (string? (car args)))
                 (car args)
                 (format "~s" (cons key args))))
           (make-irritants-condition args))))

(define-syntax with
  (lambda (x)
    (syntax-case x ()
      ((_ var val body ...)
       (let ((v (syntax->datum #'var)))
         (if (and (or (pair? v) (null? v))
                  (not (and (pair? v)
                            (symbol? (car v))
                            (let ((s (symbol->string (car v))))
                              (and (> (string-length s) 0)
                                   (char=? (string-ref s 0) #\:))))))
             #'(apply (lambda var body ...) val)
             #'(let ((var val)) body ...)))))))

(define-syntax while
  (syntax-rules ()
    ((_ test body ...)
     (let loop ()
       (when test
         body ...
         (loop))))))

;;; Guile boot placeholders
;;
;; `temp-module' / `temp-value' were only read by kernel/boot/boot.scm, the Guile
;; bootstrap that is not loaded under Chez, so they are gone.
;;
;; The `guile-a?' / `guile-b?' / `guile-c?' / `guile-b-c?' predicates are NOT
;; here: the transpiler evaluates them as compile-time `if' tests while it walks
;; the sources, i.e. before any module is loaded, so they stay in boot-chez.scm.
