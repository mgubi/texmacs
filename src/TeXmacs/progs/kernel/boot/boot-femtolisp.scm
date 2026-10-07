
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : boot-femtolisp.scm
;; DESCRIPTION : some global variables, public macros, on-entry, on-exit and
;;               the TeXmacs module system, on femtolisp
;; COPYRIGHT   : (C) 2026  Massimiliano Gubinelli
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Loaded by init-femtolisp.scm after r5rs-femtolisp.scm, as plain global
;; definitions.

(define (femtolisp-scheme?) #t)
(define (s7-scheme?) #f)
(define (guile-a?) #f)
(define (guile-b?) #f)
(define (guile-c?) #f)
(define (guile-b-c?) #f)
(define has-look-and-feel? (lambda (x) (== x "emacs")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Redirect standard output
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define original-display display)
(define original-write write)

(define (display . l)
  "display one object on the standard output or a specified port."
  (if (or (null? l) (not (null? (cdr l))))
      (apply original-display l)
      (tm-output (display-to-string (car l)))))

(define (write . l)
  "write an object to the standard output or a specified port."
  (if (or (null? l) (not (null? (cdr l))))
      (apply original-write l)
      (tm-output (object->string (car l)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Modules
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Femtolisp has one global environment. A TeXmacs module gets its private
;; names in it under names of its own: the private definitions of a module,
;; its plain (define name ...) and (define-macro (name ...) ...), become the
;; global variables and macros name@module. When a module file is loaded, its
;; forms are read and scanned for these definitions first; then each form is
;; expanded and compiled with *current-module* set to the module, and the
;; hook resolve-global of femtolisp (see Scheme/Femtolisp/patches) replaces
;; the private names by their global names. The public definitions
;; (define-public, tm-define...) are global under their own names, as in the
;; user module of Guile, where the other modules find them.

;; a module: #(module name privates), where privates maps each private name
;; to its global name; the user module is #f
(define *modules* (table))
(define *current-module* #f)
(define *module-name* '(texmacs-user))
(define *texmacs-user-module* 'texmacs-user)
(define texmacs-user 'texmacs-user)
(define temp-module #f)
(define temp-value #f)

(define (%make-module name) (vector 'module name (table)))
(define (%module? x)
  (and (vector? x) (= (length x) 3) (eq? (aref x 0) 'module)))
(define (%module-privates m) (aref m 2))
(define (%module-of x) (if (%module? x) x #f))

(define (%module-path-string name)
  (apply string-append
         (cons (symbol->string (car name))
               (map (lambda (s) (string-append "/" (symbol->string s)))
                    (cdr name)))))

(define (%module-global-name m s)
  (symbol (string-append (symbol->string s) "@"
                         (%module-path-string (aref m 1)))))

(define (%module-declare-private! m s)
  (if (not (has? (%module-privates m) s))
      (put! (%module-privates m) s (%module-global-name m s))))

(set! resolve-global
      (lambda (s)
        (let ((m *current-module*))
          (if m (get (aref m 2) s s) s))))

(define (current-module) (or *current-module* texmacs-user))
(define (set-current-module m) (set! *current-module* (%module-of m)))
(define (module-name m) (if (%module? m) (aref m 1) '(texmacs-user)))

;; the names defined privately by a list of forms read from a file
(define %public-definers
  '(define-public define-public-macro provide-public tm-define
    tm-define-macro tm-menu tm-widget menu-bind tm-property))

(define (%defined-name head)
  (if (pair? head) (%defined-name (car head)) head))

(define *file-definitions* #f)

(define (%scan-definitions forms)
  (let ((private '()) (public '()) (public-here '()) (macros '()))
    (define (scan x)
      (if (pair? x)
          (let ((h (car x)))
            (cond ((and (memq h '(define define-macro)) (pair? (cdr x)))
                   (let ((n (%defined-name (cadr x))))
                     (if (symbol? n) (set! private (cons n private)))
                     (if (and (symbol? n) (eq? h 'define-macro))
                         (set! macros (cons n macros)))))
                  ((and (memq h %public-definers) (pair? (cdr x)))
                   (let ((n (%defined-name (cadr x))))
                     (if (symbol? n) (set! public (cons n public)))
                     (if (and (symbol? n)
                              (memq h '(define-public define-public-macro
                                        provide-public)))
                         (set! public-here (cons n public-here)))
                     (if (and (symbol? n)
                              (memq h '(define-public-macro tm-define-macro)))
                         (set! macros (cons n macros)))))
                  ((memq h '(export re-export))
                   (set! public (append (filter symbol? (cdr x)) public))
                   (set! public-here
                         (append (filter symbol? (cdr x)) public-here)))
                  ((eq? h 'begin) (for-each scan (cdr x)))
                  ((and (memq h '(if when unless)) (pair? (cdr x)))
                   (for-each scan (cddr x)))
                  ((eq? h 'cond)
                   (for-each (lambda (c) (if (pair? c) (for-each scan (cdr c))))
                             (cdr x)))))))
    (for-each scan forms)
    ;; the functions of the file are compiled as plain calls, not its macros
    ;; (a macro used before its definition is compiled at its first call)
    (if *file-definitions*
        (for-each (lambda (n)
                    (if (not (memq n macros)) (put! *file-definitions* n #t)))
                  (append private public)))
    ;; a name defined by tm-define (globally, under its quoted name) and by a
    ;; plain define stays private in the module, as in Guile, where the
    ;; binding of the module hides the one of the user module
    (filter (lambda (n) (not (memq n public-here))) private)))

(define (%read-forms file)
  (if %profile?
      (let* ((t0 (time.now)) (r (%read-forms-sub file)))
        (set! %profile-read (+ %profile-read (- (time.now) t0)))
        r)
      (%read-forms-sub file)))

(define (%read-forms-sub file)
  (call-with-input-file file
    (lambda (p)
      (let loop ((acc '()))
        (let ((x (read p)))
          (if (eof-object? x) (reverse! acc) (loop (cons x acc))))))))

(define-override (eval x . env)
  (if (null? env)
      (%fl-eval x)
      (let ((m (%module-of (car env))))
        (with-bindings ((*current-module* m)
                        (*module-name* (module-name m)))
          (%fl-eval x)))))

(define (tm-eval x)
  (with-bindings ((*current-module* #f)
                  (*module-name* '(texmacs-user)))
    (%fl-eval x)))

(define (primitive-eval x) (eval x))

;; TEXMACS_FL_PROFILE: the time spent reading, expanding and compiling (or
;; finding in the cache, of which reading the cache) the forms of the loaded
;; files, (%profile-report); the expansion time by head of the top-level
;; forms, (%profile-heads-report)
(define %profile? (os.getenv "TEXMACS_FL_PROFILE"))
(define %profile-read 0.0)
(define %profile-expand 0.0)
(define %profile-compile 0.0)
(define %profile-cache-read 0.0)
(define %profile-forms 0)
(define %profile-hits 0)
(define %profile-heads (table))

(define (%profile-report)
  (display* "PROFILE forms " %profile-forms
            " cache hits " %profile-hits
            " read " (round (* 1000 %profile-read)) " ms"
            " expand " (round (* 1000 %profile-expand)) " ms"
            " compile or cache " (round (* 1000 %profile-compile)) " ms"
            " (reading the cache " (round (* 1000 %profile-cache-read))
            " ms)\n"))

(define (%profile-head! form dt)
  (let* ((h (if (pair? form) (car form) 'atom))
         (old (get %profile-heads h (cons 0 0.0))))
    (put! %profile-heads h (cons (+ (car old) 1) (+ (cdr old) dt)))))

(define (%profile-heads-report)
  (let ((l (table.foldl (lambda (k v acc) (cons (cons k v) acc))
                        '() %profile-heads)))
    (for-each (lambda (p)
                (display* "HEAD " (car p) " forms " (cadr p)
                          " expand " (round (* 1000 (cddr p))) " ms\n"))
              (list-head (sort l (lambda (a b) (> (cddr a) (cddr b))))
                         (min 25 (length l))))))

;; Femtolisp expands the macros when it compiles a form, and Guile when it
;; first evaluates it: TeXmacs code may use a macro which is defined after the
;; code (in a module loaded later). With lazy function bodies (below), a body
;; is compiled when it first runs, which is enough; with TEXMACS_FL_EAGER,
;; late calls are needed. A call (f ...) of a name f which is
;; neither bound nor a macro when it is compiled, nor defined by the file
;; being loaded, is compiled when it is first evaluated, as the function of
;; the local variables in its scope (#(form variables module function)).
;; With TEXMACS_FL_TRACE set, (%late-calls) lists them.
(define %late-sites '())

(define (%env-variables env)
  (let ((seen '()))
    (for-each (lambda (frame)
                (if (pair? frame)
                    (for-each (lambda (v)
                                (if (and (symbol? v) (not (memq v seen)))
                                    (set! seen (cons v seen))))
                              frame)))
              env)
    (reverse! seen)))

;; (when a site is compiled, the names still unknown are plain calls, which
;; raise an unbound variable error)
(define *compiling-late-site* #f)

(define (%late-call site vals)
  (let ((f (aref site 3)))
    (if (not f)
        (begin
          (set! f (with-bindings ((*current-module*
                                   (and (aref site 2)
                                        (get *modules* (aref site 2) #f)))
                                  (*compiling-late-site* #t))
                    (%fl-eval (list 'lambda (aref site 1) (aref site 0)))))
          (aset! site 3 f)))
    (apply f vals)))

(if (os.getenv "TEXMACS_FL_EAGER")
(set! compile-unknown-call
      (lambda (x env)
        (if (or *compiling-late-site*
                (and *file-definitions* (has? *file-definitions* (car x))))
            #f
            (let* ((vars (%env-variables env))
                   ;; the name of the module (the site can be written in the
                   ;; cache of compiled files)
                   (site (vector x vars (and *current-module*
                                             (module-name *current-module*))
                                 #f)))
              (if %trace-errors? (set! %late-sites (cons site %late-sites)))
              (list '%late-call (list 'quote site) (cons 'list vars)))))))

(define (%late-calls)
  (map (lambda (site) (car (aref site 0))) %late-sites))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Lazy function bodies
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Guile and s7 expand the body of a function when it first runs, femtolisp
;; when it compiles the function. A loaded form is therefore only expanded
;; at its top level (%expand-top): its macros run, but each lambda which is
;; not inside a binding form (a lambda whose free variables are global) is
;; kept unexpanded in a stub. The first call of the stub expands and compiles
;; the lambda, in the module where it was loaded, and the stub becomes the
;; compiled function (%function-become!), so that the references kept to it
;; (hooks, menus, former definitions...) call the compiled code. A stub has
;; the name and the source of its function (procedure-name,
;; procedure-source, procedure-arity). TEXMACS_FL_EAGER disables it.

(define %lazy? (not (os.getenv "TEXMACS_FL_EAGER")))
(define %lazy-made 0)
(define %lazy-forced 0)
(define %lazy-time 0.0)

(define (%make-lazy src name module)
  ;; src is (lambda formals . body), compiled with the name of the function
  (let* ((named (if name (append src name) src))
         (rec (vector named module #f #f))
         (tvals (function:vals %lazy-template))
         (n (length tvals))
         (vals (make-vector n #f)))
    (let loop ((i 0))
      (if (< i n)
          (let ((c (aref tvals i)))
            (aset! vals i
                   (cond ((eq? c '%lazy-record) rec)
                         ((and (pair? c) (eq? (car c) '%source))
                          (cons '%source src))
                         (else c)))
            (loop (+ i 1)))))
    (let ((stub (function (function:code %lazy-template) vals
                          (or name 'lambda))))
      (aset! rec 2 stub)
      (set! %lazy-made (+ %lazy-made 1))
      stub)))

(define (%lazy-force! rec)
  (let ((stub (aref rec 2)))
    (if (aref rec 3)
        stub
        (let* ((t0 (and %profile? (time.now)))
               (name (aref rec 1))
               (m (and name (get *modules* name #f)))
               (real (with-bindings ((*current-module* m)
                                     (*module-name* (or name '(texmacs-user))))
                       (%lazy-compile (aref rec 0) name))))
          ;; (a call of the stub during this compilation, from a macro, uses
          ;; this compilation's result, and the outer one becomes the stub)
          (if (not (aref rec 3))
              (begin
                (aset! rec 3 #t)
                (%function-become! stub (%with-source-of real stub))
                (set! %lazy-forced (+ %lazy-forced 1))
                (if %profile?
                    (set! %lazy-time (+ %lazy-time (- (time.now) t0))))))
          stub))))

;; f with the source of the stub g (the lambda before its expansion), as
;; the last constant: procedure-source gives it, and a function hashes as
;; its source (equal.c), so that the stub keeps its hash when it becomes f
(define (%with-source-of f g)
  (let* ((gv (function:vals g))
         (src (aref gv (- (length gv) 1)))
         (v (function:vals f))
         (n (length v))
         (has (and (> n 0) (pair? (aref v (- n 1)))
                   (eq? (car (aref v (- n 1))) '%source)))
         (w (make-vector (if has n (+ n 1)) #f)))
    (let loop ((i 0))
      (if (< i n) (begin (aset! w i (aref v i)) (loop (+ i 1)))))
    (aset! w (if has (- n 1) n) src)
    (function (function:code f) w (function:name f) (function:env f))))

;; The compiled bodies are kept in the cache of compiled files, in the file
;; %lazy.flc, under the fingerprint of their expansion, their module and its
;; private names: the bodies are still expanded at their first call (their
;; macros run, as in Guile), not compiled. The file starts with the key of
;; the cache; each compilation appends an entry, and the file is read at the
;; first call of a stub.
(define %lazy-table #f)
(define %module-privates-fp (table))
(define %lazy-hits 0)

(define (%lazy-cache-file) (%cache-file "%lazy"))

(define (%lazy-cache-shipped-file) (%cache-shipped-file "%lazy"))

;; the entries of the file cf into the table, if its key is the one of the
;; cache; #f when it is not a cache of this TeXmacs
(define (%lazy-table-read cf)
  (and cf (file-exists? cf)
       (trycatch
        (let* ((in (open-input-file cf))
               (ok (equal? (%cache-read-entry in) %cache-key)))
          (if ok
              (let loop ()
                (let ((e (%cache-read-entry in)))
                  (if (pair? e)
                      (begin (put! %lazy-table (car e) (cdr e))
                             (loop))))))
          (close-port in)
          ok)
        (lambda (e) #f))))

;; the shipped cache first, then the one of the home directory
(define (%lazy-table-load)
  (set! %lazy-table (table))
  (%lazy-table-read (%lazy-cache-shipped-file))
  (let ((cf (%lazy-cache-file)))
    (if (and cf (not (%lazy-table-read cf)))
        ;; a new file (or another version of TeXmacs): the key
        (trycatch
         (let ((out (file cf :write :create :truncate)))
           (%fl-write %cache-key out) (newline out)
           (io.close out))
         (lambda (e) #f)))))

(define (%lazy-cache-add! fp f)
  (put! %lazy-table fp f)
  (let ((cf (%lazy-cache-file)))
    (if cf
        (trycatch
         (let ((out (file cf :write :create :append)))
           (with-bindings ((*print-readably* #t) (*print-closures* #t)
                           (*print-shared* #t) (*print-pretty* #f)
                           (*print-length* #f) (*print-level* #f))
             (%fl-write (cons fp f) out) (newline out))
           (io.close out))
         (lambda (e) #f)))))

(define (%lazy-compile src name)
  (let* ((e (expand src))
         (fp (and %cache?
                  (%fingerprint (list name (get %module-privates-fp name #f)
                                      e))))
         (hit (and fp
                   (begin
                     (if (not %lazy-table) (%lazy-table-load))
                     (get %lazy-table fp #f)))))
    (if hit
        (begin (set! %lazy-hits (+ %lazy-hits 1)) hit)
        (let ((f ((compile-thunk e))))
          (if (and fp (%cache-writable? f)) (%lazy-cache-add! fp f))
          f))))

;; the code of the stubs, compiled after %lazy-force! (a direct call): a
;; record #(lambda module-name stub forced?) takes the place of the constant
;; %lazy-record
(define %lazy-template
  (%fl-eval '(lambda args (apply (%lazy-force! '%lazy-record) args))))

(define (%lazy-report)
  (display* "LAZY stubs " %lazy-made " forced " %lazy-forced
            " (cache hits " %lazy-hits ") compiling them "
            (round (* 1000 %lazy-time)) " ms\n"))

;; the lambda e (with a name or #f) as a stub
(define (%lazy-lambda e name)
  (list '%make-lazy (list 'quote e) (list 'quote name)
        (list 'quote (and *current-module* (module-name *current-module*)))))

;; expands the top level of the form e, keeping the lambdas in stubs
(define (%expand-top e)
  (if (atom? e) e
      (let ((head (car e)))
        (cond ((eq? head 'quote) e)
              ((eq? head 'lambda)
               (if (and (pair? (cdr e)) (pair? (cddr e)))
                   (let ((n (lastcdr e)))
                     (%lazy-lambda (list* 'lambda (cadr e)
                                          (proper-part (cddr e)))
                                   (and (symbol? n) n)))
                   e))
              ((eq? head 'define) (%expand-top-define e))
              ((eq? head 'let-syntax) (expand e))
              ((and (symbol? head) (not (bound? (resolve-global head)))
                    (macrocall? e))
               => (lambda (f) (%expand-top (expand-macro-call f e))))
              (else (%expand-args e))))))

(define (%expand-args e)
  (let loop ((e e))
    (if (atom? e) e
        (cons (if (atom? (car e)) (car e) (%expand-top (car e)))
              (loop (cdr e))))))

(define (%expand-top-define e)
  (let ((e (uncurry-define e)))
    (cond ((or (null? (cdr e)) (null? (cddr e))) e)
          ((atom? (cadr e))
           (list 'define (cadr e) (%expand-top (caddr e))))
          (else
           (let ((name (caadr e)))
             (list 'define name
                   (%lazy-lambda (list* 'lambda (cdadr e) (cddr e))
                                 name)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The cache of the compiled files
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Compiling the forms of the loaded files is half of the boot. The cache,
;; $TEXMACS_HOME_PATH/system/cache/femtolisp/, keeps for each file the
;; fingerprints of the expanded forms (%fingerprint: 128 bits of hash of the
;; structure) and their compiled code. The forms are still expanded at each
;; load (the expansions may have side effects), and the compiled code of a
;; form is reused when the fingerprint of its expansion is the one in the
;; cache. A cache file starts with a key (the compiler, the TeXmacs version
;; and the format of the cache) and the private names of the module, which
;; the compiled code depends on. The forms whose expansion or code holds
;; values which cannot be written and read back (uninterned symbols, tables,
;; procedures with an environment, TeXmacs objects...) are compiled at each
;; load. A file of TeXmacs is named by its path in $TEXMACS_PATH, and when the
;; home has no valid cache of it, the cache shipped with TeXmacs is read
;; ($TEXMACS_PATH/cache/femtolisp: the page in the browser has one, made by
;; its build). TEXMACS_FL_NO_CACHE disables the cache.

(define %cache-format (if %lazy? 4 2))  ; (lazy bodies: another cache)
(define %cache? (not (os.getenv "TEXMACS_FL_NO_CACHE")))
(define %cache-dir #f)
(define %cache-key #f)

(define %cache-tm-path #f)

(define (%cache-init!)
  (if (not %cache-dir)
      (let ((dir (url-concretize "$TEXMACS_HOME_PATH/system/cache/femtolisp")))
        (if (not (url-exists? dir)) (system-mkdir dir))
        (set! %cache-dir dir)
        (set! %cache-tm-path (string-append (url-concretize "$TEXMACS_PATH") "/"))
        (set! %cache-key (list *fl-boot-id* (texmacs-version) %cache-format)))))

;; the name of the cache of file: a file of TeXmacs by its path in
;; $TEXMACS_PATH (TM%progs%...), so that a cache made with another
;; $TEXMACS_PATH (the one shipped with the page in the browser) fits
(define (%cache-name file)
  (let* ((n (string-length %cache-tm-path))
         (rel (if (and (> (string-length file) n)
                       (string=? (substring file 0 n) %cache-tm-path))
                  (string-append "TM/" (substring file n (string-length file)))
                  file)))
    (string-append
     (list->string
      (map (lambda (c) (if (memv c '(#\/ #\\ #\: #\space)) #\% c))
           (string->list rel)))
     ".flc")))

(define (%cache-file file)
  (and %cache?
       (begin
         (%cache-init!)
         (string-append %cache-dir "/" (%cache-name file)))))

;; the cache shipped with TeXmacs ($TEXMACS_PATH/cache/femtolisp, made with
;; the program: the page in the browser has one), read when the cache of the
;; home directory has no valid file
(define (%cache-shipped-file file)
  (and %cache?
       (begin
         (%cache-init!)
         (string-append %cache-tm-path "cache/femtolisp/" (%cache-name file)))))

;; can x be written and read back as an equal value?
(define (%cache-writable? x)
  (let ((seen (table)))
    (let walk ((x x))
      (cond ((or (%fl-symbol? x) (number? x) (string? x) (char? x)
                 (null? x) (boolean? x) (builtin? x))
             (not (gensym? x)))
            ((pair? x)
             (or (has? seen x)
                 (begin (put! seen x #t) (and (walk (car x)) (walk (cdr x))))))
            ((vector? x)
             (or (has? seen x)
                 (begin (put! seen x #t) (every walk (vector->list x)))))
            ((function? x)
             (or (%builtin-name x)
                 (and (null? (function:env x))
                      (walk (function:vals x)))))
            (else #f)))))

(define (%cache-read-entry in)
  (trycatch (read in) (lambda (e) (eof-object))))

;; the entries of the cache of file, or #f
(define (%cache-open cf privates)
  (and cf (file-exists? cf)
       (trycatch
        (let* ((in (open-input-file cf))
               (key (read in))
               (privs (read in)))
          (if (and (equal? key %cache-key) (equal? privs privates))
              in
              (begin (close-port in) #f)))
        (lambda (e) #f))))

(define (%cache-write cf privates entries)
  (trycatch
   ;; (written aside, then moved: another TeXmacs may read or write it)
   (let ((tmp (string-append cf "." (number->string (getpid)) ".tmp")))
     (call-with-output-file tmp
       (lambda (out)
         (with-bindings ((*print-readably* #t) (*print-closures* #t)
                         (*print-shared* #t) (*print-pretty* #f)
                         (*print-length* #f) (*print-level* #f))
           (%fl-write %cache-key out) (newline out)
           (%fl-write privates out) (newline out)
           (for-each (lambda (e) (%fl-write e out) (newline out))
                     entries))))
     (system-move tmp cf))
   (lambda (e) #f)))

;; evaluates the forms of file, with the cache
(define (%eval-forms-cached file forms privates)
  (let* ((cf (%cache-file file))
         (in (or (%cache-open cf privates)
                 (%cache-open (%cache-shipped-file file) privates)))
         (dirty (not in))
         (entries '()))
    (let loop ((l forms))
      (if (pair? l)
          (let* ((t0 (and %profile? (time.now)))
                 (e (if %lazy? (%expand-top (car l)) (expand (car l))))
                 (t1 (and %profile? (time.now)))
                 (old (if in (%cache-read-entry in) (eof-object)))
                 (t2 (and %profile? (time.now)))
                 (fp (and cf (%fingerprint e)))
                 (hit (and fp (pair? old) (cdr old) (equal? (car old) fp)))
                 (thunk (if hit
                            (cdr old)
                            (begin (set! dirty #t) (compile-thunk e)))))
            (if %profile?
                (begin
                  (set! %profile-expand (+ %profile-expand (- t1 t0)))
                  (%profile-head! (car l) (- t1 t0))
                  (set! %profile-compile (+ %profile-compile
                                            (- (time.now) t1)))
                  (set! %profile-cache-read (+ %profile-cache-read
                                               (- t2 t1)))
                  (if hit (set! %profile-hits (+ %profile-hits 1)))
                  (set! %profile-forms (+ %profile-forms 1))))
            ;; (an entry read from the cache can be written back)
            (if cf
                (set! entries
                      (cons (cond (hit old)
                                  ((and fp (%cache-writable? thunk))
                                   (cons fp thunk))
                                  (else (list #f)))
                            entries)))
            (thunk)
            (loop (cdr l)))))
    (if in (close-port in))
    (if (and cf dirty) (%cache-write cf privates (reverse! entries)))))

;; loads a file; when its first form is (texmacs-module name ...), it is the
;; file of the module name
(define (%load-file file)
  (trycatch (%load-file-sub file)
            (lambda (e) (raise (list 'load-error file e)))))

(define (%load-file-sub file)
  (with-bindings ((*file-definitions* (table)))
    (%load-forms (%read-forms file) file)))

(define (%load-forms forms file)
  (begin
    (if (and (pair? forms) (pair? (car forms))
             (eq? (caar forms) 'texmacs-module) (pair? (cdar forms)))
        (let* ((name (cadar forms))
               (m (or (get *modules* name #f)
                      (let ((m (%make-module name)))
                        (put! *modules* name m)
                        m))))
          (let ((privates (%scan-definitions forms)))
            (for-each (lambda (s) (%module-declare-private! m s)) privates)
            (put! %module-privates-fp name (%fingerprint privates))
            (with-bindings ((*current-module* m)
                            (*module-name* name))
              (%eval-forms-cached file forms (cons name privates)))))
        (begin
          (%scan-definitions forms)
          (%eval-forms-cached file forms
                              (list (module-name *current-module*)))))))

(define-override (load file . env)
  (if (null? env)
      (%load-file file)
      (with-bindings ((*current-module* (%module-of (car env))))
        (%load-file file))))

(define (primitive-load file) (load file))

;; the files of the modules
(define *loaded-modules* (table))

(define (list->module module)
  (let ((u (url-unix "$GUILE_LOAD_PATH"
                     (string-append (%module-path-string module) ".scm"))))
    (url-materialize u "r")))

(define (module-available? module)
  (or (has? *modules* module)
      (let ((f (list->module module)))
        (and (string? f) (not (string-null? f))))))

(define (module-load module)
  (if (and (list? module) (not (has? *loaded-modules* module)))
      (begin
        (put! *loaded-modules* module #t)
        (let ((f (list->module module)))
          (if (or (not (string? f)) (string-null? f))
              (error "module-load: no file for module" module))
          (with-bindings ((*current-module* #f)
                          (*module-name* '(texmacs-user)))
            (%load-file f))
          (if (not (has? *modules* module))
              (put! *modules* module (%make-module module)))))))

(define (module-provide module) (module-load module))

(define (resolve-module module)
  (if (or (eq? module texmacs-user) (equal? module '(texmacs-user)))
      texmacs-user
      (begin
        (module-provide module)
        (get *modules* module #f))))
(define (resolve-interface module) (resolve-module module))
(define (module-public-interface m) m)

;; the value of sym in module m: its private definition or the global one
(define (module-ref m sym . default)
  (let ((g (if (%module? m) (get (%module-privates m) sym sym) sym)))
    (cond ((bound? g) (top-level-value g))
          ((pair? default) (car default))
          (else (error "module-ref: unbound variable" sym)))))
(define (module-defined? m sym)
  (bound? (if (%module? m) (get (%module-privates m) sym sym) sym)))
(define (module-symbols m)
  (if (%module? m) (table.keys (%module-privates m)) '()))

(define (defined? sym . env)
  (if (null? env)
      (bound? (resolve-global sym))
      (module-defined? (car env) sym)))

(define-macro (use-modules . modules)
  `(begin ,@(map (lambda (m) `(module-provide ',m)) modules) (noop)))
(define-macro (import-from . modules) `(use-modules ,@modules))
(define-macro (inherit-modules . modules) `(use-modules ,@modules))
(define-macro (export . symbols) '(noop))
(define-macro (re-export . symbols) '(noop))

(define-macro (texmacs-module name . options)
  (define (transform action)
    (cond ((not (pair? action)) '(noop))
          ((memq (car action) '(:use :inherit))
           (cons 'use-modules (cdr action)))
          ((eq? (car action) :export)
           '(begin
              (display "Warning] The option :export is no longer supported\n")
              (display "       ] Please use tm-define instead\n")))
          (else '(noop))))
  `(begin
     (%module-enter! ',name)
     ,@(map transform options)
     (noop)))

;; makes the module name the current one, as define-module in Guile (the
;; load of the file of a module also binds it, see %load-forms)
(define (%module-enter! name)
  (let ((m (or (get *modules* name #f)
               (let ((m (%make-module name)))
                 (put! *modules* name m)
                 m))))
    (put! *loaded-modules* name #t)
    (set! *current-module* m)
    (set! *module-name* name)))

;; evaluates the body in the module (as the top-level forms of its file)
(define-macro (with-module module . body)
  `(eval '(begin ,@body) ,module))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Definitions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Guile's (define-macro name transformer) besides (define-macro (name . args))
(define-macro (define-macro form . body)
  (if (symbol? form)
      `(set-syntax! ',form ,(car body))
      `(set-syntax! ',(car form) (lambda ,(cdr form) ,@body))))

;; defines the global variable s (also when s is a builtin of femtolisp), as
;; the definitions of tm-define
(define (define-global! s v)
  (if (constant? s) (%unconstant! s))
  (set-top-level-value! s v))

;; a public definition is a global one: the scan of the module file does not
;; make its name private (curried heads, ((f a) b), are expanded by define)
(define-macro (define-public head . body)
  (let ((name (%defined-name head)))
    (if (and (symbol? name) (constant? name)) (%unconstant! name))
    `(define ,head ,@body)))

(define-macro (define-public-macro head . body)
  `(define-macro ,head ,@body))

(define-macro (provide-public head . body)
  (if (or (and (symbol? head) (not (defined? head)))
          (and (pair? head) (symbol? (car head)) (not (defined? (car head)))))
      `(define-public ,head ,@body)
      '(noop)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; On-entry and on-exit macros
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (quit-TeXmacs-scheme) (noop))

(define-macro (on-entry . cmd)
  `(begin ,@cmd))

(define-macro (on-exit . cmd)
  `(set! quit-TeXmacs-scheme (lambda () ,@cmd (,quit-TeXmacs-scheme))))
