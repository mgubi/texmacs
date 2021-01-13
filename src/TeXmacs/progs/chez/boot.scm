;        (only (srfi 142) bitwise-and bitwise-ior)
;        (srfi 27) ;; random numbers
;        (srfi 26) ;; cut, cute
;        (srfi 18) ;; current-time


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; curried define
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(module curried-define-module (curried-define)
  (import (rename scheme (define primitive-define)))
  (define-syntax curried-define
    (syntax-rules ()
      ((_ ((x a1 ...) a ...) body ...) (curried-define (x a1 ...) (lambda (a ...) body ...)))
      ((_ x body ...) (primitive-define x body ...)))))
    
(import (rename curried-define-module (curried-define define)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Defmacros
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; See https://gist.github.com/yuhr/bba4de4b34db940dcfba2b756f6cbc32

(define-syntax macro
  (syntax-rules ()
    ((k transformer)
       (lambda (stx)
         (let ((r
         (syntax-case stx ()
           ((l . sv)
            (let* ((v (syntax->datum (syntax sv)))
                   (e (apply transformer v)))
              (if (eq? (void) e)
                  (syntax (void))
                  (datum->syntax (syntax l) e)))))))
                    r)))))

(define-syntax define-macro
  (syntax-rules ()
    ((k (name . args) body ...)
     (define-macro name (lambda args body ...)))
    ((k name transformer)
     (define-syntax name (macro transformer)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Misc
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (noop . args) (and (pair? args) (car args)))


;(define list? proper-list?)

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

;; setup the module system

;; we assume *texmacs-user-module* is the current top-level environment
;;(define *texmacs-user-module* (curlet))

(module (*current-module* *modules*)
  (define *current-module* *texmacs-user-module*)
  (define *modules* (make-hashtable equal-hash equal?))
)

(define *module-name* '(texmacs-user))
(define *exports* '())
(define *tm-defines* '())

(hashtable-set! *modules* '(texmacs-user) *texmacs-user-module*)

(define (current-module) *current-module*)

(define-macro (tm-export . symbols)
    `(set! *exports* (append ',symbols *exports*)))

(define-macro (define-public head . body)
    `(begin
        (define ,head ,@body)
        (tm-export ,(if (pair? head) (car head) head))))
                
(define-macro (provide-public head . body)
  (if (or (and (symbol? head) (not (defined? head)))
      (and (pair? head) (symbol? (car head)) (not (defined? (car head)))))
      `(define-public ,head ,@body)
      '(noop)))

(define-macro (define-public-macro head . body)
    `(begin
       (define-macro ,head ,@body)
       (tm-export ,(if (pair? head) (car head) head))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Module handling
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-syntax tm-declare (lambda (x)
   (syntax-case x ()
     [(_ var) (identifier? #'var)
        (with-syntax ([xvar (datum->syntax #'var
          (string->symbol (string-append (symbol->string (syntax->datum #'var)) "$global")))]
                [*current-module* (datum->syntax #'var '*current-module*)]
                [*exports* (datum->syntax #'var '*exports*)])
          #'(begin
              (define-syntax var
               (identifier-syntax (var (top-level-value 'xvar *texmacs-user-module*))
                  ((set! var expr) (set-top-level-value! 'xvar expr *texmacs-user-module*))))
               (define-top-level-syntax 'var
               (identifier-syntax (var (top-level-value 'xvar *texmacs-user-module*))
                  ((set! var expr) (set-top-level-value! 'xvar expr *texmacs-user-module*))) *current-module*)
               (if (not (eq? *current-module* *texmacs-user-module*))
                  (define-top-level-syntax 'var (top-level-syntax 'var *current-module*) *texmacs-user-module*))
              (set! *exports* (cons 'var *exports*))
             ))]))) 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Module handling
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (module-available? module)
  (if (hashtable-ref *modules* module #f) #t #f))

(define (list->module module)
  (let* ((aux (lambda (s) (string-append "/" (symbol->string s))))
     (name* (apply string-append (map aux module)))
     (name (substring name* 1 (string-length name*)))
     (u (url-unix "$GUILE_LOAD_PATH" (string-append name ".scm")))
     ;; FIXME: should use %load-path instead of $GUILE_LOAD_PATH
     )
    (url-materialize u "r")))

(define (module-load module)
  (if (list? module)
      (let ((module-file (list->module module))
             (loaded (hashtable-ref *modules* module #f))
             (uname (gensym))
             (env (copy-environment *texmacs-user-module*)))
    (when (not loaded)
        (fluid-let ((*current-module* env))
          (display "TeXmacs] Loading module ") (display module) (newline)
          (load module-file (lambda (e) (eval e *current-module*)))
          (display "TeXmacs] Loaded module ") (display module) (newline)
     )))))

(define (module-provide module)
  (if (not (module-available? module)) (module-load module)))

(define (resolve-module module)
  (module-provide module)
  (hashtable-ref *modules* module #f))

(define (module-exports which)
   (let* ((m  (resolve-module which))
          (ex (and m (top-level-value '*exports* m))))
          (if ex ex '())))

(define (module-tm-defines which)
   (let* ((m  (resolve-module which))
          (ex (and m (top-level-value '*tm-defines* m))))
          (if ex ex '())))

(define-macro (use-modules . modules)
  `(map (lambda (module) (let ((m (resolve-module module)))
     ; export bindings
      (map (lambda (symb)
        (if (top-level-bound? symb m)
          (define-top-level-value symb
            (top-level-value symb m) *current-module*)
          (if (top-level-syntax? symb m)
            (define-top-level-syntax symb
              (top-level-syntax symb m) *current-module*))))
        (module-exports module))
       (map (lambda (symb)
        (set! *tm-defines* (cons symb *tm-defines*))
;        (if (top-level-bound? symb m)
;          (define-top-level-value symb
;            (top-level-value symb *texmacs-user-module*) *current-module*)
 ;         (if (top-level-syntax? symb m)
            (define-top-level-syntax symb
              (top-level-syntax symb m) *current-module*))
              ;))
        (module-tm-defines module))
       )) ',modules))

(define-macro (import-from . modules)
  `(use-modules ,@modules))

(define-macro (re-export . symbols)
  `(tm-export ,@symbols))

(define-macro (inherit-modules . which-list)
  (let ((l (apply append (map module-exports which-list))))
    `(begin
       (use-modules ,@which-list )
       (re-export ,@l))))

(define-macro (texmacs-module name . options)
  (define (transform action)
    (cond ((not (pair? action)) (noop))
      ((equal? (car action) :use) (cons 'use-modules (cdr action)))
      ((equal? (car action) :inherit) (cons 'inherit-modules (cdr action)))
      ((equal? (car action) :export)
       (display "Warning] The option :export is no longer supported\n")
       (display "       ] Please use tm-define instead\n"))
      (else '(noop))))
  (let ((l (map transform options)))
    ;;(display "loading ") (display name) (display "\n")
    `(begin
        (define *module-name* ',name)
        (define *exports* '())
        (define *tm-defines* '())
        (hashtable-set! *modules* ',name (current-module))
       ,@l)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; On-entry and on-exit macros
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(module (quit-TeXmacs-scheme)
(define quit-TeXmacs-scheme (lambda args (noop)))
)

(define-macro (on-entry . cmd)
  `(begin ,@cmd))

(define-macro (on-exit . cmd)
  `(let ((prev quit-TeXmacs-scheme))
     (set! quit-TeXmacs-scheme (lambda () ,@cmd (prev)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; compatibility layer
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(inherit-modules (chez keywords))

(define-public (keyword->symbol k) (string->symbol (keyword->string k)))
(define-keywords or and not repeat group quote exclude range and-not match replace up down first last next previous)

(define-keywords mode require type synopsis returns note argument arguments default proposals secure check-mark interactive balloon)
(define-keywords all applicable)
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
|person| |pre| |preamble| |prefix| |preformatted| |preprocessor| |preprocessor_directive| |pretty| |previous| |print|
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

(display "End of boot.scm\n")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
