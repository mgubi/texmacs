
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : code-features.scm
;; DESCRIPTION : the features of program code and their providers
;; COPYRIGHT   : (C) 2026  Massimiliano Gubinelli
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; A feature of program code (the blocks to fold, the outline...) is given by
;; providers: tree-sitter (the syntax of the code, at once) and the language
;; servers (the meaning, over a project), see src/docs/code-features.md. Each
;; registers itself from its adapter (code-provider-set!), which is loaded
;; only if it is there: TeXmacs without one of them, or both, keeps working.
;; The commands of the features (code-fold...) ask the providers of a
;; feature, by rank, among those which are available at the cursor; the
;; preference "code:<feature>" chooses one of them ("auto": by rank, "off":
;; none).

(texmacs-module (code code-features))

;; the adapters of the providers, loaded when a feature is first asked for,
;; when the feature is part of the build (its glue, and its programs)
(define code-adapters
  '((tree-sitter-program-language "treesit/ts-code.scm" (treesit ts-code))
    (lsp-start "lsp/lsp-code.scm" (lsp lsp-code))))

(define adapters-loaded? #f)

(define (load-adapters)
  (when (not adapters-loaded?)
    (set! adapters-loaded? #t)
    (for (a code-adapters)
      (when (and (defined? (car a))
                 (url-exists? (url-append "$TEXMACS_PATH/progs" (cadr a))))
        (module-provide (caddr a))))))

;; feature -> list of (name rank available? procedure), by rank
(define providers (make-ahash-table))

(tm-define (code-provider-set! feature name rank available? proc)
  (:synopsis "The provider @name of @feature, of @rank (the lowest first)")
  ;; @available? (no argument) tells whether it can serve at the cursor
  (let* ((old (list-filter (or (ahash-ref providers feature) '())
                           (lambda (p) (!= (car p) name))))
         (l (cons (list name rank available? proc) old)))
    ;; (the order of insertion between equal ranks: sort is not stable)
    (ahash-set! providers feature
                (sort l (lambda (x y) (< (cadr x) (cadr y)))))))

(tm-define (code-provider-remove! feature name)
  (:synopsis "Removes the provider @name of @feature")
  (ahash-set! providers feature
              (list-filter (or (ahash-ref providers feature) '())
                           (lambda (p) (!= (car p) name)))))

(tm-define (code-provider-names feature)
  (:synopsis "The names of the providers of @feature, by rank")
  (load-adapters)
  (map car (or (ahash-ref providers feature) '())))

(define prefs-ignored? #f)

(tm-define (code-ignore-preferences! on)
  (:synopsis "The providers by rank only, whatever the preferences say (tests)")
  (set! prefs-ignored? on))

(define (chosen feature)
  ;; the providers of @feature which may serve, by rank
  (load-adapters)
  (let ((pref (if prefs-ignored? "auto"
                  (get-preference (string-append "code:" (symbol->string feature)))))
        (l (or (ahash-ref providers feature) '())))
    (cond ((== pref "off") '())
          ((and (string? pref) (!= pref "auto") (!= pref "default")
                (assoc (string->symbol pref) l))
           (list (assoc (string->symbol pref) l)))
          (else l))))

(tm-define (code-available-providers feature)
  (:synopsis "The names of the providers of @feature available at the cursor")
  (map car (list-filter (chosen feature) (lambda (p) ((caddr p))))))

(tm-define (code-available? feature)
  (:synopsis "Can @feature be served at the cursor?")
  (nnull? (code-available-providers feature)))

(tm-define (code-ask feature . args)
  (:synopsis "The answer of the first available provider of @feature")
  ;; (the next ones are asked when one answers #f or nothing, '(): a
  ;; server which has no block at the cursor lets tree-sitter look)
  (let loop ((l (chosen feature)))
    (cond ((null? l) #f)
          ((not ((caddr (car l)))) (loop (cdr l)))
          ((with r (apply (cadddr (car l)) args) (and (nnull? r) r)) => identity)
          (else (loop (cdr l))))))
