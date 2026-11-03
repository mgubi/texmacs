
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : code-diagnostics.scm
;; DESCRIPTION : the diagnostics of program code
;; COPYRIGHT   : (C) 2026  Massimiliano Gubinelli
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; The diagnostics of the program code of a buffer, from their sources: a
;; language server (its diagnostics, when it publishes them: progs/lsp), the
;; syntax tree (the errors and the missing nodes of tree-sitter, where no
;; server follows the code, which would say the same). Each source gives
;; its items, (severity start end message) with severity error, warning or
;; info and start, end paths of the buffer; all of them are underlined
;; (the alternative selections code-errors, code-warnings and code-infos,
;; which Edit/Interface/edit_interface.cpp draws), and F8 goes from one to
;; the next. The unmatched brackets of tree-sitter are shown at once, in
;; their colour, while the diagnostics come after a pause.
;;
;; The syntax errors come from the feature syntax-errors (code-features.scm):
;; a procedure which gives the items of the code at the cursor, or #f.

(texmacs-module (code code-diagnostics)
  (:use (code code-features)
        (code code-fold)))

;; buffer name -> source -> items
(define diagnostics (make-ahash-table))

(define (buffer-key) (url->string (current-buffer)))

(define (all-items)
  (with t (ahash-ref diagnostics (buffer-key))
    (if (not t) '() (append-map cdr (ahash-table->list t)))))

(define (severity-paths l severity)
  ;; (in the order of the text: the editor looks for the visible ones by a
  ;; binary search)
  (let ((xs (sort (list-filter l (lambda (x) (== (car x) severity)))
                  (lambda (x y) (path-less? (cadr x) (cadr y))))))
    (append-map (lambda (x) (list (cadr x) (caddr x))) xs)))

(define (draw)
  (with l (all-items)
    (for (s '((error "code-errors") (warning "code-warnings")
              (info "code-infos")))
      (with ps (severity-paths l (car s))
        (if (null? ps) (cancel-alt-selection (cadr s))
            (set-alt-selection (cadr s) ps))))))

(tm-define (code-set-diagnostics source items)
  (:synopsis "The diagnostics of @source in the current buffer are @items")
  (let ((t (or (ahash-ref diagnostics (buffer-key)) (make-ahash-table))))
    (if (null? items) (ahash-remove! t source) (ahash-set! t source items))
    (ahash-set! diagnostics (buffer-key) t)
    (draw)))

(tm-define (code-clear-diagnostics source)
  (:synopsis "The current buffer has no diagnostics of @source")
  (code-set-diagnostics source '()))

(tm-define (code-diagnostics . opt)
  (:synopsis "The diagnostics of the current buffer (of the source in @opt)")
  (if (null? opt) (all-items)
      (with t (ahash-ref diagnostics (buffer-key))
        (or (and t (ahash-ref t (car opt))) '()))))

(tm-define (code-next-diagnostic forward?)
  (:synopsis "Goes to the next (or previous) diagnostic of the buffer")
  (let* ((l (sort (all-items) (lambda (x y) (path-less? (cadr x) (cadr y)))))
         (p (cursor-path))
         (after (list-filter (if forward? l (reverse l))
                             (lambda (x) (if forward? (path-less? p (cadr x))
                                             (path-less? (cadr x) p)))))
         (x (if (nnull? after) (car after)
                (and (nnull? l) (if forward? (car l) (cAr l))))))
    (if (not x) (set-message "No diagnostics" "")
        (begin
          (go-to (cadr x))
          (set-message (cadddr x) "")))))

(tm-define (code-diagnostic-at-cursor)
  (:synopsis "The message of a diagnostic at the cursor, or #f")
  (let ((p (cursor-path)))
    (with x (list-find (all-items)
                       (lambda (x) (and (not (path-less? p (cadr x)))
                                        (not (path-less? (caddr x) p)))))
      (and x (cadddr x)))))

(tm-define (notify-buffer-closed name)
  (former name)
  (ahash-remove! diagnostics name))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The syntax errors
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define syntax-scheduled? #f)
(define syntax-checked #f)   ; (buffer . lines) of the last check

(define (server-diagnostics?)
  ;; does a server give the diagnostics of the code at the cursor?
  (code-available? 'server-diagnostics))

(tm-define (code-check-syntax)
  (:synopsis "Shows the syntax errors of the code at the cursor")
  ;; (where no server follows the code)
  ;; (those of the code at the cursor: none elsewhere, where their places
  ;; would no longer be those of the code after an edit around it)
  (set! syntax-scheduled? #f)
  (if (and (in-prog?) (code-cursor-lines) (not (server-diagnostics?)))
      (with items (code-ask 'syntax-errors)
        (code-set-diagnostics 'syntax (or items '())))
      (when (nnull? (code-diagnostics 'syntax))
        (code-clear-diagnostics 'syntax))))

(define (schedule-check)
  (when (not syntax-scheduled?)
    (set! syntax-scheduled? #t)
    (delayed
      (:idle 1000)
      (code-check-syntax))))

(tm-define (notify-buffer-changed name)
  ;; (NOTE: no mode predicates here: the check is delayed)
  (former name)
  (schedule-check))

(tm-define (notify-cursor-moved status)
  ;; (the message of a syntax error at the cursor; the code of another
  ;; region, or of another buffer, is checked once)
  (former status)
  (let* ((c (code-cursor-lines))
         (key (and c (cons (current-buffer) (car c)))))
    (when (and key (not (and syntax-checked
                             (== (car syntax-checked) (car key))
                             (tree-eq? (cdr syntax-checked) (cdr key)))))
      (set! syntax-checked key)
      (schedule-check))))
