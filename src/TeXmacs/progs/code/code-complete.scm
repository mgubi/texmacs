
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : code-complete.scm
;; DESCRIPTION : the completion of program code
;; COPYRIGHT   : (C) 2026  Massimiliano Gubinelli
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; The completion of a name at the cursor (tab): a language server completes
;; it when it follows the file (progs/lsp); the syntax tree says where the
;; cursor is (feature completion-context: string, comment or code), so that
;; there is no completion in a string or a comment, and gives the names of
;; the code (feature local-names: the names of its definitions), which tab
;; completes where no server follows the code (also in code blocks).

(texmacs-module (code code-complete)
  (:use (code code-features)
        (code code-fold)))

(tm-define (code-completion-context)
  (:synopsis "Where the cursor is: string, comment or code")
  (or (code-ask 'completion-context) 'code))

(tm-define (code-in-text?)
  (:synopsis "Is the cursor in a string or a comment of program code?")
  (in? (code-completion-context) '(string comment)))

(define (word-char? c)
  (or (char-alphabetic? c) (char-numeric? c) (== c #\_)))

(define (word-before line off)
  ;; the name which ends at @off of @line
  (let loop ((i off))
    (if (and (> i 0) (word-char? (string-ref line (- i 1))))
        (loop (- i 1))
        (substring line i off))))

(define (cursor-word)
  ;; the name before the cursor, or ""
  (let ((c (code-cursor-lines)))
    (if (not c) ""
        (with t (tree-ref (car c) (cadr c))
          (if (not (tree-atomic? t)) ""
              (with s (tree->string t)
                (word-before s (min (caddr c) (string-length s)))))))))

(tm-define (code-local-completions prefix)
  (:synopsis "The suffixes of the names of the code which start with @prefix")
  (let* ((names (or (code-ask 'local-names) '()))
         (l (list-filter names (lambda (n) (and (string-starts? n prefix)
                                                (> (string-length n)
                                                   (string-length prefix))))))
         (suffixes (map (lambda (n) (substring n (string-length prefix)
                                               (string-length n)))
                        l)))
    (list-remove-duplicates (sort suffixes string<?))))

(tm-define (code-local-completion-here?)
  (:synopsis "Does tab complete with the names of the code at the cursor?")
  ;; (where no server follows the code, nor a session which completes
  ;; itself, after a name which some names of the code continue, not in a
  ;; string or a comment; else tab keeps its meaning)
  (and (in-prog?) (code-cursor-lines)
       (not (and (in-session?) (defined? 'session-supports-completions?)
                 (session-supports-completions?)))
       (not (code-available? 'server-diagnostics))
       (code-available? 'local-names)
       (!= (cursor-word) "")
       (not (code-in-text?))
       (nnull? (code-local-completions (cursor-word)))))

(tm-define (code-complete-locally)
  (:synopsis "Completes the name at the cursor with the names of the code")
  (let* ((w (cursor-word))
         (l (code-local-completions w)))
    (if (null? l) (set-message "No completions" "")
        (custom-complete (stree->tree `(tuple ,w ,@l))))))

(tm-define (kbd-variant t forwards?)
  (:require (and forwards? (code-local-completion-here?)))
  (code-complete-locally))
