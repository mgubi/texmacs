
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : code-test.scm
;; DESCRIPTION : the features of program code, with providers of the tests
;; COPYRIGHT   : (C) 2026  Massimiliano Gubinelli
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; The mechanism which tree-sitter and the language servers share
;; (doc/code-features.md), tested with providers of its own: it does not
;; need either of them, so that it runs in every build.

(texmacs-module (check code-test)
  (:use (check check-lib)
        (code code-features)
        (code code-fold)))

(define (test-providers)
  (check-group "providers")
  (let ((asked '()))
    (code-provider-set! 'test-feature 'b 20 (lambda () #t)
                        (lambda (x) (set! asked (cons 'b asked)) (list 'b x)))
    (code-provider-set! 'test-feature 'a 10 (lambda () #t)
                        (lambda (x) (set! asked (cons 'a asked)) #f))
    (code-provider-set! 'test-feature 'c 5 (lambda () #f)
                        (lambda (x) (list 'c x)))
    ;; by rank; those which are not available are not asked; one which
    ;; answers #f lets the next one answer
    (check= (code-provider-names 'test-feature) '(c a b))
    (check= (code-available-providers 'test-feature) '(a b))
    (check= (code-ask 'test-feature 1) '(b 1))
    (check= asked '(b a))
    ;; one which is set again replaces the former one
    (code-provider-set! 'test-feature 'a 30 (lambda () #t)
                        (lambda (x) (list 'a x)))
    (check= (code-provider-names 'test-feature) '(c b a))
    (code-provider-remove! 'test-feature 'b)
    (check= (code-ask 'test-feature 2) '(a 2))
    (for (n '(a c)) (code-provider-remove! 'test-feature n))
    (check-false (code-available? 'test-feature))
    (check= (code-ask 'test-feature 3) #f)))

(define (with-code-buffer lines proc)
  ;; @proc in a buffer of program code with @lines
  (let* ((u (string->url "tmfs://aux/code-test.cpp"))
         (old (current-buffer)))
    (when (buffer-exists? u) (buffer-close u))
    (new-buffer-with u `(document ,@lines))
    (proc (buffer-get-body (current-buffer)))
    (buffer-close u)
    (when (buffer-exists? old) (switch-to-buffer old))))

(define (new-buffer-with u doc)
  (buffer-set-body u (stree->tree doc))
  (switch-to-buffer u))

(define (test-folds)
  (check-group "folds")
  (with-code-buffer '("int f () {" "  int x;" "  return x;" "}" "")
    (lambda (b)
      (check= (code-folds b) '())
      ;; the lines after the first one are hidden, a state of the lines
      (check-true (code-fold-lines b 0 2))
      (check= (code-folds b) '((0 2)))
      (check-true (code-hidden? b 1))
      (check-false (code-hidden? b 0))
      (check= (tree->stree b)
              '(document "int f () {" "  int x;" "  return x;" "}" ""))
      ;; (not again, nor from a hidden line)
      (check-false (code-fold-lines b 0 2))
      (check-false (code-fold-lines b 1 2))
      (check-true (code-unfold-lines b 0))
      (check= (code-folds b) '())
      ;; the blocks of a provider: the innermost one around the cursor
      (code-provider-set! 'fold-ranges 'test 0 (lambda () #t)
                          (lambda () '((0 3) (0 2))))
      (go-to-path (append (buffer-path) '(1 3)))
      (code-fold)
      (check= (code-folds b) '((0 2)))
      ;; (the cursor goes to the end of the first line)
      (check= (cursor-path) (append (buffer-path) '(0 10)))
      ;; an edited hidden line forgets it
      (tree-insert! (tree-ref b 2) 0 " ")
      (check= (code-folds b) '((0 1)))
      (code-unfold-all)
      (check= (code-folds b) '())
      ;; a jump into a hidden line opens the fold
      (code-fold-lines b 0 2)
      (code-cursor-in-fold (append (buffer-path) '(4 0))
                           (append (buffer-path) '(2 1)))
      (check= (code-folds b) '())
      (code-provider-remove! 'fold-ranges 'test))))

(define (test-positions)
  (check-group "positions")
  ;; columns: a character of Cork is one, a tab goes to its stop
  (check= (map (lambda (o) (code-column "a<#9>b<alpha>c" o 8)) '(0 1 5 6 13 14))
          '(0 1 8 9 10 11))
  (check= (code-column "ab<#9>c" 6 4) 4)
  ;; UTF-16 units, as the servers count them: an accent is one, a
  ;; character beyond U+FFFF two, a tab one; a backquote stays one
  (let* ((e-acute (string (integer->char 233)))
         (line (string-append "a" e-acute "<#1F600>`<#9>b")))
    (check= (map (lambda (o) (code-utf16-column line o)) '(0 1 2 10 11 15 16))
            '(0 1 2 4 5 6 7))
    (check= (map (lambda (u) (code-utf16-offset line u)) '(0 2 3 4 6 7 99))
            '(0 2 2 10 15 16 16)))
  (check= (code-utf16-offset "abc" 2) 2)
  ;; the end of the indentation: spaces and tabs
  (check= (code-indent-end "<#9>  x") 6)
  (check= (code-indent-end "   ") 3)
  (check= (code-indent-end "x") 0))

(define (test-layers)
  (check-group "layers of lines")
  (with-code-buffer '("int f (int x);" "")
    (lambda (b)
      (let ((line (tree-ref b 0))
            (layers (lambda (t) (tree->stree (code-line-layers t)))))
        (check= (layers line) '(tuple (tuple) (tuple)))
        (code-set-line-layers line
                              (stree->tree '(tuple (tuple "4" "5" "blue")
                                                   (tuple "6" "9" "red")))
                              (stree->tree '(tuple (tuple "11" "x:")
                                                   (tuple "14" "!"))))
        (check= (layers line)
                '(tuple (tuple (tuple "4" "5" "blue") (tuple "6" "9" "red"))
                        (tuple (tuple "11" "x:") (tuple "14" "!"))))
        ;; (they are not in the document)
        (check= (tree->stree b) '(document "int f (int x);" ""))
        ;; a line which is edited forgets them
        (tree-insert! line 0 " ")
        (check= (layers line) '(tuple (tuple) (tuple)))
        (code-set-line-layers line (stree->tree '(tuple (tuple "0" "1" "red")))
                              (stree->tree '(tuple)))
        (code-clear-line-layers line)
        (check= (layers line) '(tuple (tuple) (tuple)))))))

(define (test-snapshots)
  (check-group "changes of lines")
  (let* ((doc (lambda l (stree->tree `(document ,@l))))
         (snap (code-snapshot-new (doc "int a;" "int b;" "}")))
         (change (lambda (t) (tree->stree (code-snapshot-change snap t)))))
    ;; none
    (check= (change (doc "int a;" "int b;" "}")) '(tuple))
    ;; in a line: the characters between its common start and end
    (check= (change (doc "int a;" "long b;" "}")) '(tuple "1" "0" "1" "3" "long"))
    ;; a line inserted, removed (the line ends are those of the lines)
    (check= (change (doc "int a;" "x" "long b;" "}"))
            '(tuple "1" "0" "1" "0" "x\n"))
    (check= (change (doc "int a;" "long b;" "}"))
            '(tuple "1" "0" "2" "0" ""))
    ;; at the end of the text (which ends by no line end)
    (check= (change (doc "int a;" "long b;" "}" "z"))
            '(tuple "2" "1" "2" "1" "\nz"))
    (check= (change (doc "int a;" "long b;"))
            '(tuple "1" "7" "3" "1" ""))
    ;; the units of the servers, the text of the file
    (check= (change (doc "int a;" (string-append "x" (string (integer->char 233))
                                                 "<#9>`")))
            (list 'tuple "1" "0" "1" "7"
                  (string-append "x" (cork->sourcecode (string (integer->char 233)))
                                 (string #\tab) "`")))
    (check= (code-snapshot-text snap)
            (string-append "int a;\nx" (cork->sourcecode (string (integer->char 233)))
                           (string #\tab) "`"))
    (check-false (code-snapshot-changed? snap (doc "int a;" (string-append "x" (string (integer->char 233)) "<#9>`"))))
    (check-true (code-snapshot-changed? snap (doc "int a;")))
    (code-snapshot-delete snap)))

(tm-define (code-test-failures)
  (check-suite "code")
  (test-snapshots)
  (test-layers)
  (test-positions)
  (test-providers)
  (test-folds)
  (check-end))
