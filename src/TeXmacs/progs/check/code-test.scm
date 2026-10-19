
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
;; (src/docs/code-features.md), tested with providers of its own: it does not
;; need either of them, so that it runs in every build.

(texmacs-module (check code-test)
  (:use (check check-lib)
        (code code-features)
        (code code-fold)
        (code code-navigate)
        (code code-diagnostics)
        (code code-complete)
        (code code-menu)))

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
      ;; (not twice at the same line); a fold in a fold makes its lines
      ;; deeper, unfolding the outer one keeps it
      (check-false (code-fold-lines b 0 2))
      (check-true (code-fold-lines b 1 2))
      (check= (code-folds b) '((0 2) (1 2)))
      (check= (code-fold-depth b 2) 2)
      (check-true (code-unfold-lines b 0))
      (check= (code-folds b) '((1 2)))
      (check-true (code-unfold-lines b 1))
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
      ;; a provider which has no block lets the next one answer
      (code-provider-set! 'fold-ranges 'none -1 (lambda () #t) (lambda () '()))
      (check= (code-ask 'fold-ranges) '((0 3) (0 2)))
      (code-provider-remove! 'fold-ranges 'none)
      ;; hidden lines whose first line was deleted are shown by unfold all
      ;; (the cursor away from them: a cursor which comes into them opens
      ;; them, unfold-around!)
      (check-true (code-fold-lines b 0 2))
      (go-to-path (append (buffer-path) '(3 0)))
      (tree-remove! b 0 1)
      (check-true (code-hidden? b 0))
      (check= (code-folds b) '())
      (code-unfold-all)
      (check-false (code-hidden? b 0))
      (check-false (code-hidden? b 1))
      ;; a jump into a hidden line opens the folds around it, from the
      ;; innermost
      (code-fold-lines b 1 2)
      (code-fold-lines b 0 3)
      (check= (code-folds b) '((0 3) (1 2)))
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

(define (test-navigation)
  (check-group "outline and definitions")
  (with-code-buffer '("int f () {" "  return 1;" "}" "int g;" "")
    (lambda (b)
      (let ((f-at (append (buffer-path) '(0 4)))
            (g-at (append (buffer-path) '(3 4)))
            (jumps '()))
        (code-provider-set! 'outline 'test 0 (lambda () #t)
                            (lambda () (list (list 0 "function" "f" "()" f-at)
                                             (list 1 "variable" "g" "" g-at))))
        (check= (map caddr (code-outline-entries)) '("f" "g"))
        ;; the page of the outline: its entries go to their places
        (code-outline)
        (with page (current-buffer)
          (check-true (url-rooted-tmfs? page))
          (with s (object->string (tree->stree (buffer-get-body page)))
            (check-true (string-contains? s "function"))
            (check-true (string-contains? s "code-go-to")))
          (buffer-close page))
        ;; a provider which finds nothing lets the next one look
        (code-provider-set! 'definition 'first 0 (lambda () #t)
                            (lambda () (set! jumps (cons 'first jumps)) #f))
        (code-provider-set! 'definition 'second 1 (lambda () #t)
                            (lambda () (set! jumps (cons 'second jumps)) #t))
        (code-go-to-definition)
        (check= jumps '(second first))
        (for (x '((outline test) (definition first) (definition second)))
          (code-provider-remove! (car x) (cadr x)))))))

(define (test-diagnostics)
  (check-group "diagnostics")
  (with-code-buffer '("int f () {" "  retur 1;" "}" "")
    (lambda (b)
      (let ((at (lambda (r c) (append (buffer-path) (list r c)))))
        ;; the items of each source, all of them underlined
        (code-set-diagnostics 'server (list (list 'error (at 1 2) (at 1 7) "unknown")))
        (code-set-diagnostics 'syntax (list (list 'warning (at 0 4) (at 0 5) "x")))
        (check= (length (code-diagnostics)) 2)
        (check= (get-alt-selection "code-errors") (list (at 1 2) (at 1 7)))
        (check= (get-alt-selection "code-warnings") (list (at 0 4) (at 0 5)))
        ;; the menu Focus visits them
        ;; the menu Focus visits them (greyed without diagnostics)
        (with m (menu-expand '(vertical (dynamic (code-diagnostics-menu))))
          (check= (car (car (cadr m))) 'when)
          (check-true (cadr (car (cadr m)))))
        ;; from one to the next, and back to the first
        (go-to (at 0 0))
        (code-next-diagnostic #t)
        (check= (cursor-path) (at 0 4))
        (code-next-diagnostic #t)
        (check= (cursor-path) (at 1 2))
        (check= (code-diagnostic-at-cursor) "unknown")
        (code-next-diagnostic #t)
        (check= (cursor-path) (at 0 4))
        ;; a source which is cleared leaves the others
        (code-clear-diagnostics 'syntax)
        (check= (code-diagnostics 'server)
                (list (list 'error (at 1 2) (at 1 7) "unknown")))
        (check= (get-alt-selection "code-warnings") '())
        (code-clear-diagnostics 'server)
        (check= (get-alt-selection "code-errors") '())
        (with m (menu-expand '(vertical (dynamic (code-diagnostics-menu))))
          (check-false (cadr (car (cadr m)))))))))

(define (test-completion)
  (check-group "completion")
  (let ((where 'code))
    (code-provider-set! 'completion-context 'test 0 (lambda () #t)
                        (lambda () where))
    (code-provider-set! 'local-names 'test 0 (lambda () #t)
                        (lambda () '("count" "counter" "cout" "max" "count")))
    (check-false (code-in-text?))
    (set! where 'string)
    (check-true (code-in-text?))
    (set! where 'comment)
    (check-true (code-in-text?))
    ;; the suffixes of the names which start with the prefix
    (check= (code-local-completions "cou") '("nt" "nter" "t"))
    (check= (code-local-completions "count") '("er"))
    (check= (code-local-completions "z") '())
    (code-provider-remove! 'completion-context 'test)
    (code-provider-remove! 'local-names 'test)
    (check= (code-completion-context) 'code)))

(tm-define (code-test-failures)
  (check-suite "code")
  ;; (the providers by rank, whatever the preferences code:<feature> of
  ;; the user say)
  (code-ignore-preferences! #t)
  (test-completion)
  (test-diagnostics)
  (test-navigation)
  (test-snapshots)
  (test-layers)
  (test-positions)
  (test-providers)
  (test-folds)
  (code-ignore-preferences! #f)
  (check-end))
