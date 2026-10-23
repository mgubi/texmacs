;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : search-test.scm
;; DESCRIPTION : tests of structured searching
;; COPYRIGHT   : (C) 2026  Massimiliano Gubinelli
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; The search engine of Edit > Search and Edit > Replace (C++
;; src/Data/Tree/tree_search.cpp), through tree-search-tree and
;; tree-search-tree-at on detached trees. A result is the flat list of the
;; start and end paths of the hits, in the order of the document.
;;
;; Searching and replacing match differently. Edit > Search (toolbar or
;; widget) sets the preference "search-and-replace" to "off": the matching
;; then follows the preferences allow-blank-match, allow-initial-match,
;; allow-partial-match and allow-injective-match, all on by default, so
;; that a string of the pattern may match a part of a string, an empty
;; argument matches any argument, and a tag matches a tag whose arguments
;; contain those of the pattern. Edit > Replace sets it to "on": the
;; matching follows allow-blank-replace, ..., all off by default, and a
;; string must match a whole string. Both allow cascaded matches (a pattern
;; inside an argument). The checks set preferences in memory only, and
;; restore them: nothing is saved.
;;
;; The formulas are written as the editor keeps them: a concat never has
;; two strings in a row (tree-simplify merges them), so that a+b_i is
;; (concat "a+b" (rsub "i")).
;;
;; Wildcards (std 1, std 2, std 3 in the widgets) are (wildcard "x"), ...;
;; at the start or at the end of a concat pattern, a wildcard extends the
;; hit to the start or to the end of the formula (at the end, not always:
;; #453; several hits inside one tag: #454).

(texmacs-module (check search-test)
  (:use (check check-lib)))

(define (with-preference which val thunk)
  ;; run thunk with the preference set in memory only: nothing is saved
  (let* ((had? (cpp-has-preference? which))
         (old (get-preference which)))
    (cpp-set-preference which val)
    (with r (check-run thunk)
      (if had?
          (cpp-set-preference which old)
          (cpp-reset-preference which))
      (when (and (pair? r) (== (car r) 'error))
        (check-report #f "the group" (object->string r))))))

(define (with-search-mode mode thunk)
  ;; mode is 'search (Edit > Search) or 'replace (Edit > Replace)
  (with-preference "search-and-replace" (if (== mode 'search) "off" "on")
    thunk))

(define (hits doc what)
  (tree-search-tree (stree->tree doc) what '() 1000))

(define (check-both doc what expected)
  ;; the same hits when searching and when replacing
  (with-search-mode 'search (lambda () (check= (hits doc what) expected)))
  (with-search-mode 'replace (lambda () (check= (hits doc what) expected))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Text
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; a string is found inside strings, inside tags, several times in a string
(define (test-text)
  (check-group "text")
  (check-both '(document "the cat sat" (concat "a cat " (em "cat") " scat"))
              "cat"
              '((0 4) (0 7) (1 0 2) (1 0 5) (1 1 0 0) (1 1 0 3)
                (1 2 2) (1 2 5)))
  (check-both '(document "the cat sat") "dog" '())
  ;; case sensitive by default; not with case-insensitive-match, also for
  ;; the strings of a concat pattern
  (check-both '(document "Cat cat CAT") "cat" '((0 4) (0 7)))
  ;; FIXME (#460): case-insensitive-match ignores the strings inside the
  ;; tags of the pattern (match (tree t, tree what), tree_search.cpp:139:
  ;; its string cases compare with the case): (em "cat") in
  ;; (concat "a " (em "Cat")), or (concat "x" (rsub "n")) in
  ;; (concat "X" (rsub "N")), gives (), expected the em or the formula.
  (with-preference "case-insensitive-match" "on"
    (lambda ()
      (check-both '(document "Cat cat CAT") "cat"
                  '((0 0) (0 3) (0 4) (0 7) (0 8) (0 11)))
      (check-both '(document "Cat cat CAT") "CAT"
                  '((0 0) (0 3) (0 4) (0 7) (0 8) (0 11)))
      (check-both '(document (concat "X+" (rsub "n"))) '(concat "x+" (rsub "n"))
                  '((0 0 0) (0 1 1)))
      (check-both '(document (concat "X" (rsub "n"))) '(concat "x" (rsub "n"))
                  '((0 0 0) (0 1 1))))))

;; a concat of strings and wildcards also matches inside one string: in
;; the search widget, each occurrence; in the replace widget, the whole
;; string only
(define (test-text-wildcards)
  (check-group "text wildcards")
  (let ((doc '(document "abc axc ac" (concat "ab" (em "x") "c")))
        (what '(concat "a" (wildcard "x") "c")))
    (with-search-mode 'search
      (lambda ()
        (check= (hits doc what)
                '((0 0) (0 3) (0 4) (0 7) (0 8) (0 10) (1 0 0) (1 2 1)))))
    (with-search-mode 'replace
      (lambda ()
        (check= (hits doc what) '((0 0) (0 10) (1 0 0) (1 2 1))))))
  (with-search-mode 'search
    (lambda ()
      (check= (hits '(document "abc xbc") '(concat (wildcard "x") "bc"))
              '((0 0) (0 3) (0 3) (0 7)))))
  (with-search-mode 'replace
    (lambda ()
      (check= (hits '(document "abc xbc") '(concat (wildcard "x") "bc"))
              '((0 0) (0 7)))))
  (check-both '(document "abc xbc") '(concat "a" (wildcard "x"))
              '((0 0) (0 7))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Tags (the examples of the manual, man-search-and-replace)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (test-tags)
  (check-group "tags")
  (let ((doc '(document (concat "a " (em "bold a") " b " (em "c")))))
    (with-search-mode 'search
      (lambda ()
        ;; the em tags which contain a; all the em tags
        (check= (hits doc '(em "a")) '((0 1 0) (0 1 1)))
        (check= (hits doc '(em "")) '((0 1 0) (0 1 1) (0 3 0) (0 3 1)))))
    (with-search-mode 'replace
      (lambda ()
        (check= (hits doc '(em "a")) '())
        (check= (hits doc '(em "")) '()))))
  (with-search-mode 'search
    (lambda ()
      ;; the fractions whose numerator contains x
      (check= (hits '(document (math (concat (frac "x+1" "y") "+"
                                             (frac "z" "x"))))
                    '(frac "x" ""))
              '((0 0 0 0) (0 0 0 1)))
      ;; the fractions whose denominator contains a square root which
      ;; contains x: (a+b)/(c+sqrt(x+y)), not sqrt(x+y)/(a+sqrt(y))
      (check= (hits '(document
                       (math (concat (frac "a+b"
                                           (concat "c+" (sqrt "x+y")))
                                     "="
                                     (frac (sqrt "x+y")
                                           (concat "a+" (sqrt "y"))))))
                    '(frac "" (sqrt "x")))
              '((0 0 0 0) (0 0 0 1)))))
  ;; select-region: the letters a inside strong, not the other ones
  (check-both '(document (concat "a " (strong "a cat") " a"))
              '(strong (select-region "a"))
              '((0 1 0 0) (0 1 0 1) (0 1 0 3) (0 1 0 4))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Formulas: concat patterns
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (test-formulas)
  (check-group "formulas")
  ;; at the end of a string, inside a fraction, and after a near miss A_j
  (check-both '(document
                 (concat "B+A" (rsub "i") "+C"
                         (frac "A" (concat "A" (rsub "i") "x")))
                 (concat "A" (rsub "j") "A" (rsub "i")))
              '(concat "A" (rsub "i"))
              '((0 0 2) (0 1 1) (0 3 1 0 0) (0 3 1 1 1) (1 2 0) (1 3 1)))
  (check-both '(document (concat "A" (rsub "j"))) '(concat "A" (rsub "k")) '())
  ;; the first string of the pattern matches the end of a string, the last
  ;; one the start of a string
  (check-both '(document (concat "x+A" (rsub "i") "+y"))
              '(concat "+A" (rsub "i") "+")
              '((0 0 1) (0 2 1)))
  ;; two tags in a row, in this order only
  (check-both '(document (concat "f" (rsub "n") (rsup "2") "+f" (rsup "2")
                                 (rsub "n")))
              '(concat (rsub "n") (rsup "2"))
              '((0 1 0) (0 2 1)))
  ;; a hit inside a tag, then one in the formula after it: the search goes
  ;; on after the tag
  (check-both '(document (concat "B" (frac (concat "A" (rsub "i")) "1")
                                 "+A" (rsub "i")))
              '(concat "A" (rsub "i"))
              '((0 1 0 0 0) (0 1 0 1 1) (0 2 1) (0 3 1)))
  ;; A_ij contains A_i when searching only
  (let ((doc '(document (concat "A" (rsub "ij") "+A" (rsub "i"))))
        (what '(concat "A" (rsub "i"))))
    (with-search-mode 'search
      (lambda ()
        (check= (hits doc what) '((0 0 0) (0 1 1) (0 2 1) (0 3 1)))))
    (with-search-mode 'replace
      (lambda ()
        (check= (hits doc what) '((0 2 1) (0 3 1)))))))

(define (test-formula-wildcards)
  (check-group "formula wildcards")
  (let ((doc '(document (concat "A" (rsub "n") "+B" (rsub "n")
                                "+C" (rsub "m")))))
    ;; a leading wildcard extends the hit to the start of the formula
    (check-both doc '(concat (wildcard "x") (rsub "n")) '((0 0 0) (0 1 1)))
    (check-both doc '(concat (wildcard "x") (wildcard "y") (rsub "n"))
                '((0 0 0) (0 1 1)))
    ;; a trailing one to its end
    (check-both doc '(concat "A" (wildcard "x")) '((0 0 0) (0 5 1)))
    (check-both doc '(concat (rsub "n") (wildcard "y")) '((0 1 0) (0 5 1))))
  (check-both '(document (concat "A+B" (rsub "m") "+C"))
              '(concat "A+" (wildcard "y"))
              '((0 0 0) (0 2 2)))
  ;; in the middle: any content; when searching, each f(...) and g(...);
  ;; when replacing, a string of the pattern must end or start with the
  ;; string it matches, so that ")" only matches the last ")"
  (let ((doc '(document (concat "f(" (frac "1" "2") ")+g(" (rsub "n") ")")))
        (what '(concat "(" (wildcard "x") ")")))
    (with-search-mode 'search
      (lambda ()
        (check= (hits doc what) '((0 0 1) (0 2 1) (0 2 3) (0 4 1)))))
    (with-search-mode 'replace
      (lambda ()
        (check= (hits doc what) '((0 0 1) (0 4 1))))))
  (check-both '(document (concat "f(" (frac "1" "2") ")+g(" (rsub "n") ")"))
              '(concat "f(" (wildcard "x") ")+g(")
              '((0 0 0) (0 2 4)))
  ;; inside a tag of the pattern
  (check-both '(document (concat "A" (rsub "n") "=" (frac "1" "2")))
              '(concat (rsub (wildcard "x")) "=")
              '((0 1 0) (0 2 1)))
  ;; a wildcard alone matches whole formulas and strings
  (check-both '(document (concat "A" (rsub "n")) "text")
              '(concat (wildcard "x"))
              '((0 0 0) (0 1 1) (1 0) (1 4)))
  ;; a trailing wildcard after a tag, when no tag follows in the formula
  ;; (only a string, or nothing): from the hit to the end (#453)
  (check-both '(document (concat "A" (rsub "n") "+B"))
              '(concat (rsub "n") (wildcard "y"))
              '((0 1 0) (0 2 2)))
  (check-both '(document (concat "A" (rsub "n")))
              '(concat (rsub "n") (wildcard "y"))
              '((0 1 0) (0 1 1)))
  (check-both '(document (concat "A" (rsub "n") "+B"))
              '(concat "A" (rsub "n") (wildcard "y"))
              '((0 0 0) (0 2 2)))
  (check-both '(document (concat "a+" (frac "1" "2") "+b"))
              '(concat (wildcard "x") (frac "1" "2") (wildcard "y"))
              '((0 0 0) (0 2 2)))
  ;; an empty string in a concat pattern is left out (it never returned,
  ;; and the memory grew without bound: #452)
  (check-both '(document (concat "a" (rsub "b") "c"))
              '(concat (rsub "b") "")
              '((0 1 0) (0 1 1)))
  (check-both '(document (concat "a" (rsub "b") "c"))
              '(concat "" (rsub "b"))
              '((0 1 0) (0 1 1)))
  (check-both '(document (concat "a" (rsub "b") (rsub "c")))
              '(concat (rsub "b") "" (rsub "c"))
              '((0 1 0) (0 2 1)))
  (check-both '(document (concat "a" (rsub "b") "c"))
              '(concat (wildcard "y") (wildcard "y") (rsub "b") "")
              '((0 0 0) (0 1 1)))
  ;; a trailing wildcard after a string with several hits: from the
  ;; first one (the last one was taken)
  (check-both '(document (concat "x" (rsub "n") "bb"))
              '(concat "b" (wildcard "y"))
              '((0 2 0) (0 2 2)))
  (check-both '(document (concat "x" (rsub "n") "bab"))
              '(concat "b" (wildcard "y"))
              '((0 2 0) (0 2 3)))
  ;; an empty paragraph of a document pattern matches an empty paragraph
  ;; (it never returned either)
  (check-both '(document "" "abc") '(document "" "abc") '((0 0) (1 3)))
  (check-both '(document "x" "abc") '(document "" "abc") '())
  ;; an empty pattern is found nowhere with the matching of Replace
  (with-search-mode 'replace
    (lambda () (check= (hits '(document "abc") "") '()))))

;; FIXME (#461): a hit inside a string hides a hit which starts later in
;; the same string (the outer search_concat, tree_search.cpp:300: a child
;; with a hit inside is never the start of a match across the children):
;; when searching, (concat "f(" (wildcard "x") ")") in
;; (concat "f(x)+f(" (frac "1" "2") ")") gives ((0 0 0) (0 0 4)), expected
;; also ((0 0 5) (0 2 1)).
;; FIXME (#454): a leading or trailing wildcard with several hits inside
;; one tag gives empty and nested ranges (the outer search_concat,
;; tree_search.cpp:315-320: the loops which extend the hits step by one
;; over the pairs): (concat (wildcard "x") "a") in
;; (concat "b" (frac "a" "a")) gives ((0 0 0) (0 0 0) (0 0 0) (0 1 1 1)).

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Paragraphs, limits, positions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (test-paragraphs)
  (check-group "paragraphs")
  ;; a document pattern matches consecutive paragraphs
  (check-both '(document "one" "two" "three" "two" "three")
              '(document "two" "three")
              '((1 0) (2 5) (3 0) (4 5)))
  ;; the first one may end, and the last one start, as the pattern
  (check-both '(document "a one" "two" "three b")
              '(document "one" "two" "three")
              '((0 2) (2 5))))

(define (test-limit-and-position)
  (check-group "limit and position")
  (let* ((doc (stree->tree `(document ,@(map (lambda (i) "a cat") (iota 50)))))
         (all (tree-search-tree doc "cat" '() 1000))
         (some (tree-search-tree doc "cat" '() 10)))
    (check= (length all) 100)
    ;; the search stops soon after the limit: a prefix of all the hits
    (check-true (< (length some) (length all)))
    (check-true (>= (length some) 10))
    (check= (list-head all (length some)) some)
    ;; from a position: the same hits, in the order of the document
    (check= (tree-search-tree-at doc "cat" '() '(30 0) 1000) all)
    (check= (tree-search-tree-at doc '(concat "a" (wildcard "x")) '() '(30 0)
                                 1000)
            (tree-search-tree doc '(concat "a" (wildcard "x")) '() 1000))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Speed: long formulas (#450)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; a formula of n terms x_{k+1}+(ab)/(d^e)(y), as the editor keeps it:
;; x_{k+1}+(ab)/(d^e)(y)x_{k+1}+... with "(y)x" one string
(define (long-formula n)
  (tree-simplify
    (stree->tree
      `(document
         (concat ,@(append-map
                     (lambda (k)
                       (list "x" '(rsub "k+1") "+"
                             '(frac "ab" (concat "d" (rsup "e")))
                             "(y)"))
                     (iota n)))))))

;; x+_k x+_k ... (n terms): a string with a hit in each term
(define (dense-formula n)
  (tree-simplify
    (stree->tree
      `(document (concat ,@(append-map (lambda (k) (list "x+" '(rsub "k")))
                                       (iota n)))))))

(define (time-of thunk)
  (let* ((start (texmacs-time))
         (r (thunk)))
    (cons (- (texmacs-time) start) r)))

(define (check-fast t what expected)
  (with r (time-of (lambda () (tree-search-tree t what '() 100)))
    (check= (cdr r) expected)
    (check-true (< (car r) 500))))

;; each keystroke in the search field searches the document again: a long
;; formula takes milliseconds. On 800 terms, each of these took 1.3 to 3.9 s
;; before #450 (quadratic in the length of the formula), and takes 1 to
;; 3 ms after.
(define (test-speed)
  (check-group "speed")
  (let ((t (long-formula 800)))
    (for-each
      (lambda (mode)
        (with-search-mode mode
          (lambda ()
            (for-each (lambda (what) (check-fast t what '()))
                      (list '(concat "A" (rsub "ij"))
                            '(concat "x" (rsub "k-"))
                            '(concat "(y)x" (rsub "j"))
                            '(concat "d" (rsup "f"))
                            '(concat (wildcard "x") "q" (rsub "k"))))
            ;; a hit in each string which contains x: the whole formula.
            ;; The scans are not started again after a hit inside a string
            ;; (they took 2 s on the dense formula when they were)
            (check-fast t '(concat "x" (wildcard "y"))
                        '((0 0 0) (0 3200 3)))
            (check-fast (dense-formula 3000) '(concat "x" (wildcard "y"))
                        '((0 0 0) (0 5999 1))))))
      '(search replace))
    ;; with hits all along the formula (fast before #450 too)
    (with-search-mode 'search
      (lambda ()
        (with r (time-of (lambda ()
                           (tree-search-tree t '(concat "x" (rsub "k"))
                                             '() 10000)))
          (check= (length (cdr r)) 1600)
          (check-true (< (car r) 500))))))
  ;; a wildcard in the middle which fails late scanned from each start to
  ;; the end of the formula: 4.2 s on 200 terms before #450 (cubic), then
  ;; 14 ms on 200 and about 1 s on 1600 terms (quadratic); its failures are
  ;; remembered now (linear: a few ms)
  (with t (long-formula 1600)
    (for-each
      (lambda (mode)
        (with-search-mode mode
          (lambda ()
            (check-fast t '(concat "y)" (wildcard "x") "z") '()))))
      '(search replace))))

(tm-define (search-test-failures)
  (check-suite "search")
  (test-text)
  (test-text-wildcards)
  (test-tags)
  (test-formulas)
  (test-formula-wildcards)
  (test-paragraphs)
  (test-limit-and-position)
  (test-speed)
  (check-end))
