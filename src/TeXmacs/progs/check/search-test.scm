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
;; hit to the start or to the end of the formula, from or to its first
;; hit.

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
  ;; the strings of a concat pattern and those inside its tags (#460)
  (check-both '(document "Cat cat CAT") "cat" '((0 4) (0 7)))
  (check-both '(document (concat "a " (em "Cat") " " (em "cat"))) '(em "cat")
              '((0 3 0) (0 3 1)))
  (with-preference "case-insensitive-match" "on"
    (lambda ()
      (check-both '(document "Cat cat CAT") "cat"
                  '((0 0) (0 3) (0 4) (0 7) (0 8) (0 11)))
      (check-both '(document "Cat cat CAT") "CAT"
                  '((0 0) (0 3) (0 4) (0 7) (0 8) (0 11)))
      (check-both '(document (concat "X+" (rsub "n"))) '(concat "x+" (rsub "n"))
                  '((0 0 0) (0 1 1)))
      (check-both '(document (concat "X" (rsub "n"))) '(concat "x" (rsub "n"))
                  '((0 0 0) (0 1 1)))
      (check-both '(document (concat "X" (rsub "N") "+x" (rsub "n")))
                  '(concat "x" (rsub "n"))
                  '((0 0 0) (0 1 1) (0 2 1) (0 3 1)))
      (with-search-mode 'search
        (lambda ()
          (check= (hits '(document (concat "a " (em "Cat") " " (em "cAT")))
                        '(em "cat"))
                  '((0 1 0) (0 1 1) (0 3 0) (0 3 1)))
          ;; a concat pattern inside a tag
          (check= (hits '(document (concat (frac "X+Y" "2") "=z"))
                        '(frac (concat "x" (wildcard "w") "y") ""))
                  '((0 0 0) (0 0 1))))))))

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

;; a hit inside a string, then one which starts later in the same string
;; and goes on in the next children: both (#461). When replacing, the
;; strings of the pattern must match whole strings, and one match goes from
;; the first f( to the end, as in the same text as one string (#477).
(define (test-hits-after-a-hit)
  (check-group "hits after a hit")
  (let ((doc '(document (concat "f(x)+f(" (frac "1" "2") ")")))
        (what '(concat "f(" (wildcard "x") ")")))
    (with-search-mode 'search
      (lambda ()
        (check= (hits doc what) '((0 0 0) (0 0 4) (0 0 5) (0 2 1)))))
    (with-search-mode 'replace
      (lambda ()
        (check= (hits doc what) '((0 0 0) (0 2 1))))))
  (with-search-mode 'search
    (lambda ()
      (check= (hits '(document (concat "axb+a" (rsub "1") "b+axb"))
                    '(concat "a" (wildcard "x") "b"))
              '((0 0 0) (0 0 3) (0 0 4) (0 2 1) (0 2 2) (0 2 5))))))

;; a leading (trailing) wildcard and several hits inside one tag: one hit,
;; from the start of the formula to the first of them (from the first of
;; them to the end), as with one hit; no empty ranges (#454)
(define (test-wildcards-and-several-hits)
  (check-group "wildcards and several hits")
  (check-both '(document (concat "b" (frac "a" "c")))
              '(concat (wildcard "x") "a")
              '((0 0 0) (0 1 0 1)))
  (check-both '(document (concat "b" (frac "a" "a")))
              '(concat (wildcard "x") "a")
              '((0 0 0) (0 1 0 1)))
  (check-both '(document (concat "b+" (frac "a" (concat "a" (rsub "a")))))
              '(concat (wildcard "x") "a")
              '((0 0 0) (0 1 0 1)))
  (check-both '(document (concat (frac "a" "a") "b"))
              '(concat "a" (wildcard "x"))
              '((0 0 0 0) (0 1 1))))

;; a string with several hits of its own, after a tag: the hits of the
;; string, as when it is alone; no empty or inverted ranges (#473). A
;; trailing wildcard starts at the first hit of the string.
(define (test-hits-of-a-string-after-a-tag)
  (check-group "hits of a string after a tag")
  (with-search-mode 'search
    (lambda ()
      (check= (hits '(document "aba+b") '(concat "a" (wildcard "x") "b"))
              '((0 0) (0 2) (0 2) (0 5)))
      (check= (hits '(document (concat "x" (rsub "c") "aba+b"))
                    '(concat "a" (wildcard "x") "b"))
              '((0 2 0) (0 2 2) (0 2 2) (0 2 5)))
      (check= (hits '(document (concat "ab" (rsub "c") "ab+ab"))
                    '(concat "a" (wildcard "x") "b"))
              '((0 0 0) (0 0 2) (0 2 0) (0 2 2) (0 2 3) (0 2 5)))
      (check= (hits '(document (concat "x" (rsub "c") "ba+a" (rsub "d")))
                    '(concat "a" (wildcard "x")))
              '((0 2 1) (0 3 1)))))
  ;; the search goes on after such a string: a scan which stopped there
  ;; starts again after it
  (check-both '(document (concat "x" (rsub "c") "ab" (rsub "1") "a" (rsub "2")
                                 "b"))
              '(concat "a" (wildcard "x") "b")
              '((0 2 0) (0 2 2) (0 4 0) (0 6 1)))
  ;; when replacing, a string of the pattern must match a whole string
  (with-search-mode 'replace
    (lambda ()
      (check= (hits '(document (concat "x" (rsub "c") "aba+b"))
                    '(concat "a" (wildcard "x") "b"))
              '((0 2 0) (0 2 5))))))

;; the first string of a pattern may overlap itself in the string where
;; the match starts: aa_x in aaa_x starts at the second a (#477)
(define (test-overlapping-strings)
  (check-group "overlapping strings")
  (check-both '(document (concat "aaa" (rsub "x")))
              '(concat "aa" (rsub "x"))
              '((0 0 1) (0 1 1)))
  (check-both '(document (concat "aa" (rsub "x")))
              '(concat "aa" (rsub "x"))
              '((0 0 0) (0 1 1)))
  ;; a symbol is one character: at the end of the string it is not an a,
  ;; and the occurrences of <alpha><alpha> overlap in three of them
  (check-both '(document (concat "a<alpha>" (rsub "x")))
              '(concat "a" (rsub "x"))
              '())
  (check-both '(document (concat "<alpha><alpha><alpha>" (rsub "x")))
              '(concat "<alpha><alpha>" (rsub "x"))
              '((0 0 7) (0 1 1)))
  ;; before a wildcard, the first occurrence: the match furthest to the left,
  ;; as in the same text as one string
  (check-both '(document "aaaxyb") '(concat "aa" (wildcard "x") "b")
              '((0 0) (0 6)))
  (check-both '(document (concat "aaa" (frac "x" "y") "b"))
              '(concat "aa" (wildcard "x") "b")
              '((0 0 0) (0 2 1)))
  ;; also when the pattern starts with strings and wildcards
  (check-both '(document (concat "abab" (frac "x" "y") "c"))
              '(concat "a" (wildcard "x") "b" (wildcard "y") "c")
              '((0 0 0) (0 2 1)))
  ;; when searching, as in the same text as one string
  (with-search-mode 'search
    (lambda ()
      (check= (hits '(document "aaaaaxya") '(concat "aa" (wildcard "x") "a"))
              '((0 0) (0 3) (0 3) (0 8)))
      (check= (hits '(document (concat "aaaaa" (frac "x" "y") "a"))
                    '(concat "aa" (wildcard "x") "a"))
              '((0 0 0) (0 0 3) (0 0 3) (0 2 1)))))
  (with-preference "case-insensitive-match" "on"
    (lambda ()
      (check-both '(document (concat "xAAA" (rsub "x")))
                  '(concat "aa" (rsub "x"))
                  '((0 0 2) (0 1 1))))))

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
            ;; The skipping of failed scans starts again only after a
            ;; child where such a scan stops; here the first hit, extended
            ;; to the end of the formula by the trailing wildcard, leaves
            ;; none (starting again after every hit took 2 s on the dense
            ;; formula)
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

;; between two elements which are not wildcards, a string of the pattern
;; matches a whole string of the formula, as one match: aa does not match
;; aaaa (#480); a part with a wildcard matches when it covers the string
(define (test-middle-strings)
  (check-group "middle strings")
  (check-both '(document (concat "c" (rsub "x") "aaaa" (rsub "y")))
              '(concat (rsub "x") "aa" (rsub "y"))
              '())
  (check-both '(document (concat "c" (rsub "x") "aa" (rsub "y")))
              '(concat (rsub "x") "aa" (rsub "y"))
              '((0 1 0) (0 3 1)))
  (check-both '(document (concat "c" (rsub "x") "AA" (rsub "y")))
              '(concat (rsub "x") "aa" (rsub "y"))
              '())
  (with-preference "case-insensitive-match" "on"
    (lambda ()
      (check-both '(document (concat "c" (rsub "x") "AA" (rsub "y")))
                  '(concat (rsub "x") "aa" (rsub "y"))
                  '((0 1 0) (0 3 1)))))
  (check-both '(document (concat "c" (rsub "x") "abaa" (rsub "y")))
              '(concat (rsub "x") "a" (wildcard "w") "a" (rsub "y"))
              '((0 1 0) (0 3 1)))
  (check-both '(document (concat "c" (rsub "x") "abab" (rsub "y")))
              '(concat (rsub "x") "a" (wildcard "w") "a" (rsub "y"))
              '())
  ;; in linear time: a test which tried every place of every string (a
  ;; first version of #480) took seconds in a string of 60000 characters
  (let* ((abc (apply string-append (map (lambda (i) "abc") (iota 20000))))
         (doc `(document
                 (concat (rsub "1") ,(string-append abc "x") (rsub "2"))))
         (what '(concat (rsub "1") "a" (wildcard "x") "b" (wildcard "y") "c"
                        (rsub "2"))))
    (for-each
      (lambda (mode)
        (with-search-mode mode
          (lambda ()
            (with r (time-of (lambda () (hits doc what)))
              (check= (cdr r) '())
              (check-true (< (car r) 500))))))
      '(search replace))))

;; a string of 100000 characters and a pattern of a string and a wildcard,
;; with partial matches: the starts were tried by a recursion per character,
;; which overflowed the stack and crashed TeXmacs (#479)
(define (test-long-strings)
  (check-group "long strings")
  (let ((doc `(document ,(string-append (make-string 100000 #\a) "bc"))))
    (with-search-mode 'search
      (lambda ()
        (check= (hits doc '(concat "b" (wildcard "x")))
                '((0 100000) (0 100002)))
        (check= (hits doc '(concat "q" (wildcard "x"))) '()))))
  ;; and in linear time: each start rescanned the string after it (10 s
  ;; for a<wildcard>b in 100000 times a)
  (let ((doc `(document ,(make-string 100000 #\a))))
    (with-search-mode 'search
      (lambda ()
        (with r (time-of
                  (lambda () (hits doc '(concat "a" (wildcard "x") "b"))))
          (check= (cdr r) '())
          (check-true (< (car r) 500)))))))

(tm-define (search-test-failures)
  (check-suite "search")
  (test-text)
  (test-text-wildcards)
  (test-tags)
  (test-formulas)
  (test-formula-wildcards)
  (test-hits-after-a-hit)
  (test-wildcards-and-several-hits)
  (test-hits-of-a-string-after-a-tag)
  (test-overlapping-strings)
  (test-paragraphs)
  (test-limit-and-position)
  (test-middle-strings)
  (test-speed)
  (test-long-strings)
  (check-end))
