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
;; In the text, searching and replacing match the same way (#495): a match
;; may start and end inside a string, and a wildcard takes the shortest
;; run, maybe empty, except a leading (trailing) wildcard, which extends the
;; hit to the start (end) of the formula, or of the paragraph in a pattern
;; of several paragraphs. They differ in the arguments of tags. Edit > Search
;; (toolbar or widget) sets the preference "search-and-replace" to "off":
;; the arguments then follow the preferences allow-blank-match,
;; allow-initial-match, allow-partial-match and allow-injective-match, all
;; on by default, so that a string of the pattern may match a part of an
;; argument, an empty argument matches any argument, and a tag matches a
;; tag whose arguments contain those of the pattern. Edit > Replace sets it
;; to "on": the arguments follow allow-blank-replace, ..., all off by
;; default, and a string of the pattern must match a whole argument. Both
;; allow cascaded matches (a pattern inside an argument). The checks set
;; preferences in memory only, and restore them: nothing is saved.
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
;; both widgets, each occurrence (the replace widget took the whole string
;; only, #495)
(define (test-text-wildcards)
  (check-group "text wildcards")
  (check-both '(document "abc axc ac" (concat "ab" (em "x") "c"))
              '(concat "a" (wildcard "x") "c")
              '((0 0) (0 3) (0 4) (0 7) (0 8) (0 10) (1 0 0) (1 2 1)))
  (check-both '(document "abc xbc") '(concat (wildcard "x") "bc")
              '((0 0) (0 3) (0 3) (0 7)))
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

;; a pattern with a region (std ? in the widgets) is matched as the same
;; pattern with the content of the region, by the rules of any pattern, and
;; the region of each match is selected. Its parts were matched child by
;; child, with other children in between, and its paragraphs and strings
;; by the rule of the arguments of tags (#500)
(define (test-regions)
  (check-group "regions")
  (let ((y '(rsub "y")))
    ;; the parts are adjacent
    (check-both `(document (concat "abx" ,y "zc"))
                `(concat "ab" (select-region ,y) "c")
                '())
    ;; in text, Search and Replace match the same way
    (with-search-mode 'replace
      (lambda ()
        (check= (hits '(document (concat "xab" (rsub "d")) (rsub "c"))
                      '(document (concat "ab" (rsub "d"))
                                 (select-region (rsub "c"))))
                '((1 0) (1 1)))))
    ;; a string of the pattern does not match a string inside a tag
    (with-search-mode 'search
      (lambda ()
        (check= (hits '(document (concat "y" (strong "xab") (rsub "c")))
                      '(concat "ab" (select-region (rsub "c"))))
                '())))
    ;; a region inside a string, a region of several children
    (check-both '(document "xabcx" (concat "zabc" ,y))
                '(concat "a" (select-region "b") "c")
                '((0 2) (0 3) (1 0 2) (1 0 3)))
    (check-both `(document (concat "xab" ,y (rsub "z") "cz"))
                `(concat "ab" (select-region (wildcard "w")) "c")
                '((0 1 0) (0 2 1)))
    ;; an empty region in a formula takes exactly one character or one tag,
    ;; never nothing (the maintainer's decision); [*] takes any run
    (check-both '(document "abc abxc") '(concat "ab" (select-region ""))
                '((0 2) (0 3) (0 6) (0 7)))
    (check-both '(document (concat "ab" (rsub "1") (rsub "2")))
                '(concat "ab" (select-region ""))
                '((0 1 0) (0 1 1)))
    (check-both '(document "abc") '(concat "ab" (select-region "") "c") '())
    (check-both '(document "abxc") '(concat "ab" (select-region "") "c")
                '((0 2) (0 3)))
    (check-both '(document "abcc ababxc") '(concat "ab" (select-region "") "c")
                '((0 2) (0 3) (0 9) (0 10)))
    (check-both `(document (concat "xab" ,y (rsub "z") "cz"))
                `(concat "ab" (select-region "") "c")
                '())
    (check-both `(document (concat "xab" ,y "cz" (frac "abxc" "1")))
                `(concat "ab" (select-region "") "c")
                '((0 1 0) (0 1 1) (0 3 0 2) (0 3 0 3)))
    ;; in a pattern of several paragraphs, the part with the empty region
    ;; is matched with the tokens of its paragraph (the search, with a
    ;; wildcard there, gave other starts and ends)
    (check-both '(document "aab" "c")
                '(document (concat "a" (select-region "")) "c")
                '((0 2) (0 3)))
    (check-both '(document "xa" "cbd")
                '(document "a" (concat "c" (select-region "")))
                '((1 1) (1 2)))
    (check-both '(document "xa" "cbd" "e")
                '(document "a" (concat "c" (select-region "") "d") "e")
                '((1 1) (1 2)))
    (check-both '(document "xa" "cbxd" "e")
                '(document "a" (concat "c" (select-region "") "d") "e")
                '())
    ;; a trailing wildcard after an empty region takes nothing (the match
    ;; is the shortest), so that a[]* gives a region per a and its next
    ;; character, while a[b]* gives one region, the wildcard taking the rest
    ;; of the formula (#454)
    (check-both '(document "abcabc")
                '(concat "a" (select-region "") (wildcard "w"))
                '((0 1) (0 2) (0 4) (0 5)))
    (check-both '(document "abcabc")
                '(concat "a" (select-region "b") (wildcard "w"))
                '((0 1) (0 2)))
    ;; in a tag argument with Replace, the whole argument
    (with-search-mode 'replace
      (lambda ()
        (check= (hits '(document (frac (concat "zab" (rsub "y")) "c")
                                 (frac (concat "ab" (rsub "y")) "c"))
                      '(frac (concat "ab" (select-region "")) "c"))
                '((1 0 1 0) (1 0 1 1)))))
    ;; a region which starts in a string and ends after a tag
    (check-both `(document (concat "xab" ,y "cd"))
                `(concat "a" (select-region (concat "b" ,y "c")) "d")
                '((0 0 2) (0 2 1)))
    ;; a hit inside a tag, carried to the ends of the formula by a leading
    ;; and a trailing wildcard: the region of the first hit inside the tag
    (check-both `(document (concat ,y (frac "xaba" "a")))
                '(concat (wildcard "w") (select-region "a") (wildcard "v"))
                '((0 1 0 1) (0 1 0 2)))
    ;; the leftmost region: a hit inside an earlier tag, which the
    ;; wildcards carry to the ends of the formula, comes first
    (check-both '(document (concat "b" (frac "xa" "1") "a"))
                '(concat (wildcard "w") (select-region "a") (wildcard "v"))
                '((0 1 0 1) (0 1 0 2)))
    ;; several regions; regions at the start and at the end of the pattern
    (check-both '(document "xaybz abcd")
                '(concat (select-region "a") (wildcard "w")
                         (select-region "b"))
                '((0 1) (0 2) (0 3) (0 4) (0 6) (0 7) (0 7) (0 8)))
    (check-both '(document "xabcd") '(concat (select-region "a") "bc")
                '((0 1) (0 2)))
    (check-both '(document "xabcd") '(concat "bc" (select-region "d"))
                '((0 4) (0 5)))
    ;; case-insensitive
    (with-preference "case-insensitive-match" "on"
      (lambda ()
        (check-both '(document "xABcx") '(concat "a" (select-region "b") "c")
                    '((0 2) (0 3)))))
    ;; a region in a tag inside a formula of the pattern
    (check-both '(document (concat "x" (strong "abc") "y" (strong "abc")))
                '(concat "y" (strong (concat "a" (select-region "b") "c")))
                '((0 3 0 1) (0 3 0 2)))
    ;; a document pattern with the region in its first or middle paragraph
    (check-both '(document "xab" "cd" "e")
                '(document (concat "a" (select-region "b")) "cd")
                '((0 2) (0 3)))
    (check-both '(document "xab" "cd" "ef")
                '(document "ab" (concat "c" (select-region "d")) "e")
                '((1 1) (1 2)))
    ;; in linear time: a wildcard before a region which fails tried the
    ;; rest of the hit from each place (4 s for 64000 characters)
    (let ((doc `(document (concat ,(make-string 64000 #\b) (frac "bxc" "1")))))
      (for-each
        (lambda (mode)
          (with-search-mode mode
            (lambda ()
              (with r (time-of
                        (lambda ()
                          (hits doc '(concat (wildcard "u")
                                             (select-region
                                               (concat "b" (wildcard "w") "c"))
                                             (wildcard "v")))))
                (check= (cdr r) '((0 1 0 0) (0 1 0 3)))
                (check-true (< (car r) 500)))
              (with r (time-of
                        (lambda ()
                          (hits doc '(concat "b" (select-region "") "q"))))
                (check= (cdr r) '())
                (check-true (< (car r) 500))))))
        '(search replace)))
    ;; in a tag of the pattern: the tag in its place in the text
    (check-both '(document (concat "q" (frac "1" "2") "b" (frac "3" "2") "c"))
                '(concat (frac (select-region "") "2") "c")
                '((0 3 0 0) (0 3 0 1)))
    ;; in an argument of a tag: as a string, anywhere with Search, the whole
    ;; argument with Replace
    (let ((pat '(frac (concat "a" (select-region "b")) "c")))
      (with-search-mode 'search
        (lambda ()
          (check= (hits '(document (frac "xab" "c")) pat)
                  '((0 0 2) (0 0 3)))))
      (with-search-mode 'replace
        (lambda ()
          (check= (hits '(document (frac "xab" "c") (frac "ab" "c")) pat)
                  '((1 0 1) (1 0 2))))))
    ;; the arguments of a tag one by one with Replace, as for other patterns
    (with-search-mode 'replace
      (lambda ()
        (check= (hits '(document (concat (tuple "a" "b" "c") (tuple "a" "b")))
                      '(tuple (select-region "a") "b"))
                '((0 1 0 0) (0 1 0 1)))))))

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
  ;; in the middle: any content, each f(...) and g(...) (when replacing,
  ;; ")" only matched the last ")" before #495)
  (check-both '(document (concat "f(" (frac "1" "2") ")+g(" (rsub "n") ")"))
              '(concat "(" (wildcard "x") ")")
              '((0 0 1) (0 2 1) (0 2 3) (0 4 1)))
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
  ;; an empty first paragraph of a document pattern matches the end of a
  ;; paragraph, as when the selection starts with a paragraph break (it
  ;; never returned, and later only matched an empty paragraph)
  (check-both '(document "" "abc") '(document "" "abc") '((0 0) (1 3)))
  (check-both '(document "x" "abc") '(document "" "abc") '((0 1) (1 3)))
  (check-both '(document "x" "zabc") '(document "" "abc") '())
  ;; an empty pattern is found nowhere with the matching of Replace
  (with-search-mode 'replace
    (lambda () (check= (hits '(document "abc") "") '()))))

;; a hit inside a string, then one which starts later in the same string
;; and goes on in the next children: both (#461), also when replacing (one
;; match went from the first f( to the end before #495).
(define (test-hits-after-a-hit)
  (check-group "hits after a hit")
  (check-both '(document (concat "f(x)+f(" (frac "1" "2") ")"))
              '(concat "f(" (wildcard "x") ")")
              '((0 0 0) (0 0 4) (0 0 5) (0 2 1)))
  (check-both '(document (concat "axb+a" (rsub "1") "b+axb"))
              '(concat "a" (wildcard "x") "b")
              '((0 0 0) (0 0 3) (0 0 4) (0 2 1) (0 2 2) (0 2 5))))

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
  (check-both '(document "aba+b") '(concat "a" (wildcard "x") "b")
              '((0 0) (0 2) (0 2) (0 5)))
  (check-both '(document (concat "x" (rsub "c") "aba+b"))
              '(concat "a" (wildcard "x") "b")
              '((0 2 0) (0 2 2) (0 2 2) (0 2 5)))
  (check-both '(document (concat "ab" (rsub "c") "ab+ab"))
              '(concat "a" (wildcard "x") "b")
              '((0 0 0) (0 0 2) (0 2 0) (0 2 2) (0 2 3) (0 2 5)))
  (check-both '(document (concat "x" (rsub "c") "ba+a" (rsub "d")))
              '(concat "a" (wildcard "x"))
              '((0 2 1) (0 3 1)))
  ;; the search goes on after such a string: a scan which stopped there
  ;; starts again after it
  (check-both '(document (concat "x" (rsub "c") "ab" (rsub "1") "a" (rsub "2")
                                 "b"))
              '(concat "a" (wildcard "x") "b")
              '((0 2 0) (0 2 2) (0 4 0) (0 6 1))))

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
  ;; as in the same text as one string
  (check-both '(document "aaaaaxya") '(concat "aa" (wildcard "x") "a")
              '((0 0) (0 3) (0 3) (0 8)))
  (check-both '(document (concat "aaaaa" (frac "x" "y") "a"))
              '(concat "aa" (wildcard "x") "a")
              '((0 0 0) (0 0 3) (0 0 3) (0 2 1)))
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

;; a pattern which starts with strings and wildcards: the match starts at
;; the first place where the rest matches, as for a string (#481); before
;; a tag, it ends at the end of the string
(define (test-strings-and-wildcards-first)
  (check-group "strings and wildcards first")
  (check-both '(document (concat "aaa" (rsub "y")))
              '(concat "a" (wildcard "x") "a" (rsub "y"))
              '((0 0 0) (0 1 1)))
  (check-both '(document (concat "aaaa" (rsub "y")))
              '(concat "aa" (wildcard "x") "a" (rsub "y"))
              '((0 0 0) (0 1 1)))
  (check-both '(document (concat "xab" (rsub "y")))
              '(concat "a" (wildcard "x") "b" (rsub "y"))
              '((0 0 1) (0 1 1)))
  (check-both '(document (concat "xaa" (rsub "y")))
              '(concat "a" (wildcard "x") "b" (rsub "y"))
              '())
  ;; two strings in a row in a pattern (from Scheme, or left by an empty
  ;; string between them) are one string
  (check-both '(document (concat "aab" (rsub "1")))
              '(concat "a" "b" (rsub "1"))
              '((0 0 1) (0 1 1)))
  (check-both '(document (concat "xaab" (rsub "1")))
              '(concat "a" "" "b" (wildcard "x") (rsub "1"))
              '((0 0 2) (0 1 1)))
  ;; also with a region: its text is xab, adjacent (each child of the
  ;; pattern was matched with a child of its own, with gaps, #500). (In the
  ;; first search of a session, this needs the patterns prepared after
  ;; initialize_search, which makes the label select-region; this check,
  ;; not the first search of the suite, does not test that.)
  (check-both '(document (concat "xab" (rsub "1")))
              '(concat (select-region "x") "a" "b")
              '((0 0 0) (0 0 1)))
  (check-both '(document (concat "x" (frac "1" "2") "a" (rsub "1") "b"))
              '(concat (select-region "x") "a" "b")
              '())
  ;; a leading wildcard and strings before a tag: the string ends as the
  ;; part does, and the match starts at the start of the formula (#489)
  (check-both '(document (concat "bbb" (rsub "y")))
              '(concat (wildcard "x") "bb" (rsub "y"))
              '((0 0 0) (0 1 1)))
  (check-both '(document (concat (rsub "z") "cbbb" (rsub "y")))
              '(concat (wildcard "x") "bb" (rsub "y"))
              '((0 0 0) (0 2 1)))
  (check-both '(document (concat "xabb" (rsub "y")))
              '(concat (wildcard "w") "a" (wildcard "v") "b" (rsub "y"))
              '((0 0 0) (0 1 1)))
  (check-both '(document (concat "bbx" (rsub "y")))
              '(concat (wildcard "x") "bb" (rsub "y"))
              '())
  ;; before a wildcard: as the same text as one string, abxyacabxyc
  (check-both '(document (concat "ab" (frac "x" "y") "acab"
                                 (frac "x" "y") "c"))
              '(concat "a" (wildcard "x") "b" (wildcard "y") "c")
              '((0 0 0) (0 2 2) (0 2 2) (0 4 1))))

;; a match across the children which ends inside a string: the hits of
;; the string after it, as in the same text as one string, xa12bababa
;; (#482), with both matchings (with the replace matching, the match went
;; on to the last ba before #495)
(define (test-hits-after-a-match)
  (check-group "hits after a match")
  (let ((doc '(document (concat "xa" (frac "1" "2") "bababa")))
        (what '(concat "a" (wildcard "x") "ba")))
    (check-both '(document "xa12bababa") what '((0 1) (0 6) (0 7) (0 10)))
    (check-both doc what '((0 0 1) (0 2 2) (0 2 3) (0 2 6)))))

;; a string of 100000 characters and a pattern of a string and a wildcard,
;; with partial matches: the starts were tried by a recursion per character,
;; which overflowed the stack and crashed TeXmacs (#479)
(define (test-long-strings)
  (check-group "long strings")
  (let ((doc `(document ,(string-append (make-string 100000 #\a) "bc"))))
    (check-both doc '(concat "b" (wildcard "x")) '((0 100000) (0 100002)))
    (check-both doc '(concat "q" (wildcard "x")) '()))
  ;; and in linear time: each start rescanned the string after it (10 s
  ;; for a<wildcard>b in 100000 times a)
  (let ((doc `(document ,(make-string 100000 #\a))))
    (for-each
      (lambda (mode)
        (with-search-mode mode
          (lambda ()
            (with r (time-of
                      (lambda () (hits doc '(concat "a" (wildcard "x") "b"))))
              (check= (cdr r) '())
              (check-true (< (car r) 500))))))
      '(search replace))))

;; a part of the pattern after a tag (or a leading wildcard) and before a
;; wildcard covers the start of its string (or any piece of it): the
;; wildcard takes the rest. With the replace matching, it had to cover the
;; whole string (#488)
(define (test-parts-before-wildcards)
  (check-group "parts before wildcards")
  (let ((x '(rsub "x")) (y '(rsub "y")) (z '(rsub "z")) (q '(rsub "q")))
    (check-both `(document (concat "c" ,x "abba" ,y))
                `(concat ,x (wildcard "w") "bb" (wildcard "v") ,y)
                '((0 1 0) (0 3 1)))
    (check-both `(document (concat "c" ,x "babbbbbbabbaabbaaab" ,y))
                `(concat ,x (wildcard "w") "bb" (wildcard "v") ,y)
                '((0 1 0) (0 3 1)))
    (check-both `(document (concat ,x "abba" ,q "c" ,y))
                `(concat ,x (wildcard "w") "b" (wildcard "v") ,y)
                '((0 0 0) (0 4 1)))
    (check-both `(document (concat "a" ,y "aabbb" ,x ,z))
                `(concat ,y (wildcard "w") "ab" (wildcard "v") ,x)
                '((0 1 0) (0 3 1)))
    (check-both `(document (concat "abb" ,z "bab" ,x ,x))
                `(concat ,z (wildcard "w") "ba" (wildcard "v") ,x ,x)
                '((0 1 0) (0 4 1)))
    (check-both `(document (concat ,y "aabaa" ,z "bb" ,x "aaa"))
                `(concat ,y "aa" (wildcard "w") "b" (wildcard "v") ,z)
                '((0 0 0) (0 2 1)))
    ;; a leading wildcard
    (check-both `(document (concat "aabbab" ,y "a" ,y))
                `(concat (wildcard "z") "aa" (wildcard "y") "a" ,y)
                '((0 0 0) (0 3 1)))
    (check-both `(document (concat "<alpha>AaaA" ,z " A" ,y))
                `(concat (wildcard "z") "a" (wildcard "x") "A" ,y)
                '((0 0 0) (0 3 1)))
    ;; and no more than that
    (check-both `(document (concat ,x "abab" ,y))
                `(concat ,x (wildcard "w") "bb" (wildcard "v") ,y) '())
    (check-both `(document (concat ,x "abba" ,y))
                `(concat ,x "bb" (wildcard "v") ,y) '())
    (check-both `(document (concat "aabbab" ,y "b" ,y))
                `(concat (wildcard "z") "aa" (wildcard "y") "a" ,y) '())
    (check-both `(document (concat "bbab" ,y "a" ,y))
                `(concat (wildcard "z") "aa" (wildcard "y") "a" ,y) '())))

;; in the text, Search and Replace match the same way: a match may start
;; and end inside a string, and a wildcard takes the shortest run, maybe
;; empty. With Replace, a pattern with a wildcard had to cover its whole
;; string, except one which started with a string and a wildcard (#495).
;; A string of the pattern still matches a whole argument of a tag when
;; replacing
(define (test-text-same-in-both)
  (check-group "text, the same in both")
  (let ((x '(rsub "x")) (y '(rsub "y")) (z '(rsub "z")))
    (check-both `(document (concat ,x "bab")) `(concat ,x (wildcard "w") "b")
                '((0 0 0) (0 1 1)))
    (check-both `(document (concat ,x "bab" ,z "b"))
                `(concat ,x (wildcard "w") "b")
                '((0 0 0) (0 1 1)))
    (check-both `(document (concat ,x "abab")) `(concat ,x (wildcard "w") "ab")
                '((0 0 0) (0 1 2)))
    (check-both `(document (concat "abab" ,x)) `(concat "b" (wildcard "w") ,x)
                '((0 0 1) (0 1 1)))
    (check-both `(document (concat "abab" ,x))
                `(concat "b" (wildcard "w") "a" (wildcard "v") ,x)
                '((0 0 1) (0 1 1)))
    (check-both `(document (concat ,x ,y)) `(concat ,x (wildcard "w") ,y)
                '((0 0 0) (0 1 1))))
  (check-both '(document (concat "xabbx")) '(concat "a" (wildcard "w") "b")
              '((0 0 1) (0 0 3)))
  (check-both '(document (concat "bab")) '(concat (wildcard "w") "b")
              '((0 0 0) (0 0 1)))
  (check-both '(document (concat "xab")) '(concat "a" (wildcard "w") "b")
              '((0 0 1) (0 0 3)))
  (check-both '(document (concat "abab")) '(concat "a" (wildcard "w") "b")
              '((0 0 0) (0 0 2) (0 0 2) (0 0 4)))
  (check-both '(document (concat (frac "a" "c"))) '(frac "a" (wildcard "w"))
              '((0 0 0) (0 0 1)))
  ;; patterns of several paragraphs: the part in the first one reaches its
  ;; end, though the shortest hit (ab) does not; and select-region
  (check-both '(document "abxb" "c")
              '(document (concat "a" (wildcard "w") "b") "c")
              '((0 0) (1 1)))
  (check-both '(document "c" "abxb" "d")
              '(document "c" (concat "a" (wildcard "w") "b") "d")
              '((0 0) (2 1)))
  (check-both '(document (concat "zab" (rsub "x") "b") "c")
              '(document (concat "a" (wildcard "w") "b") "c")
              '((0 0 1) (1 1)))
  (check-both '(document "abx" "c")
              '(document (concat "a" (wildcard "w") "b") "c")
              '())
  (check-both '(document (concat "xa" (rsub "b")))
              '(concat "a" (select-region (rsub "b")))
              '((0 1 0) (0 1 1)))
  (check-both '(document (concat "xa" (rsub "b") "cy"))
              '(concat "a" (select-region (rsub "b")) "c")
              '((0 1 0) (0 1 1)))
  (check-both '(document "xab" (rsub "y"))
              '(document "ab" (select-region (rsub "y")))
              '((1 0) (1 1)))
  ;; in the argument of a tag of the pattern, a whole string when
  ;; replacing, also inside another tag
  (with-search-mode 'replace
    (lambda ()
      (check= (hits '(document (frac (concat "xa" (rsub "y")) "b"))
                    '(frac (concat "a" (select-region (rsub "y"))) "b"))
              '())
      (check= (hits '(document (frac (strong (concat "xa" (rsub "y"))) "b"))
                    '(frac (concat "a" (select-region (rsub "y"))) "b"))
              '())))
  ;; the leftmost start in the first paragraph; a middle paragraph is
  ;; covered by one match, not by several hits
  (check-both '(document "abab" "c")
              '(document (concat "a" (wildcard "x") "b") "c")
              '((0 0) (1 1)))
  (check-both '(document "abab" "c")
              '(document (concat (wildcard "x") "b") "c")
              '((0 0) (1 1)))
  (check-both '(document "ab" "abab" "ab") '(document "ab" "ab" "ab") '())
  ;; a second match may start in the paragraph where the first one ended,
  ;; after it; not with a leading wildcard, which reaches the start of the
  ;; paragraph
  (check-both '(document "ab" "aab" "a")
              '(document (concat "a" (wildcard "x") "b") "a")
              '((0 0) (1 1) (1 1) (2 1)))
  (check-both '(document "abxab" "abxab" "abxab" "abxab" "abxab")
              '(document (concat (wildcard "w") "b")
                         (concat (wildcard "w") "x" (wildcard "w")) "ab")
              '((0 0) (2 2)))
  (check-both '(document "ab" "bbb" "a") '(document "ab" "b" "a") '())
  ;; an empty part: the end (start) of a paragraph, or an empty paragraph
  (check-both '(document "xab" "def") '(document "ab" "") '((0 1) (1 0)))
  (check-both '(document "ab" "defx") '(document "" "def") '((0 2) (1 3)))
  (check-both '(document "a" (rsub "x") "z") '(document "a" "" "z") '())
  ;; also in a document inside a tag of a paragraph
  (check-both '(document "a" (theorem (document "b" "c")) "d")
              '(document "" "")
              '((0 1) (1 0) (1 0 0 1) (1 0 1 0) (1 1) (2 0)))
  (check-both '(document "a" "" "b") '(document "a" "" "b") '((0 0) (2 1)))
  (with-search-mode 'search
    (lambda ()
      (check= (hits '(document (concat (frac "ab" "c")))
                    '(frac "a" (wildcard "w")))
              '((0 0 0) (0 0 1)))))
  (with-search-mode 'replace
    (lambda ()
      (check= (hits '(document (concat (frac "ab" "c")))
                    '(frac "a" (wildcard "w")))
              '()))))

;; formulas of 40000 children and more: the scan for the start of a
;; match, and a wildcard which takes many children, went from one child to
;; the next by a recursion, which overflowed the stack and crashed TeXmacs
;; (#487)
(define (repeated n l)
  (append-map (lambda (k) l) (iota n)))

(define (test-long-concats)
  (check-group "long formulas")
  (let ((ab `(concat ,@(repeated 20000 '("ab" (rsub "1")))))
        (xz `(concat "x" ,@(repeated 20000 '((rsub "1") (rsub "2"))) "z")))
    (check-both `(document ,ab) '(concat (rsub "1") "ab" (rsub "2")) '())
    (check-both `(document ,(append ab '("ab" (rsub "2"))))
                '(concat (rsub "1") "ab" (rsub "2"))
                '((0 39999 0) (0 40001 1)))
    (check-both `(document ,ab) '(concat (wildcard "x") (rsub "2")) '())
    (check-both `(document ,xz) '(concat "x" (wildcard "y") "z")
                '((0 0 0) (0 40001 1)))
    (check-both `(document ,xz) '(concat "x" (wildcard "y") "q") '())))

(tm-define (search-test-failures)
  (check-suite "search")
  (test-text)
  (test-text-wildcards)
  (test-tags)
  (test-regions)
  (test-formulas)
  (test-formula-wildcards)
  (test-hits-after-a-hit)
  (test-wildcards-and-several-hits)
  (test-hits-of-a-string-after-a-tag)
  (test-overlapping-strings)
  (test-paragraphs)
  (test-limit-and-position)
  (test-middle-strings)
  (test-strings-and-wildcards-first)
  (test-hits-after-a-match)
  (test-speed)
  (test-long-strings)
  (test-long-concats)
  (test-parts-before-wildcards)
  (test-text-same-in-both)
  (check-end))
