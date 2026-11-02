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
;; default, and a string of the pattern must match a whole argument. A
;; formula in an argument of the pattern is text, as in a paragraph, and
;; follows the same flags: anywhere in the argument, or the whole argument
;; (#501). Both allow cascaded matches (a pattern inside an argument).
;; The checks set preferences in memory only, and restore them: nothing is
;; saved.
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
  (:use (check check-lib)
        (utils library cursor)
        (generic search-widgets)))

;; the private bindings of the widget, which the tests of Replace use
(define widgets (resolve-module '(generic search-widgets)))
(define (widget x) (eval x widgets))

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

(define (with-preference-value which val thunk)
  ;; the value of thunk, with the preference set (in memory only, and
  ;; restored also when thunk fails)
  (let* ((had? (cpp-has-preference? which))
         (old (get-preference which)))
    (dynamic-wind
      (lambda () (cpp-set-preference which val))
      thunk
      (lambda ()
        (if had? (cpp-set-preference which old)
            (cpp-reset-preference which))))))

(define (with-search-mode-value mode thunk)
  ;; the value of thunk, with the matching of mode
  (with-preference-value "search-and-replace"
    (if (== mode 'search) "off" "on") thunk))

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
    ;; a candidate which fails does not hide a match which overlaps it; a
    ;; leading empty region is one token, not a wildcard; a second match
    ;; does not reuse the end of the first one
    (check-both '(document "b" "cbd" "cb")
                '(document (concat "b" (select-region "")) "cb")
                '((1 2) (1 3)))
    (check-both '(document "x" "bTa" "aTbcaab" "aa")
                '(document (concat (select-region "") (wildcard "w") "a"
                                   (wildcard "v"))
                           "a")
                '((1 0) (1 1) (2 1) (2 2)))
    (check-both '(document "axb" "caab" "ca")
                '(document (concat "a" (select-region "") "b") "ca")
                '((0 1) (0 2)))
    ;; a paragraph of the pattern which is only an empty region is a whole
    ;; paragraph, never an empty one
    (check-both '(document "xa" "") '(document "a" (select-region "")) '())
    (check-both '(document "xa" "yz") '(document "a" (select-region ""))
                '((1 0) (1 2)))
    (check-both '(document "a" "yz" "b")
                '(document "a" (select-region "") "b")
                '((1 0) (1 2)))
    ;; in linear time: the hits inside the tags were sorted with those of
    ;; the formula one by one (3.6 s for 16000 terms)
    (with doc (stree->tree
                `(document (concat ,@(append-map (lambda (k)
                                                   '("ab" (frac "ab" "1")))
                                                 (iota 16000)))))
      (with r (time-of (lambda ()
                         (tree-search-tree doc '(concat "a" (select-region ""))
                                           '() 100000)))
        (check= (length (cdr r)) 64000)
        (check-true (< (car r) 500))))
    ;; a first part with a leading wildcard reaches the start of its
    ;; paragraph: no second match starts where the first one ended, also
    ;; without an empty region
    (check-both '(document "a" "bxa" "bx")
                '(document (concat (wildcard "w") (select-region "a"))
                           (concat "b" (select-region "x")))
                '((0 0) (0 1) (1 1) (1 2)))
    (check-both '(document "a" "bxa" "bx")
                '(document (concat (wildcard "w") (select-region "a"))
                           (concat "b" (select-region "")))
                '((0 0) (0 1) (1 1) (1 2)))
    ;; the documents inside the paragraphs of a match are searched too
    (check-both '(document (concat (strong (document "xa" "bc")) "ya") "bc")
                '(document (select-region "a") (concat "b" (select-region "")))
                '((0 0 0 0 1) (0 0 0 0 2) (0 0 0 1 1) (0 0 0 1 2)
                  (0 1 1) (0 1 2) (1 1) (1 2)))
    ;; and the limit of hits holds in the loop over the paragraphs
    (with doc (stree->tree `(document ,@(map (lambda (i) "ab") (iota 400))))
      (with r (tree-search-tree
                doc '(document (concat "a" (select-region ""))
                               (concat "a" (select-region "")))
                '() 10)
        (check-true (and (>= (length r) 10) (< (length r) 100)))))
    ;; no region inside a tag which a hit of the formula already covers
    (check-both '(document (concat "x" (rsup "ba") "a"))
                '(concat (select-region "") (wildcard "w") "a")
                '((0 0 0) (0 0 1)))
    ;; a tag alone is a formula of one token
    (check-both '(document (frac "1" "2"))
                '(concat (select-region "") (wildcard "w"))
                '((0 0) (0 1)))
    ;; in tag arguments: a region which is the whole argument selects its
    ;; content anywhere in the argument, with both (as (strong [a]) in the
    ;; tag checks, the example of the manual); a document pattern matches
    ;; the whole argument with Replace
    (check-both '(document (frac "abc" "1"))
                '(frac (select-region "b") "1")
                '((0 0 1) (0 0 2)))
    (with-search-mode 'replace
      (lambda ()
        (check= (hits '(document (strong (document "xay" "bz"))
                                 (strong (document "ay" "b")))
                      '(strong (document (concat "a" (select-region "y")) "b")))
                '((1 0 0 1) (1 0 0 2)))
        (check= (hits '(document (strong (document "xay" "bz"))
                                 (strong (document "ay" "b")))
                      '(strong (document (concat "a" (select-region "")) "b")))
                '((1 0 0 1) (1 0 0 2)))))
    ;; in linear time in nested formulas: a hit carried out of the tags took
    ;; a search of each level from each level (2.7 s at depth 80)
    (let* ((deep (lambda (self n)
                   (if (= n 0) "ab"
                       `(concat "x" (frac ,(self self (- n 1)) "ab") "ab"))))
           (doc (stree->tree `(document ,(deep deep 200)))))
      (for-each
        (lambda (mode)
          (with-search-mode mode
            (lambda ()
              (with r (time-of
                        (lambda ()
                          (tree-search-tree
                            doc '(concat (wildcard "x") (select-region "b")
                                         (wildcard "y"))
                            '() 1000)))
                (check= (length (cdr r)) 2)
                (check-true (< (car r) 500))))))
        '(search replace)))
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
    ;; also when a wildcard at an end of the pattern is inside the region:
    ;; it reaches the ends of the argument where the hit is
    (let ((doc '(document (concat "b" (strong "xa") "c"))))
      (check-both doc '(concat (select-region (concat (wildcard "w") "a"))
                               (wildcard "v"))
                  '((0 1 0 0) (0 1 0 2)))
      (check-both doc '(concat (select-region (wildcard "w")) "a"
                               (wildcard "v"))
                  '((0 1 0 0) (0 1 0 1)))
      (check-both doc '(concat (wildcard "w") "a"
                               (select-region (wildcard "v")))
                  '((0 1 0 2) (0 1 0 2))))
    (check-both '(document (concat "x" (rsup "acb") "y"))
                '(concat (wildcard "w")
                         (select-region (concat "c" (wildcard "v"))))
                '((0 1 0 1) (0 1 0 3)))
    ;; and with wildcards at both ends inside regions; the region stays in
    ;; the argument where the hit is (it went from the start of the formula
    ;; in strong to inside the rsup)
    (with doc '(document (concat "b" (strong "xa") "c"))
      (check-both doc '(concat (wildcard "u")
                               (select-region (concat (wildcard "w") "a"
                                                      (wildcard "v")))
                               (wildcard "t"))
                  '((0 1 0 0) (0 1 0 2)))
      (check-both doc '(concat (select-region (wildcard "w")) "a"
                               (select-region (wildcard "v")))
                  '((0 1 0 0) (0 1 0 1) (0 1 0 2) (0 1 0 2))))
    (check-both '(document (concat "b" (strong (concat "x" (rsup "za"))) "c"))
                '(concat (select-region (concat (wildcard "w") "a"))
                         (wildcard "v"))
                '((0 1 0 1 0 0) (0 1 0 1 0 2)))
    ;; a tag of the pattern does not match a tag of the formula inside it
    (check-both '(document (concat "ab" (rsub "y") "cd"))
                '(concat (wildcard "w") (rsub "y")
                         (select-region (wildcard "v")))
                '((0 2 0) (0 2 2)))
    ;; in nested tags, in linear time: the region was looked for again at
    ;; each level (2.2 s at depth 200, and up to 900 s for the first search
    ;; of a session at depth 300)
    (let* ((nest (lambda (self d)
                   (if (= d 0) "xa"
                       `(concat "b" (strong ,(self self (- d 1))) "c"))))
           (doc (stree->tree `(document ,(nest nest 300)))))
      (for-each
        (lambda (what)
          (with r (time-of (lambda () (tree-search-tree doc what '() 100)))
            (check-true (pair? (cdr r)))
            (check-true (< (car r) 500))))
        (list '(concat (wildcard "u")
                       (select-region (concat (wildcard "w") "a"
                                              (wildcard "v")))
                       (wildcard "t"))
              '(concat (select-region (wildcard "w")) "a"
                       (select-region (wildcard "v"))))))
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
    ;; with Replace, a region formula in an argument is also found inside the
    ;; tags of the argument (a cascaded match), as without a region
    (with-search-mode 'replace
      (lambda ()
        (check= (hits '(document (frac (concat "q" (strong "ab")) "1"))
                      '(frac (concat "a" (select-region "b")) "1"))
                '((0 0 1 0 1) (0 0 1 0 2)))))
    ;; the arguments of a tag one by one with Replace, as for other patterns
    (with-search-mode 'replace
      (lambda ()
        (check= (hits '(document (concat (tuple "a" "b" "c") (tuple "a" "b")))
                      '(tuple (select-region "a") "b"))
                '((0 1 0 0) (0 1 0 1)))))))

;; a region which the wildcards of a pattern carry out of a tag is the
;; region which the pattern gives in the argument when it is alone: the
;; regions in b (strong F) c are those in F, moved into the strong, for
;; formulas F made from a fixed pseudo-random sequence. A formula F which
;; is a tag alone is left out here: it is a formula of one child only with
;; the rule of #512 for a tag alone in its paragraph, which this branch does
;; not have (its merge with #512 was checked with them).
(define (region-formulas n)
  (let* ((seed 11)
         (rnd (lambda (k)
                (set! seed (modulo (+ (* seed 1103515245) 12345) 2147483648))
                (modulo (quotient seed 65536) k)))
         (str (lambda ()
                (apply string-append
                       (map (lambda (i) (list-ref '("a" "a" "x" "y") (rnd 4)))
                            (iota (+ 1 (rnd 3)))))))
         (formula
           (lambda (self d)
             (let loop ((k (+ 1 (rnd 4))) (l '()))
               (if (= k 0)
                   (let ((r (reverse l)))
                     (if (null? (cdr r)) (car r) (cons 'concat r)))
                   (let ((x (if (and (> d 0) (< (rnd 100) 45))
                                (case (rnd 3)
                                  ((0) `(rsup ,(self self (- d 1))))
                                  ((1) `(strong ,(self self (- d 1))))
                                  (else `(frac ,(self self (- d 1))
                                               ,(self self (- d 1)))))
                                (str))))
                     (if (and (string? x) (pair? l) (string? (car l)))
                         (loop (- k 1) (cons (string-append (car l) x) (cdr l)))
                         (loop (- k 1) (cons x l)))))))))
    (let loop ((i 0) (r '()))
      (if (= i n) (reverse r)
          (with f (formula formula (+ 1 (rnd 3)))
            (if (or (string? f) (func? f 'concat))
                (loop (+ i 1) (cons f r))
                (loop i r)))))))

(define (moved-into-strong l)
  ;; the paths of a paragraph F as paths of F in b (strong F) c
  (map (lambda (p) (cons 0 (cons 1 (cons 0 (cdr p))))) l))

(define (test-regions-in-arguments)
  (check-group "regions in arguments")
  (let* ((w '(wildcard "w")) (v '(wildcard "v"))
         (u '(wildcard "u")) (t '(wildcard "t"))
         (pats `((concat ,u (select-region (concat ,w "a" ,v)) ,t)
                 (concat (select-region (concat ,w "a")) ,v)
                 (concat ,u (select-region (concat "a" ,v)))
                 (concat (select-region ,w) "a" (select-region ,v))
                 (concat (select-region (concat ,w "a")) "x" ,v)
                 (concat ,u "x" (select-region (concat "a" ,v)))
                 (select-region (concat ,w "a" ,v))
                 (concat ,u (select-region (concat ,w "ax" ,v)) ,t)
                 ;; a wildcard inside the part of the pattern, a region after
                 ;; a tag of the pattern
                 (concat ,u "x" ,w (select-region "a") ,t)
                 (concat ,u "y" ,w (select-region "a") "x" ,t)
                 (concat ,u (strong (select-region "a")) ,w
                         (select-region "x") ,t)))
         (fs (region-formulas 200)))
    ;; the region of the formula where the pattern matches, the outermost
    ;; one (not the deepest), and lazily (xyaaaaax gave all of it)
    (check-both '(document (concat "b" (strong (concat "x" (rsup "ya") "az"))
                                   "c"))
                (list-ref pats 3)
                '((0 1 0 0 0) (0 1 0 1 1) (0 1 0 2 1) (0 1 0 2 2)))
    (check-both '(document (concat "b" (strong (concat (rsup "a") "a")) "c"))
                (list-ref pats 0)
                '((0 1 0 0 0) (0 1 0 1 1)))
    (check-both '(document (concat "b" (strong "xyaaaaax") "c"))
                (list-ref pats 0)
                '((0 1 0 0) (0 1 0 3)))
    ;; in the formula which matches, the leftmost region, which may be in a
    ;; tag before it; a region inside a tag of the pattern counts
    (check-both '(document (concat "b" (strong (concat "x" (rsup "xa") "a"))
                                   "c"))
                (list-ref pats 8)
                '((0 1 0 1 0 1) (0 1 0 1 0 2)))
    (check-both '(document (concat "b" (rsup (concat (strong "a")
                                                     (rsup (concat (strong "a")
                                                                   "x"))
                                                     "x"))
                                   "c"))
                (list-ref pats 10)
                '((0 1 0 0 0 0) (0 1 0 0 0 1) (0 1 0 2 0) (0 1 0 2 1)))
    ;; a tag of the pattern alone in an argument: the wildcards take
    ;; nothing (it was not found, with or without the region)
    (let ((p '(concat (wildcard "u") (strong (select-region "a"))
                      (wildcard "t"))))
      (check-both '(document (concat "z" (rsup (strong "a")) "q")) p
                  '((0 1 0 0 0) (0 1 0 0 1)))
      (check-both '(document (concat "z" (rsup (strong "a")))) p
                  '((0 1 0 0 0) (0 1 0 0 1)))
      (check-both '(document (frac (strong "a") "y")) p
                  '((0 0 0 0) (0 0 0 1)))
      (check-both '(document (rsup (strong "a"))) p
                  '((0 0 0 0) (0 0 0 1)))
      (check-both '(document (concat "z" (rsup (strong "a")) "q"))
                  '(concat (wildcard "u") (strong "a") (wildcard "t"))
                  '((0 0 0) (0 2 1))))
    ;; in linear time: each formula of the argument is matched once (the
    ;; leftmost region searched again at each level: 9.8 s at depth 300)
    (let* ((nest (lambda (self k)
                   (if (= k 0) "xa"
                       `(concat "x" (rsup ,(self self (- k 1))) "a"))))
           (doc (stree->tree
                  `(document (concat "b" (strong ,(nest nest 300)) "c")))))
      (with r (time-of (lambda () (tree-search-tree doc (list-ref pats 8)
                                                    '() 100)))
        (check-true (pair? (cdr r)))
        (check-true (< (car r) 500))))
    (for-each
      (lambda (mode)
        (with-search-mode mode
          (lambda ()
            (for-each
              (lambda (what)
                (check= (list-filter
                          fs
                          (lambda (f)
                            (!= (hits `(document (concat "b" (strong ,f) "c"))
                                      what)
                                (moved-into-strong
                                  (hits `(document ,f) what)))))
                        '()))
              pats))))
      '(search replace))))

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

;; from a position inside a tag which matches a tag pattern: that tag is a
;; hit, as from a position outside it; only its children were searched, which
;; cannot match a pattern with its label (#523). With the cursor inside a
;; match, "next" selects that enclosing match, as in a text (the
;; maintainer's decision)
(define (positions-in t p)
  ;; the cursor positions inside the tree t at p
  (if (string? t)
      (map (lambda (k) (rcons p k)) (iota (+ (string-length t) 1)))
      (append (list (rcons p 0) (rcons p 1))
              (append-map (lambda (i) (positions-in (list-ref t (+ i 1))
                                                    (rcons p i)))
                          (iota (- (length t) 1))))))

(define (stree-at t p)
  (if (null? p) t (stree-at (list-ref t (+ (car p) 1)) (cdr p))))

(define (check-from-inside doc what at expected)
  ;; the hits of what from each position inside the subtree of doc at the
  ;; path at, in both modes, are the hits from the start
  (for-each
    (lambda (mode)
      (with-search-mode mode
        (lambda ()
          (let* ((t (stree->tree doc))
                 (all (tree-search-tree t what '() 1000))
                 (sub (stree-at doc at))
                 (bad (list-filter
                        (positions-in sub at)
                        (lambda (pos)
                          (!= (tree-search-tree-at t what '() pos 1000)
                              all)))))
            (check= all expected)
            (check= bad '())))))
    '(search replace)))

(define (test-position-inside-a-match)
  (check-group "a position inside a match")
  (check-from-inside '(document (frac (sqrt "a") "c"))
                     '(frac (sqrt "a") "c") '(0)
                     '((0 0) (0 1)))
  (check-from-inside '(document (concat "x+" (frac (sqrt "a") "c") "+y"))
                     '(frac (sqrt "a") "c") '(0 1)
                     '((0 1 0) (0 1 1)))
  (check-from-inside '(document (concat "x+" (frac (sqrt "a") "c") "+y"))
                     '(sqrt "a") '(0 1 0)
                     '((0 1 0 0) (0 1 0 1)))
  ;; a wildcard in the pattern
  (check-from-inside
    '(document (frac (sqrt (concat "a" (rsub "y") (rsub "x"))) "c"))
    '(frac (sqrt (concat "a" (wildcard "w") (rsub "x"))) "c") '(0)
    '((0 0) (0 1)))
  ;; a tag in a text
  (check-from-inside '(document (concat "x " (strong "ab") " y"))
                     '(strong "ab") '(0 1)
                     '((0 1 0) (0 1 1)))
  ;; wildcards around a tag, as an argument
  (check-from-inside '(document (concat "z" (frac (strong "ab") "c")))
                     '(frac (concat (wildcard "v") (strong "ab")
                                    (wildcard "w"))
                            "c")
                     '(0 1)
                     '((0 1 0) (0 1 1)))
  ;; a paragraph which is the tag alone
  (check-from-inside '(document "u" (strong "ab") "v")
                     '(strong "ab") '(1)
                     '((1 0) (1 1)))
  (check-from-inside '(document "u" (frac "a" (sqrt "b")) "v")
                     '(frac "a" (sqrt "b")) '(1)
                     '((1 0) (1 1))))

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

;; a formula in the argument of a tag of the pattern is text, as in a
;; paragraph: its parts are contiguous and a wildcard takes any run, maybe
;; empty. It was matched child by child, so that a wildcard took exactly
;; one child, and Search skipped the children between the parts (#501). As
;; for a string argument, Search finds it anywhere in the argument, Replace
;; only as the whole argument
(define (test-formulas-in-arguments)
  (check-group "formulas in arguments")
  (let ((pat '(frac (concat "a" (wildcard "w") (rsub "x")) "c")))
    ;; the wildcard takes nothing, two children, a part of a string
    (check-both '(document (frac (concat "a" (rsub "x")) "c")) pat
                '((0 0) (0 1)))
    (with-search-mode 'replace
      (lambda ()
        (check= (hits '(document (frac (concat "a" (rsub "y") (rsub "z")
                                               (rsub "x"))
                                       "c"))
                      pat)
                '((0 0) (0 1)))))
    (check-both '(document (frac (concat "ab" (rsub "x")) "c")) pat
                '((0 0) (0 1))))
  ;; a leading wildcard may be empty before an argument which is a tag
  (check-both '(document (frac (rsub "x") "c"))
              '(frac (concat (wildcard "w") (rsub "x")) "c")
              '((0 0) (0 1)))
  ;; with Replace, the whole argument: a formula pattern does not match one
  ;; child of the formula of the argument (only inside its tags)
  (with-search-mode 'replace
    (lambda ()
      (check= (hits '(document (concat (frac (concat (rsub "x") "a") "c")
                                       (frac (rsub "x") "c")))
                    '(frac (concat (wildcard "w") (rsub "x")) "c"))
              '((0 1 0) (0 1 1)))))
  ;; without a wildcard, the parts are adjacent, also when searching
  (with-search-mode 'search
    (lambda ()
      (check= (hits '(document (frac (concat "a" (rsub "y") (rsub "x")) "c"))
                    '(frac (concat "a" (rsub "x")) "c"))
              '())
      (check= (hits '(document (frac (concat "ab" (rsub "x")) "c"))
                    '(frac (concat "a" (rsub "x")) "c"))
              '()))))

;; a match of several paragraphs may start in the paragraph where the
;; previous one ended, also inside a tag of it, where a trailing wildcard
;; carries it to the end of the paragraph: it was missed (#501)
(define (test-match-after-a-tag)
  (check-group "a match after a match in a tag")
  (check-both '(document "xb" (concat (strong "bab") "x") "a")
              '(document (concat "b" (wildcard "w"))
                         (concat (wildcard "v") "a"))
              '((0 1) (1 0 0 2) (1 0 0 2) (2 1))))

;; a paragraph which is a tag alone is a formula of one child: a leading
;; (trailing) wildcard carries a hit inside the tag to the start (end) of
;; the paragraph, as when the tag is in a formula; it stopped at the end of
;; the string inside the tag (#501, the maintainer's decision)
(define (test-lone-tags)
  (check-group "a tag alone in its paragraph")
  (check-both '(document (strong "xab") "c")
              '(document (concat "a" (wildcard "x")) "c")
              '((0 0 1) (1 1)))
  (check-both '(document (strong "xab") "c") '(concat "a" (wildcard "x"))
              '((0 0 1) (0 1)))
  (check-both '(document (strong "xab") "c") '(concat (wildcard "x") "a")
              '((0 0) (0 0 2)))
  (check-both '(document "xb" (strong "bab") "a")
              '(document (concat "b" (wildcard "w"))
                         (concat (wildcard "v") "a"))
              '((0 1) (1 0 2) (1 0 2) (2 1)))
  ;; but a wildcard never carries a hit out of a document, as in a theorem
  ;; or an item: the hits stay in their paragraphs (the maintainer's
  ;; decision; a formula with a theorem gave one hit which ran out of the
  ;; theorem); inline tags such as section, strong and frac carry it
  (let ((th '(theorem (document "xab" "yab"))))
    (check-both `(document (concat "q" ,th)) '(concat "a" (wildcard "w"))
                '((0 1 0 0 1) (0 1 0 0 3) (0 1 0 1 1) (0 1 0 1 3)))
    (check-both `(document ,th) '(concat "a" (wildcard "w"))
                '((0 0 0 1) (0 0 0 3) (0 0 1 1) (0 0 1 3)))
    (check-both `(document ,th "c") '(document (concat "a" (wildcard "w")) "c")
                '()))
  (check-both '(document
                 (concat "z" (itemize (document (concat (item) "xab")
                                                (concat (item) "yab")))))
              '(concat "a" (wildcard "w"))
              '((0 1 0 0 1 1) (0 1 0 0 1 3) (0 1 0 1 1 1) (0 1 0 1 1 3)))
  (check-both '(document
                 (concat "z" (equation* (document (concat "xa" (rsub "y"))))))
              '(concat "a" (wildcard "w"))
              '((0 1 0 0 0 1) (0 1 0 0 1 1)))
  (check-both '(document (section "xab") "c") '(concat "a" (wildcard "w"))
              '((0 0 1) (0 1)))
  (check-both '(document (frac (concat "a" (rsub "y")) "a+c"))
              '(concat "a" (wildcard "w"))
              '((0 0 0 0) (0 1))))

;; Search finds at least what Replace finds in tag arguments: an argument
;; which is a tag alone is a formula of one child for both (Search missed
;; it inside the tags of an argument, which the cascade of Replace sees)
(define (search-replace-cases n)
  ;; n formula patterns and arguments, from a fixed pseudo-random sequence
  (let* ((seed 7)
         (rnd (lambda (k)
                (set! seed (modulo (+ (* seed 1103515245) 12345) 2147483648))
                (modulo (quotient seed 65536) k)))
         (pick (lambda (l) (list-ref l (rnd (length l)))))
         (atom (lambda () (pick '("a" "b" "ab" "z"))))
         (tag (lambda () (pick '((rsub "x") (strong "ab") (rsup "2")))))
         (arg (lambda ()
                (case (rnd 4)
                  ((0) (tag))
                  ((1) `(em ,(tag)))
                  ((2) `(concat ,(atom) (sqrt ,(tag))))
                  (else `(concat ,(atom) ,(tag))))))
         (elem (lambda ()
                 (case (rnd 3)
                   ((0) '(wildcard "w"))
                   ((1) (tag))
                   (else (atom))))))
    (map (lambda (i)
           (let* ((l (list (elem) (elem)))
                  (l2 (if (and (string? (car l)) (string? (cadr l)))
                          (list (car l) (tag))
                          l)))
             (list `(document (frac ,(arg) "c"))
                   `(frac (concat ,@l2) "c"))))
         (iota n))))

(define (test-search-contains-replace)
  (check-group "search contains replace")
  (with r (list-filter
            (search-replace-cases 400)
            (lambda (c)
              (let* ((rh (with-search-mode-value 'replace
                           (lambda () (hits (car c) (cadr c)))))
                     (sh (with-search-mode-value 'search
                           (lambda () (hits (car c) (cadr c))))))
                (and (pair? rh) (null? sh)))))
    (check= r '()))
  (check-both '(document (frac (concat "z" (em (rsub "x"))) "c"))
              '(frac (concat (wildcard "w") (rsub "x")) "c")
              '((0 0) (0 1)))
  (check-both '(document (frac (concat "1+" (sqrt (rsup "2"))) "c"))
              '(frac (concat (wildcard "w") (rsup "2")) "c")
              '((0 0) (0 1)))
  (check-both '(document (frac (em (strong "ab")) "c"))
              '(frac (concat (strong "ab") (wildcard "w")) "c")
              '((0 0) (0 1)))
  ;; the arguments where the pattern was not found are remembered, but not
  ;; the hidden ones, which may be shared with a visible argument (a
  ;; document holds one tree several times, and the search does not go
  ;; into the value of a with); and for each formula of the pattern
  (let* ((s (stree->tree '(concat "z" (rsub "x"))))
         (d (tree 'document
                  (tree 'concat
                        (tree 'frac
                              (tree 'concat "q" (tree 'with "color" s "u"))
                              "c")
                        " " (tree 'frac s "c"))))
         (d2 (tree 'document
                   (tree 'concat (tree 'frac s "c") "+" (tree 'frac "q" s)))))
    (for-each
      (lambda (mode)
        (with-search-mode mode
          (lambda ()
            (check= (tree-search-tree
                      d '(frac (concat (wildcard "w") (rsub "x")) "c") '() 100)
                    '((0 2 0) (0 2 1)))
            (check= (tree-search-tree
                      d2 '(frac (concat "q" (wildcard "w"))
                                (concat (wildcard "v") (rsub "x")))
                      '() 100)
                    '((0 2 0) (0 2 1))))))
      '(search replace))))

;; a formula pattern in a tag argument, in nested fractions: the search of
;; each argument looked again inside all the tags of the argument, at each
;; level (16 s for a continued fraction of depth 60, a few ms now)
(define (continued-fraction d)
  (if (= d 0) "q"
      `(frac (concat "1+" (frac "1" ,(continued-fraction (- d 1)))) "c")))

(define (test-speed-in-arguments)
  (check-group "speed in arguments")
  ;; and of depth 200 (2.3 s before the arguments without a hit were
  ;; remembered, 0.55 s on wip_fixes, about 20 ms now)
  (with t (stree->tree `(document ,(continued-fraction 200)))
    (for-each
      (lambda (mode)
        (with-search-mode mode
          (lambda ()
            (check-fast t '(frac (concat "a" (wildcard "w") (rsub "x")) "c")
                        '()))))
      '(search replace)))
  (let ((t (stree->tree `(document ,(continued-fraction 60))))
        (u (stree->tree `(document ,@(map (lambda (i) (continued-fraction 15))
                                          (iota 300))))))
    (for-each
      (lambda (mode)
        (with-search-mode mode
          (lambda ()
            (for-each
              (lambda (what)
                (check-fast t what '())
                (check-fast u what '()))
              '((frac (concat "a" (wildcard "w") (rsub "x")) "c")
                (frac (concat "1+" (rsub "x")) "c"))))))
      '(search replace))))

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
;; With the cursor just after a tag with one argument (the path ends with 1,
;; out of the range of its children), the children of the tag were never
;; searched, and tree-search-tree-at missed the hits inside it (#548)
(define (check-from-anywhere doc what expected)
  ;; the hits of what from every cursor position in doc, in both modes, are
  ;; the hits from the start
  (check-from-inside doc what '() expected))

(define (test-position-after-a-one-argument-tag)
  (check-group "a position after a one-argument tag")
  (check-from-anywhere
    '(document (theorem (document "a" (frac "x" "y") "b")))
    '(frac "x" "y") '((0 0 1 0) (0 0 1 1)))
  (check-from-anywhere
    '(document (theorem (document "a" (frac "x" "y") "b")) "c")
    '(frac "x" "y") '((0 0 1 0) (0 0 1 1)))
  (check-from-anywhere
    '(document "a" (theorem (document "b d")) "c")
    "d" '((1 0 0 2) (1 0 0 3)))
  (check-from-anywhere
    '(document (tabular (table (row (cell "a") (cell (frac "x" "y")))
                               (row (cell "b") (cell "c")))))
    '(frac "x" "y") '((0 0 0 1 0 0) (0 0 0 1 0 1)))
  (check-from-anywhere
    '(document (tabular (table (row (cell "a") (cell "b d")))))
    "d" '((0 0 0 1 0 2) (0 0 0 1 0 3)))
  (check-from-anywhere
    '(document (concat "a " (strong "x b") " c"))
    "b" '((0 1 0 2) (0 1 0 3)))
  (check-from-anywhere
    '(document (concat "a " (strong (concat "x" (frac "x" "y"))) " c"))
    '(frac "x" "y") '((0 1 0 1 0) (0 1 0 1 1))))

;; Replace: the values of the wildcards are the runs which they take in the
;; match, as the search aligns the pattern with the text of the match
;; (lazily, a part of a string, several children, nothing); a run of several
;; children is spliced into the formula around the wildcard, and an empty
;; one is "" (the maintainer's decisions). They were matched child by child:
;; with another number of children the replacement kept (wildcard "w"), and
;; a*_x on ab_y_x gave _y (#520)
(define adjust-by (widget 'adjust-by))

(define (replaced by what match)
  (tm->stree (adjust-by by what match)))

(define (check-replaced by what match expected)
  ;; match is a hit of what in Replace mode, and its replacement by by
  (with-search-mode 'replace
    (lambda ()
      (check= (hits `(document ,match) what) '((0 0) (0 1)))
      (check= (replaced by what match) expected))))

(define (formula-ends match)
  ;; the start and the end of the formula or string match, in a document
  (if (string? match) (list '(0 0) (list 0 (string-length match)))
      (let* ((n (- (length match) 2))
             (last (list-ref match (+ n 1))))
        (list '(0 0 0)
              (list 0 n (if (string? last) (string-length last) 1))))))

(define (check-replaced-text by what match expected)
  ;; idem for a formula pattern, which matches all the formula match
  (with-search-mode 'replace
    (lambda ()
      (check= (hits `(document ,match) what) (formula-ends match))
      (check= (replaced by what match) expected))))

(define (distinct-wildcards what)
  ;; what (a fraction of a formula), the second wildcard named v: two
  ;; wildcards with the same name take the value of the first
  (let* ((l (cdadr what))
         (k (list-find-index l (cut == <> '(wildcard "w"))))
         (l2 (if k (append (list-head l (+ k 1))
                           (map (lambda (x)
                                  (if (== x '(wildcard "w")) '(wildcard "v") x))
                                (list-tail l (+ k 1))))
                 l)))
    `(frac (concat ,@l2) ,(caddr what))))

(define (test-replace-wildcards)
  (check-group "replace wildcards")
  (let ((what '(frac (concat "a" (wildcard "w") (rsub "x")) "c"))
        (by '(frac (wildcard "w") "c")))
    (for-each
      (lambda (row)
        (check-replaced by what `(frac ,(car row) "c") `(frac ,(cadr row) "c")))
      '(((concat "a" (rsub "y") (rsub "z") (rsub "x"))
         (concat (rsub "y") (rsub "z")))
        ((concat "a" (rsub "x")) "")
        ((concat "ab" (rsub "x")) "b")
        ((concat "a" (rsub "y") (rsub "x")) (rsub "y"))
        ((concat "ab" (rsub "y") (rsub "x")) (concat "b" (rsub "y"))))))
  (let ((what '(concat "a" (wildcard "w") (rsub "x"))))
    (for-each
      (lambda (row)
        (check-replaced-text '(wildcard "w") what (car row) (cadr row))
        ;; spliced into the formula of the replacement, and simplified
        (check-replaced-text '(concat "q" (wildcard "w") "r") what
                             (car row) (caddr row)))
      '(((concat "a" (rsub "y") (rsub "z") (rsub "x"))
         (concat (rsub "y") (rsub "z"))
         (concat "q" (rsub "y") (rsub "z") "r"))
        ((concat "abc" (rsub "x")) "bc" "qbcr")
        ((concat "ab" (rsub "y") (rsub "x"))
         (concat "b" (rsub "y"))
         (concat "qb" (rsub "y") "r")))))
  ;; the case of the letters does not matter
  (with-preference "case-insensitive-match" "on"
    (lambda ()
      (check-replaced-text '(concat "<" (wildcard "w") ">")
                           '(concat "A" (wildcard "w") "C") "abc" "<b>")
      (check-replaced-text '(wildcard "w")
                           '(concat "A" (wildcard "w") (rsub "X"))
                           '(concat "aBc" (rsub "x")) "Bc")))
  ;; with allow-partial-replace, a formula anywhere in the argument: the
  ;; leftmost one, as the search finds it (a_x ends at the first _x; the
  ;; rest of the argument goes with the fraction, which Replace replaces as
  ;; a whole). A trailing wildcard takes the rest of the argument, as the
  ;; search carries it to the end of the formula, and in a string, without
  ;; initial matches, the match ends at its end (match_atomic); the
  ;; wildcard took nothing
  (with-preference "allow-partial-replace" "on"
    (lambda ()
      (for-each
        (lambda (row)
          (check-replaced '(frac (wildcard "w") "c") (car row)
                          `(frac ,(cadr row) "c") `(frac ,(caddr row) "c")))
        '(((frac (concat "a" (wildcard "w") (rsub "x")) "c")
           (concat "zab" (rsub "x") "qa" (rsub "x")) "b")
          ((frac (concat "a" (wildcard "w")) "c") "zab" "b")
          ((frac (concat "a" (wildcard "w")) "c")
           (concat "za" (rsub "1") "b") (concat (rsub "1") "b"))
          ((frac (concat "a" (wildcard "w") "x") "c") "zaxbax" "xba")))))
  ;; a hit which a wildcard carries out of a tag, or into one: Replace
  ;; replaces the match in the argument which holds its literal part, as
  ;; when the argument is alone (the maintainer's decision); the run was
  ;; the whole paragraph
  (with-search-mode 'replace
    (lambda ()
      (for-each
        (lambda (row)
          (let* ((doc (stree->tree (car row)))
                 (what (cadr row))
                 (h (tree-search-tree doc what '() 100)))
            (check= h (caddr row))
            (with r (tree-search-replace-match doc what (car h) (cadr h))
              (check= (map (lambda (x) (map string->number (cdr (tm->stree x))))
                           (list (car r) (cadr r)))
                      (cadddr row))
              (check= (map tm->stree (cdddr r)) (car (cddddr row))))))
        '(((document (concat (strong "xab") "c")) (concat "a" (wildcard "w"))
           ((0 0 0 1) (0 1 1)) ((0 0 0 1) (0 0 0 3)) ("a" "b"))
          ((document (concat "c" (frac "n" "bax"))) (concat (wildcard "w") "a")
           ((0 0 0) (0 1 1 2)) ((0 1 1 0) (0 1 1 2)) ("b" "a"))
          ;; a hit inside a tag is replaced as it is
          ((document (concat "q" (strong "xacb")))
           (concat "a" (wildcard "w") "b")
           ((0 1 0 1) (0 1 0 4)) ((0 1 0 1) (0 1 0 4)) ("a" "c" "b"))))))
  ;; replacing the hits of a pattern by the pattern itself gives them back
  ;; (the direct hits: a cascaded match is inside a tag of the argument,
  ;; which the replacement does not keep)
  (with-search-mode 'replace
    (lambda ()
      (with-preference "allow-cascaded-match" "off"
        (lambda ()
          (with bad
              (list-filter
                (search-replace-cases 400)
                (lambda (c)
                  (let* ((doc (car c)) (what (distinct-wildcards (cadr c))))
                    (and (== (hits doc what) '((0 0) (0 1)))
                         (!= (replaced what what (cadr doc))
                             (tm->stree (tree-simplify
                                          (stree->tree (cadr doc)))))))))
            (check= bad '())))))))

;; Edit > Replace in a buffer: the pattern and the replacement in the
;; auxiliary buffers of the widget, driven by its Replace one and Replace All
(define (with-search-buffers doc what by thunk)
  ;; the value of thunk in a buffer holding doc, with the auxiliary buffers
  ;; of the widget holding the pattern what and the replacement by, and the
  ;; hits of what selected; the buffers are closed afterwards
  (let* ((old (current-buffer))
         (before (buffer-list))
         (u (new-buffer)))
    (switch-to-buffer* u)
    (buffer-set-body u (stree->tree doc))
    (buffer-set (search-buffer) (stree->tree `(document (body (document ,what)))))
    (buffer-set (replace-buffer) (stree->tree `(document (body (document ,by)))))
    (buffer-set-master (search-buffer) u)
    (buffer-set-master (replace-buffer) u)
    (go-start)
    (set-alt-selection "search-reference" (list (cursor-path) (cursor-path)))
    (with r (check-run (lambda () (thunk u)))
      (for (b (buffer-list))
        (when (nin? b before) (buffer-close b)))
      (when (buffer-exists? old) (switch-to-buffer old))
      r)))

(define test-search-filter '(attr))
(define test-source-mode? #f)

(define (with-test-filter filter thunk)
  ;; thunk with the filter of the search set in the buffers of the tests
  (with old test-search-filter
    (dynamic-wind
      (lambda () (set! test-search-filter filter))
      thunk
      (lambda () (set! test-search-filter old)))))

(define (with-test-source-mode thunk)
  ;; thunk with the buffers of the tests in source mode
  (dynamic-wind
    (lambda () (set! test-source-mode? #t))
    thunk
    (lambda () (set! test-source-mode? #f))))

(define (replace-all-in-buffer doc what by)
  ;; the real Replace All of the widget (perform-search*, replace-all) in a
  ;; buffer holding doc, then undo: the result and the body after undo
  (with-search-buffers doc what by
    (lambda (u)
      (let* ((table (widget 'search-filter-table)))
        ;; the hits are not filtered by the mode at the cursor (a buffer
        ;; without a window is not typeset), unless a test sets a filter
        (ahash-set! table (current-buffer) (stree->tree test-search-filter))
        (when test-source-mode? (init-env "mode" "src"))
        (clear-undo-history)
        ((widget 'perform-search*))
        (replace-all)
        (with res (tm->stree (buffer-get-body u))
          (undo 0)
          (list res (tm->stree (buffer-get-body u))))))))

;; Edit > Replace and Replace All on documents (#520): the rule. Replace
;; replaces the match in the argument of the tag which holds the literal
;; part of the pattern, as when the argument is alone (the maintainer's
;; decision: the regions found on b (strong F) c are those on F alone), and
;; keeps the text around it: a lone tag, a hit which a wildcard carries out
;; of a tag or into it, a cascaded match. A cascaded match of a tag pattern
;; is replaced as a whole, its values in copies of the tags around the
;; argument. Replace leaves a hit, with a message, when the replacement would
;; lose text: the literal part crosses the border of a tag, those tags have
;; other content, or the match takes only a part of the argument. It goes
;; on in the text after its replacement, never in it

(define (replace-one-repeatedly doc what by)
  ;; the real Replace one of the widget, again and again until nothing
  ;; changes (at most 100 times: else a runaway)
  (with-search-buffers doc what by
    (lambda (u)
      (let* ((state (lambda () (list (tm->stree (buffer-get-body u))
                                     (get-alt-selection "search-reference")))))
        (ahash-set! (widget 'search-filter-table)
                    (current-buffer) (stree->tree test-search-filter))
        (when test-source-mode? (init-env "mode" "src"))
        ((widget 'perform-search*))
        (let loop ((i 0) (s (state)))
          (replace-one)
          (with s2 (state)
            (cond ((== s2 s) (car s))
                  ((>= i 100) (list 'runaway (car s2)))
                  (else (loop (+ i 1) s2)))))))))

(define (check-replace-rows rows)
  ;; each row: a document, the pattern, the replacement, the result; by
  ;; the real Replace one again and again, and by the real Replace All,
  ;; whose undo gives the document back in one step: they agree
  (for-each
    (lambda (row)
      (let* ((doc (car row)) (what (cadr row)) (by (caddr row))
             (res (cadddr row)))
        (check= (replace-one-repeatedly doc what by) res)
        (check= (replace-all-in-buffer doc what by)
                (list res (tm->stree (tree-simplify (stree->tree doc)))))))
    rows))

(define (test-replace-in-buffers)
  (check-group "replace in buffers")
  ;; Enter in the fields of the widget (the next hit, Replace One) calls
  ;; the inside? of the kernel in this module: a helper of the module hid
  ;; it, and Enter failed (wrong number of arguments)
  (check= (widget '(inside? 'inactive))
          #f)
  (with-search-mode 'replace
    (lambda ()
      (check-replace-rows
        '(((document (concat "a" (rsub "y") (rsub "z") (rsub "x") " end"))
           (concat "a" (wildcard "w") (rsub "x"))
           (concat "<" (wildcard "w") ">")
           (document (concat "<" (rsub "y") (rsub "z") "> end")))
          ((document (concat "s " (frac (concat "ab" (rsub "y") (rsub "x")) "c")
                             " t"))
           (frac (concat "a" (wildcard "w") (rsub "x")) "c")
           (frac (wildcard "w") "d")
           (document (concat "s " (frac (concat "b" (rsub "y")) "d") " t")))
          ((document (concat "a" (rsub "1") "x a" (rsub "2") (rsub "3") "x"))
           (concat "a" (wildcard "w") "x")
           (concat "[" (wildcard "w") "]")
           (document (concat "[" (rsub "1") "] [" (rsub "2") (rsub "3") "]")))
          ((document "a1x a22x a333x")
           (concat "a" (wildcard "w") "x")
           (concat "[" (wildcard "w") "]")
           (document "[1] [22] [333]"))
          ;; hits which touch, and a replacement which ends with the
          ;; start of the next match
          ((document "ababab") "ab" "X" (document "XXX"))
          ((document "aaaaaa") "aa" "a" (document "aaa"))
          ;; a plain string: only its occurrences are replaced (the text
          ;; from the cursor to the end of the hit was)
          ((document "aabb") "ab" "a" (document "aab"))
          ((document "aabb xy") "ab" "" (document "ab xy"))
          ((document "thethee end") "the" "th" (document "ththe end"))
          ((document "aabb ab xy") "ab" "a" (document "aab a xy"))
          ;; matches removed one after the other (a crash), and replaced
          ;; by text which ends with the pattern
          ((document (frac "xbzbb" "c")) (concat (wildcard "w") "b") ""
           (document (frac "" "c")))
          ((document (frac "xbzb" "c")) (concat (wildcard "w") "b") "bb"
           (document (frac "bbbb" "c")))
          ((document (concat "q" (strong "xbbb")))
           (concat (wildcard "w") "b") ""
           (document (concat "q" (strong ""))))
          ;; nested documents (a footnote): only the matches are replaced,
          ;; and Replace All ends (it used paths which an edit made stale)
          ((document (concat "ab " (footnote (document "ab")) (strong "q")
                             (em "zzzz") (strong "zzab")))
           "ab" (frac "1" "2")
           (document (concat (frac "1" "2") " " (footnote (document (frac "1" "2")))
                             (strong "q") (em "zzzz")
                             (strong (concat "zz" (frac "1" "2"))))))
          ((document (concat "ab" (footnote (document "ab")) (strong "q")
                             (strong "zzab") (em "qqqqqq")))
           "ab" ""
           (document (concat (footnote (document "")) (strong "q")
                             (strong "zz") (em "qqqqqq"))))
          ;; the next match after a replacement in a paragraph of a
          ;; document inside a tag: the search after it saw the tag of the
          ;; reference, whose text before it was cut, as the start of the
          ;; hit, which took the earlier replacements (and was left)
          ((document (concat (strong (concat (tabular (table (row (cell "b"))))
                                             "x" (theorem (document "b" "baa"))))
                             "ab"))
           (concat (wildcard "w") "b") (concat "<" (wildcard "w") ">")
           (document (concat (strong (concat (tabular (table (row (cell "<>"))))
                                             "x" (theorem (document "<>" "<>aa"))))
                             "<a>")))
          ((document (concat (strong (concat (tabular (table (row (cell
                                (document "aabaxaaa" "bbxxxxab")))))
                                             "abx"
                                             (theorem (document "abbb" "baa"))))
                             "abbbb"))
           (concat (wildcard "w") "b") (concat "<" (wildcard "w") ">")
           (document (concat (strong (concat (tabular (table (row (cell
                                (document "<aa>axaaa" "<><><xxxxa>")))))
                                             "<a>x"
                                             (theorem (document "<a><><>"
                                                                "<>aa"))))
                             "<a><><><>")))
          ;; the reference in a script argument with text after it: the
          ;; script, which holds the reference, is not a part of the next
          ;; match (the hit after it was left)
          ((document (concat (rsub "xbbx") "b"))
           (concat (wildcard "w") "b") (frac (wildcard "w") "1")
           (document (concat (rsub (concat (frac "x" "1") (frac "" "1") "x"))
                             (frac "" "1"))))
          ((document (concat (rsub "bx") "b"))
           (concat (wildcard "w") "b") (concat "<" (wildcard "w") ">")
           (document (concat (rsub "<>x") "<>")))
          ;; next to scripts: exactly the range is removed (the selection
          ;; of the editor moved its ends over the base of a script: the
          ;; base was lost, also a whole tag)
          ((document (concat "x" (rsub "1") "a"))
           (concat (rsub (wildcard "w")) "a") "Z" (document "xZ"))
          ((document (concat "x" (rsub "1") "a"))
           (concat (rsub (wildcard "w")) "a") (concat "<" (wildcard "w") ">")
           (document "x<1>"))
          ((document (concat (frac "p" "q") (rsub "1") "a"))
           (concat (rsub (wildcard "w")) "a") "Z"
           (document (concat (frac "p" "q") "Z")))
          ((document (concat (strong "p") (rsup "2") "a"))
           (concat (rsup (wildcard "w")) "a") "Z"
           (document (concat (strong "p") "Z")))
          ((document (concat "a" (lsub "1") "x"))
           (concat "a" (lsub (wildcard "w"))) "Z" (document "Zx"))
          ((document (concat "a" (lsup "1") (frac "p" "q")))
           (concat "a" (lsup (wildcard "w"))) "Z"
           (document (concat "Z" (frac "p" "q"))))
          ((document (concat "x" (rsub "1") (rsup "2") "a"))
           (concat "x" (rsub (wildcard "w"))) "Z"
           (document (concat "Z" (rsup "2") "a")))
          ((document (concat "f" (rprime "'") "a"))
           (concat (rprime (wildcard "w")) "a") "Z" (document "fZ"))
          ;; a leading wildcard does not take the text before the reference
          ((document (concat "p" (strong "xb") "zb"))
           (concat (wildcard "w") "b") (concat "<" (wildcard "w") ">")
           (document (concat "p" (strong "<x>") "<z>")))
          ((document "a--b--c") "--" "-" (document "a-b-c"))
          ((document "a1xa2xa3x")
           (concat "a" (wildcard "w") "x") (concat "[" (wildcard "w") "]")
           (document "[1][2][3]"))
          ((document "a1x a2x")
           (concat "a" (wildcard "w") "x") (concat "a" (wildcard "w") "xa")
           (document "a1xa a2xa"))
          ;; a replacement which ends with the first letter of the pattern
          ;; next to the next hit: the leftmost match after it, not one
          ;; which takes both
          ((document "a1xa2xa3x") (concat "a" (wildcard "w") "x") "a"
           (document "aaa"))
          ((document "a1xa2xa3x") (concat "a" (wildcard "w") "x") "aa"
           (document "aaaaaa"))
          ((document "a1xa2xa3x") (concat "a" (wildcard "w") "x") "xa"
           (document "xaxaxa"))
          ;; a leading wildcard, whose hit enters the tag: the next
          ;; matches in the argument, which the search does not give as
          ;; hits, are replaced too, as by Replace one again and again
          ((document (frac "xabzb" "c"))
           (concat (wildcard "w") "b") (concat "<" (wildcard "w") ">")
           (document (frac "<xa><z>" "c")))
          ((document (concat "q" (strong "xabzb")))
           (concat (wildcard "w") "b") (concat "<" (wildcard "w") ">")
           (document (concat "q" (strong "<xa><z>"))))
          ((document (tabular (table (row (cell "xbyb") (cell "zb")))))
           (concat (wildcard "w") "b") (concat "<" (wildcard "w") ">")
           (document (tabular (table (row (cell "<x><y>") (cell "<z>"))))))
          ((document (frac "a1xa2x" "c"))
           (concat (wildcard "w") "a" (wildcard "v") "x")
           (concat "[" (wildcard "w") "|" (wildcard "v") "]")
           (document (frac "[|1][|2]" "c")))
          ((document (frac "a2xa3x" "c"))
           (concat "a" (wildcard "w") "x") (concat "[" (wildcard "w") "]")
           (document (frac "[2][3]" "c")))
          ;; only the parts of the replacement which come from a wildcard
          ;; are spliced and merged: the rest is as the user wrote it
          ((document "a1x")
           (concat "a" (wildcard "w") "x")
           (concat (quote "q") (wildcard "w"))
           (document (concat (quote "q") "1")))
          ;; a hit of several paragraphs is one selection, replaced in place
          ((document "xab" "cdy") (document "b" "c") "Q" (document "xaQdy"))
          ((document (theorem (document "xab" "m" "cdy")))
           (document "b" "m" "c") "Q"
           (document (theorem (document "xaQdy"))))
          ((document "xab" "cdy")
           (document (concat "a" (wildcard "w")) (concat (wildcard "v") "d"))
           (concat "[" (wildcard "w") "|" (wildcard "v") "]")
           (document "x[b|c]y"))
          ;; a hit carried out of a tag or into it: inside the argument
          ((document (strong "xab") "c")
           (concat "a" (wildcard "w")) (concat "<" (wildcard "w") ">")
           (document (strong "x<b>") "c"))
          ((document (concat (strong "xab") "c"))
           (concat "a" (wildcard "w")) (concat "<" (wildcard "w") ">")
           (document (concat (strong "x<b>") "c")))
          ((document (concat (strong "xab") "c" (strong "ad")))
           (concat "a" (wildcard "w")) (concat "<" (wildcard "w") ">")
           (document (concat (strong "x<b>") "c" (strong "<d>"))))
          ((document (concat "c" (strong "bax")))
           (concat (wildcard "w") "a") (concat "<" (wildcard "w") ">")
           (document (concat "c" (strong "<b>x"))))
          ((document (concat "q" (frac (strong "xab") "d") "c"))
           (concat "a" (wildcard "w")) (concat "<" (wildcard "w") ">")
           (document (concat "q" (frac (strong "x<b>") "d") "c")))
          ((document (concat "q" (with "color" "red" "xab") "c"))
           (concat "a" (wildcard "w")) (concat "<" (wildcard "w") ">")
           (document (concat "q" (with "color" "red" "x<b>") "c")))
          ((document (concat (tabular (table (row (cell "xab") (cell "d"))))
                             "c"))
           (concat "a" (wildcard "w")) (concat "<" (wildcard "w") ">")
           (document (concat (tabular (table (row (cell "x<b>") (cell "d"))))
                             "c")))
          ((document (concat (strong (concat (label "l1") "ab")) "c"))
           (concat "a" (wildcard "w")) (concat "<" (wildcard "w") ">")
           (document (concat (strong (concat (label "l1") "<b>")) "c")))
          ;; next to scripts (a selection was extended to their base)
          ((document (concat (strong "xab") (rsub "q") "c"))
           (concat "a" (wildcard "w")) (wildcard "w")
           (document (concat (strong "xb") (rsub "q") "c")))
          ((document (math (concat (sqrt "x+ab") (rsup "2") "+c")))
           (concat "a" (wildcard "w")) (concat "[a" (wildcard "w") "]")
           (document (math (concat (sqrt "x+[ab]") (rsup "2") "+c"))))
          ;; two-sided patterns: lone tags, text around the tag, empty runs,
          ;; a replacement which drops or repeats a wildcard (the other
          ;; arguments are not touched), hidden arguments, tables
          ((document (frac "xaby" "cdy"))
           (concat (wildcard "w") "b" (wildcard "v"))
           (concat "<" (wildcard "w") "|" (wildcard "v") ">")
           (document (frac "<xa|y>" "cdy")))
          ((document (concat "p" (frac "xaby" "c") "q"))
           (concat (wildcard "w") "b" (wildcard "v"))
           (concat "<" (wildcard "w") "|" (wildcard "v") ">")
           (document (concat "p" (frac "<xa|y>" "c") "q")))
          ((document (math (concat "x+" (frac "aby" "c"))))
           (concat (wildcard "w") "b" (wildcard "v"))
           (concat "<" (wildcard "w") "|" (wildcard "v") ">")
           (document (math (concat "x+" (frac "<a|y>" "c")))))
          ((document (frac "b" "c"))
           (concat (wildcard "w") "b" (wildcard "v"))
           (concat "<" (wildcard "w") "|" (wildcard "v") ">")
           (document (frac "<|>" "c")))
          ((document (frac "xaby" "cdy"))
           (concat (wildcard "w") "b" (wildcard "v"))
           (concat "<" (wildcard "v") ">")
           (document (frac "<y>" "cdy")))
          ((document (frac "xaby" "cdy"))
           (concat (wildcard "w") "b" (wildcard "v"))
           (concat (wildcard "w") (wildcard "w"))
           (document (frac "xaxa" "cdy")))
          ((document (concat "p" (folded "xaby" "secret") "q"))
           (concat (wildcard "w") "b" (wildcard "v"))
           (concat "<" (wildcard "w") "|" (wildcard "v") ">")
           (document (concat "p" (folded "<xa|y>" "secret") "q")))
          ((document (tabular (table (row (cell "b") (cell "d")))))
           (concat (wildcard "w") "b" (wildcard "v"))
           (concat "<" (wildcard "w") "|" (wildcard "v") ">")
           (document (tabular (table (row (cell "<|>") (cell "d"))))))
          ((document (concat "p" (strong (em "xaby")) "c"))
           (concat (wildcard "w") "a" (wildcard "v"))
           (concat "<" (wildcard "w") "|" (wildcard "v") ">")
           (document (concat "p" (strong (em "<x|by>")) "c")))
          ;; Replace All goes on after each replacement, never inside it
          ((document (strong "xbybz"))
           (concat (wildcard "w") "b" (wildcard "v"))
           (concat "<" (wildcard "w") "|" (wildcard "v") ">")
           (document (strong "<x|ybz>")))
          ;; and on in the rest of a hit in which it replaced (a table,
          ;; a formula with two fractions)
          ((document (tabular (table (row (cell "xby") (cell "d"))
                                     (row (cell "zbt") (cell "e")))))
           (concat (wildcard "w") "b" (wildcard "v"))
           (concat "<" (wildcard "w") "|" (wildcard "v") ">")
           (document (tabular (table (row (cell "<x|y>") (cell "d"))
                                     (row (cell "<z|t>") (cell "e"))))))
          ((document (concat (frac "xb" "c") (frac "yb" "d")))
           (concat (wildcard "w") "b" (wildcard "v"))
           (concat "<" (wildcard "w") "|" (wildcard "v") ">")
           (document (concat (frac "<x|>" "c") (frac "<y|>" "d"))))
          ((document (frac "xbyb" "c") (frac "zbt" "d"))
           (concat (wildcard "w") "b" (wildcard "v"))
           (concat "<" (wildcard "w") "|" (wildcard "v") ">")
           (document (frac "<x|yb>" "c") (frac "<z|t>" "d")))
          ;; cascaded matches of a tag pattern: the tags around the value
          ;; are kept; with other content, the hit is left
          ((document (frac (strong "ab") "c"))
           (frac (concat "a" (wildcard "w")) "c") (frac (wildcard "w") "d")
           (document (frac (strong "b") "d")))
          ((document (frac (strong "abz") "c"))
           (frac (concat "a" (wildcard "w") "z") "c") (frac (wildcard "w") "d")
           (document (frac (strong "b") "d")))
          ((document (frac (frac "ab" "z") "c") (frac "ab" "c"))
           (frac (concat "a" (wildcard "w")) "c") (frac (wildcard "w") "d")
           (document (frac (frac "ab" "z") "c") (frac "b" "d")))
          ((document (frac (folded "ab" "hid") "c"))
           (frac (concat "a" (wildcard "w")) "c") (frac (wildcard "w") "d")
           (document (frac (folded "ab" "hid") "c")))))
      ;; partial and initial matches
      (with-preference "allow-partial-replace" "on"
        (lambda ()
          (check-replace-rows
            '(((document (frac (strong "zab") "c"))
               (frac (concat "a" (wildcard "w")) "c") (frac (wildcard "w") "d")
               (document (frac (strong "zab") "c")))
              ;; the match in an argument as the search highlights it, not
              ;; up to its end
              ((document (frac "xabzb" "c"))
               (concat (wildcard "w") "b") (concat "<" (wildcard "w") ">")
               (document (frac "<xa><z>" "c")))
              ((document (frac "zxbyz" "c"))
               (concat "x" (wildcard "w") "y") (concat "<" (wildcard "w") ">")
               (document (frac "z<b>z" "c")))))))
      (with-preference "allow-initial-replace" "on"
        (lambda ()
          (check-replace-rows
            '(((document (frac (strong "abz") "c"))
               (frac (concat "a" (wildcard "w")) "c") (frac (wildcard "w") "d")
               (document (frac (strong "bz") "d")))))))))
  ;; a hit left as it is says why; Replace one replaces the hit which it
  ;; then highlights
  (with-search-mode 'replace
    (lambda ()
      (let* ((start (lambda ()
                      (ahash-set! (widget 'search-filter-table)
                                  (current-buffer) (stree->tree '(attr)))
                      ((widget 'perform-search*))))
             (rel (lambda (p)
                    (with q (tree->path (buffer-tree))
                      (if (list-starts? p q) (list-tail p (length q)) p))))
             (skip (lambda (doc what by)
                     (with-search-buffers doc what by
                       (lambda (u)
                         (start)
                         (replace-one)
                         (list (tm->stree (buffer-get-body u))
                               (widget 'replace-skip-message)))))))
        (check= (skip '(document (frac (frac "ab" "z") "c"))
                      '(frac (concat "a" (wildcard "w")) "c")
                      '(frac (wildcard "w") "d"))
                '((document (frac (frac "ab" "z") "c"))
                  "Match left: it would lose the rest of a tag"))
        (check= (adjust-by '(concat "<" (wildcard "w") ">")
                           '(concat "a" (wildcard "w") "b") "xyz")
                #f)
        (check= (adjust-by "Q" '(concat "a" (wildcard "w") "b") "xyz") "Q")
        ;; Replace All from a reference inside a hit replaces that hit
        (check= (with-search-buffers
                  '(document "a1x a22x a3x")
                  '(concat "a" (wildcard "w") "x")
                  '(concat "[" (wildcard "w") "]")
                  (lambda (u)
                    (with p (tree->path (buffer-tree))
                      (set-alt-selection "search-reference"
                                         (list (append p '(0 6))
                                               (append p '(0 6))))
                      (start)
                      (replace-all)
                      (tm->stree (buffer-get-body u)))))
                '(document "a1x [22] [3]"))
        (check= (with-search-buffers
                  '(document "a1x a2x a3x")
                  '(concat "a" (wildcard "w") "x")
                  '(concat "[" (wildcard "w") "]")
                  (lambda (u)
                    (start)
                    (replace-one)
                    (let* ((b1 (tm->stree (buffer-get-body u)))
                           (s1 (if (selection-active-any?)
                                   (list (rel (selection-get-start))
                                         (rel (selection-get-end)))
                                   'none)))
                      (replace-one)
                      (list b1 s1 (tm->stree (buffer-get-body u))))))
                '((document "[1] a2x a3x") ((0 4) (0 7))
                  (document "[1] [2] a3x")))))))

;; Random documents, patterns and replacements (seeded, the same each time):
;; strings over a small alphabet, strong, em, math, with, sqrt, frac, rsub,
;; theorems and footnotes (nested documents), table cells (with documents),
;; several paragraphs; string, wildcard, tag, region and multi-paragraph
;; patterns; case-insensitive, partial and initial modes. Replace All and
;; Replace one again and again agree, undo gives the document back, no hit
;; means no change, nothing runs away or takes too long, and for a string
;; pattern the result is that of replacing the occurrences in each string,
;; from left to right (#520)
(define random-state 0)

(define (set-random-seed! s) (set! random-state s))

(define (random-next n)
  ;; a deterministic random number in 0..n-1 (a linear congruential one)
  (set! random-state (modulo (+ (* random-state 1103515245) 12345) 2147483648))
  (modulo (quotient random-state 65536) n))

(define (random-pick l) (list-ref l (random-next (length l))))

(define random-upper? #f)

(define (random-string)
  (list->string
    (map (lambda (i) (random-pick (if random-upper? '(#\a #\b #\x #\A #\B)
                                      '(#\a #\b #\x #\a #\b))))
         (iota (random-next 5)))))

(define (random-item d)
  (if (or (<= d 0) (< (random-next 10) 5)) (random-string)
      (with x (lambda () (random-formula (- d 1)))
        (case (random-next 16)
          ((0) `(strong ,(x)))
          ((1) `(math ,(x)))
          ((2) `(with "color" "red" ,(x)))
          ((3) `(sqrt ,(x)))
          ((4) `(frac ,(x) ,(x)))
          ((5) `(rsub ,(x)))
          ((6) `(theorem (document ,(x) ,(x))))
          ((7) `(tabular (table (row (cell ,(x)) (cell ,(x))))))
          ((8) `(footnote ,(x)))
          ((9) `(tabular (table (row (cell (document ,(x) ,(x)))))))
          ;; scripts, next to the strings and tags of the formula
          ((11) `(rsub ,(random-string)))
          ((12) `(rsup ,(random-string)))
          ((13) `(lsub ,(random-string)))
          ((14) `(lsup ,(random-string)))
          ((15) `(rprime "'"))
          (else `(em ,(x)))))))

(define (random-formula d)
  (with n (+ 1 (random-next 3))
    (if (= n 1) (random-item d)
        `(concat ,@(map (lambda (i) (random-item d)) (iota n))))))

(define (random-case)
  (set! random-upper? (< (random-next 10) 2))
  (let* ((w '(wildcard "w")) (v '(wildcard "v"))
         (doc (simplified `(document ,@(map (lambda (i) (random-formula 2))
                                            (iota (+ 1 (random-next 3)))))))
         (what (case (random-next 15)
                 ((0 1 2) (random-pick '("a" "ab" "ba" "aa" "b" "abx")))
                 ((3) `(concat "a" ,w))
                 ((4) `(concat ,w "b"))
                 ((5) `(concat "a" ,w "b"))
                 ((6) `(concat ,w "a" ,v))
                 ((7) `(frac ,w ,v))
                 ((8) `(concat "a" (select-region "")))
                 ((9) `(concat "a" (select-region ,w) "b"))
                 ((10) `(document (concat ,w "a") (concat "b" ,v)))
                 ((12) `(concat (rsub ,w) "a"))
                 ((13) `(concat "a" (lsub ,w)))
                 ((14) `(concat "b" (rsub ,w)))
                 (else `(concat (strong ,w) "a"))))
         (by (if (and (pair? what) (in? (car what) '(concat document frac))
                      (not (and (== (car what) 'concat)
                                (list-or (map (cut tm-func? <> 'select-region)
                                              (cdr what))))))
                 (random-pick (list "" "Z" `(concat "<" ,w ">") w `(concat ,w ,w)
                                    "aa" `(frac ,w "1")
                                    (if (== (car what) 'document)
                                        `(document ,w ,v) `(concat ,w "a"))))
                 (random-pick '("" "Z" "aa" "ab" "b" "xab"))))
         (prefs (case (random-next 8)
                  ((0) '("case-insensitive-match"))
                  ((1) '("allow-partial-replace"))
                  ((2) '("allow-initial-replace"))
                  (else '()))))
    (list doc what by prefs)))

(define (replace-in-strings t what by)
  ;; the occurrences of the string what in the strings of t replaced by by,
  ;; from left to right (not in the variable and the value of a with)
  (cond ((string? t)
         (let loop ((i 0) (acc ""))
           (with j (string-search-forwards what i t)
             (if (< j 0) (string-append acc (substring t i (string-length t)))
                 (loop (+ j (string-length what))
                       (string-append acc (substring t i j) by))))))
        ((and (pair? t) (== (car t) 'with))
         (append (list-head t (- (length t) 1))
                 (list (replace-in-strings (cAr t) what by))))
        ((pair? t) (cons (car t) (map (cut replace-in-strings <> what by)
                                      (cdr t))))
        (else t)))

(define (simplified t) (tm->stree (tree-simplify (stree->tree t))))

(define (text-and-tags t)
  ;; the strings and the tags of t in order, without its formulas and
  ;; documents: the editor makes a paragraph of a block in a formula (a
  ;; theorem) when the formula changes, which changes no text
  (let loop ((l (let walk ((t t))
                  (cond ((string? t) (if (== t "") (list) (list t)))
                        ((and (pair? t) (in? (car t) '(concat document)))
                         (append-map walk (cdr t)))
                        ((pair? t) (cons (list (car t))
                                         (append-map walk (cdr t))))
                        (else (list))))))
    (cond ((or (null? l) (null? (cdr l))) l)
          ((and (string? (car l)) (string? (cadr l)))
           (loop (cons (string-append (car l) (cadr l)) (cddr l))))
          (else (cons (car l) (loop (cdr l)))))))

(define (chars-and-tags t keep?)
  ;; the characters and the tags of t at the root, in order, which keep?
  ;; keeps (it gets the position before and after each), without the
  ;; formulas and documents
  (let walk ((t t) (q (list)))
    (cond ((string? t)
           (let loop ((i 0) (acc (list)))
             (if (>= i (string-length t)) (reverse acc)
                 (loop (+ i 1)
                       (if (keep? (rcons q i) (rcons q (+ i 1)))
                           (cons (substring t i (+ i 1)) acc) acc)))))
          ((and (pair? t) (in? (car t) '(concat document)))
           (append-map (lambda (k) (walk (list-ref t (+ k 1)) (rcons q k)))
                       (iota (- (length t) 1))))
          ((pair? t)
           (append (if (keep? (rcons q 0) (rcons q 1)) (list (list (car t)))
                       (list))
                   (append-map (lambda (k) (walk (list-ref t (+ k 1))
                                                 (rcons q k)))
                               (iota (- (length t) 1)))))
          (else (list)))))

(define (subsequence? xs ys)
  (cond ((null? xs) #t)
        ((null? ys) #f)
        ((== (car xs) (car ys)) (subsequence? (cdr xs) (cdr ys)))
        (else (subsequence? xs (cdr ys)))))

(define (kept-outside? trace result)
  ;; each replacement of trace (the document before it, and its range)
  ;; kept all the text outside its range: in the next document, in order
  (let loop ((l (reverse trace)))
    (or (null? l)
        (let* ((d (caar l)) (a (cadar l)) (b (caddar l))
               (next (if (null? (cdr l)) result (caadr l)))
               (outside (chars-and-tags d (lambda (x y)
                                            (or (path-less? x a)
                                                (path-less? b y))))))
          (and (subsequence? outside (chars-and-tags next (lambda (x y) #t)))
               (loop (cdr l)))))))

(define (with-preferences-on l thunk)
  ;; the value of thunk with the preferences l on (in memory only, and
  ;; restored also when thunk fails)
  (if (null? l) (thunk)
      (with-preference-value (car l) "on"
        (lambda () (with-preferences-on (cdr l) thunk)))))

(define (random-case-problems c)
  ;; the checks which the random case c fails (a case which does not end
  ;; is caught by the time limit of the suite)
  (let* ((doc (car c)) (what (cadr c)) (by (caddr c)) (prefs (cadddr c)))
    (with-preferences-on prefs
      (lambda ()
        (catch #t
          (lambda ()
            (let* ((no-skip (widget '(set! replace-skip-message #f)))
                   (all (replace-all-in-buffer doc what by))
                   (no-trace (widget '(set! replace-trace (list))))
                   (one (replace-one-repeatedly doc what by))
                   (trace (widget 'replace-trace))
                   (no-trace2 (widget '(set! replace-trace #f)))
                   (skip (widget 'replace-skip-message))
                   (hits (tree-search-tree (stree->tree doc) (stree->tree what)
                                           '() 1000))
                   (expect (and (string? what) (string? by) (null? prefs)
                                (not random-upper?)
                                (simplified (replace-in-strings doc what by)))))
              (append
                (if (and (pair? all) (== (cadr all) doc)) '() '(undo))
                (if (and (pair? one) (== (car one) 'runaway)) '(runaway) '())
                (if (and (pair? all) (== (simplified (car all)) (simplified one)))
                    '() '(all-differs-from-one))
                (if (or (nnull? hits) (and (pair? all) (== (car all) doc)))
                    '() '(change-without-hit))
                (if (or (not expect)
                        (and (pair? all) (== (text-and-tags (car all))
                                             (text-and-tags expect))))
                    '() '(string-replacement))
                ;; nothing outside the range of each replacement changed
                (if (or (not (pair? one)) (== (car one) 'runaway)
                        (kept-outside? trace one))
                    '() '(text-outside-changed))
                ;; a hit of a string or formula pattern is never left: the
                ;; search gives only hits which Replace can replace (a hit
                ;; of a tag pattern whose values would lose text is left)
                (if (and skip (or (string? what) (tm-func? what 'concat)
                                  (tm-func? what 'document)))
                    (list (list 'skip skip)) '()))))
          (lambda args (list 'error args)))))))

(define (random-replace-failures n seed)
  ;; the cases among n random ones which fail a check, with the checks
  (set-random-seed! seed)
  (with-search-mode-value 'replace
    (lambda ()
      (let loop ((i 0) (bad (list)))
        (if (>= i n) (reverse bad)
            (let* ((c (random-case))
                   (p (random-case-problems c)))
              (loop (+ i 1) (if (null? p) bad (cons (list c p) bad)))))))))

;; An empty match: Replace goes on after its replacement, and the next match
;; starts at least one character or tag later; after a match, no empty match
;; where it ends (as sed and vim do: a lone wildcard as (concat w), the hits
;; of the search). The replacement was found again and again, until a cap on
;; the number of steps; a lone wildcard was also replaced by the empty match
;; after it
(define (test-replace-empty-matches)
  (check-group "replace empty matches")
  (with-search-mode 'replace
    (lambda ()
      (check-replace-rows
        '(((document "ab ab") (wildcard "w") "X" (document "X"))
          ((document "ab ab") (concat (wildcard "w")) "X" (document "X"))
          ((document "ab ab") (wildcard "w") (concat "<" (wildcard "w") ">")
           (document "<ab ab>"))
          ((document "ab ab") (concat (wildcard "w"))
           (concat "<" (wildcard "w") ">") (document "<ab ab>"))
          ((document "ab") (select-region "") "X" (document "X"))
          ((document "aab") (concat (select-region (wildcard "w")) "a") "X"
           (document "XaXab"))
          ((document (concat (with "color" "red" "") (label "1") "a"))
           (concat (select-region (wildcard "w")) "a") "X"
           (document "Xa"))))
      ;; after an empty match, a tag counts as a token, also without text:
      ;; the hits after rules or empty labels were left out (each hit: the
      ;; match, and the start of the whole hit, which the filter judges)
      (with from (lambda (doc what)
                   (tree-search-from (stree->tree doc) (stree->tree what)
                                     '() '(0 0 0) 1 #t #t))
        (check= (from '(document (concat (hrule) (hrule) (hrule) "a")) "a")
                '((0 3 0) (0 3 1) (0 3 0)))
        (check= (from '(document (concat (label "") (label "") (label "")
                                         (label "")))
                      '(label (wildcard "v")))
                '((0 1 0) (0 1 1) (0 1 0)))))))

;; A lone wildcard is a formula of it: one hit in each paragraph, never
;; across their borders (the maintainer's decision); it took all of the
;; document, and Replace put a document inside a paragraph
(define (test-lone-wildcard)
  (check-group "lone wildcard")
  (with-search-mode 'replace
    (lambda ()
      (for (w '((wildcard "w") (concat (wildcard "w"))))
        (check= (hits '(document "ab" "cd") w) '((0 0) (0 2) (1 0) (1 2)))
        (check= (hits '(document "ab" (theorem (document "x" "y")) "cd") w)
                '((0 0) (0 2) (1 0 0 0) (1 0 0 1) (1 0 1 0) (1 0 1 1)
                  (2 0) (2 2))))
      (check-replace-rows
        (append-map
          (lambda (w)
            `(((document "ab" "cd") ,w (concat "<" (wildcard "w") ">")
               (document "<ab>" "<cd>"))
              ((document "ab" "cd" "ef") ,w (concat "<" (wildcard "w") ">")
               (document "<ab>" "<cd>" "<ef>"))
              ((document "ab" (theorem (document "x" "y")) "cd") ,w
               (concat "<" (wildcard "w") ">")
               (document "<ab>" (theorem (document "<x>" "<y>")) "<cd>"))))
          '((wildcard "w") (concat (wildcard "w")))))
      ;; after an empty match, the hit at the start of the text after it is
      ;; left out, and the search goes on for the next one
      (with doc '(document (concat (strong "x") (strong "y") (strong "z")))
        (check= (tree-search-from (stree->tree doc)
                                  (stree->tree '(strong (wildcard "w")))
                                  '() '(0 0) 1 #t #t)
                '((0 1 0) (0 1 1) (0 1 0))))
      ;; a tag alone in its paragraph (a theorem, a table): after the last
      ;; paragraph of its text, the rest of the tag takes no token, and is
      ;; no hit; it was one, from the end of that text to after the tag
      ;; (a trailing wildcard carries a hit to the end of the paragraph),
      ;; which Replace left with "Match left" (#589)
      (let* ((thm '(document (theorem (document "ab" "cd"))))
             (tab '(document (tabular (table (row (cell (document "ab"
                                                                   "cd"))))))))
        (for (w '((wildcard "w") (concat (wildcard "w"))
                  (concat (wildcard "w") (wildcard "v"))))
          (check= (tree-search-from (stree->tree thm) (stree->tree w)
                                    '() '(0 0 1 2) 1 #f #t)
                  '())
          (check= (tree-search-from (stree->tree tab) (stree->tree w)
                                    '() '(0 0 0 0 0 1 2) 1 #f #t)
                  '())
          (for (row `((,thm (document (theorem (document "X" "X"))))
                      (,tab (document (tabular (table (row (cell (document
                                                                   "X"
                                                                   "X")))))))
                      ((document "ab" (theorem (document "x" "y")))
                       (document "X" (theorem (document "X" "X"))))))
            (check= (replace-with-messages (car row) w "X")
                    (list (cadr row) '() '())))))
      ;; a pattern of several paragraphs: a hit which is only the break
      ;; between two paragraphs is no empty hit (the check of #589 left such
      ;; hits out, and Replace stopped)
      (check= (tree-search-from (stree->tree '(document "ab" "" "" "ef"))
                                (stree->tree '(document "" ""))
                                '() '(1 0) 2 #f #f)
              '((1 0) (2 0) (1 0) (2 0) (3 0) (2 0)))
      (for (row '(((document "ab" "" "" "ef") (document "" "") "" "abef")
                  ((document "ab" "" "" "ef") (document "" "") "Z"
                   "abZZZef")
                  ((document "a" "" "" "" "b") (document "" "") "" "ab")
                  ((document "ab" "" "" "ef")
                   (document (wildcard "w") (wildcard "v")) "X" "XXX")))
        (check= (car (replace-all-in-buffer (car row) (cadr row)
                                            (caddr row)))
                `(document ,(cadddr row))))
      ;; (the same in a document inside a tag: a theorem, a cell)
      (for (row '(((theorem (document "ab" "" "" "ef")) (document "" "") ""
                   (theorem (document "abef")))
                  ((theorem (document "ab" "" "" "ef")) (document "" "") "Z"
                   (theorem (document "abZZZef")))
                  ((theorem (document "a" "" "" "" "b")) (document "" "") ""
                   (theorem (document "ab")))
                  ((theorem (document "ab" "" "" "ef"))
                   (document (wildcard "w") (wildcard "v")) "X"
                   (theorem (document "XXX")))
                  ((tabular (table (row (cell (document "ab" "" "" "ef")))))
                   (document "" "") ""
                   (tabular (table (row (cell (document "abef"))))))))
        (check= (car (replace-all-in-buffer `(document ,(car row)) (cadr row)
                                            (caddr row)))
                `(document ,(cadddr row))))
      ;; (the rest of the text is a hit)
      (check= (tree-search-from (stree->tree '(document (theorem (document
                                                                   "ab"
                                                                   "cd"))))
                                (stree->tree '(concat (wildcard "w")))
                                '() '(0 0 1 1) 1 #f #t)
              '((0 0 1 1) (0 0 1 2) (0 0 1 0))))))

(define (replace-with-messages doc what by)
  ;; the result of Replace All in a buffer holding doc, and the messages of
  ;; the hits left by Replace All and by Replace one again and again
  (let* ((run (lambda (all?)
                (with-search-buffers doc what by
                  (lambda (u)
                    (ahash-set! (widget 'search-filter-table)
                                (current-buffer) (stree->tree '(attr)))
                    ((widget 'perform-search*))
                    (let loop ((i 0) (msgs (list)))
                      (widget '(set! replace-skip-message #f))
                      (with old (tm->stree (buffer-get-body u))
                        (if all? (replace-all) (replace-one))
                        (let* ((m (widget 'replace-skip-message))
                               (msgs (if m (cons m msgs) msgs)))
                          (if (or all? (>= i 20)
                                  (== (tm->stree (buffer-get-body u)) old))
                              (list (tm->stree (buffer-get-body u))
                                    (reverse msgs))
                              (loop (+ i 1) msgs)))))))))
         (all (run #t))
         (one (run #f)))
    (list (car all) (cadr all) (cadr one))))

;; The wildcards of a region pattern take their values in the region, as its
;; content; the values were looked for by the literal parts around the
;; region, which are outside it (and by a case-sensitive search), and the
;; hit was left
(define (test-replace-region-wildcards)
  (check-group "replace region wildcards")
  (with-search-mode 'replace
    (lambda ()
      (check-replace-rows
        '(((document (concat "axyb a" (strong "x") "yb"))
           (concat "a" (select-region (wildcard "w")) "b")
           (concat "[" (wildcard "w") "]")
           (document (concat "a[xy]b a[" (strong "x") "y]b")))
          ((document "axyb")
           (concat "a" (select-region (concat "x" (wildcard "w"))) "b")
           (concat "<" (wildcard "w") ">")
           (document "a<y>b"))
          ;; a wildcard outside the region has no value: left
          ((document "axyb")
           (concat (wildcard "v") "a" (select-region (wildcard "w")) "b")
           (concat (wildcard "v") (wildcard "w"))
           (document "axyb"))))
      (with-preference "case-insensitive-match" "on"
        (lambda ()
          (check-replace-rows
            '(((document "AxyB aXYb")
               (concat "a" (select-region (concat "x" (wildcard "w"))) "b")
               (concat "<" (wildcard "w") ">")
               (document "A<y>B a<Y>b")))))))))

;; Replace one and Replace All are one step of undo, also when a step fails:
;; the document is as before, and the error goes on. The document kept the
;; partial edit (at the failing step, the match was removed and nothing
;; inserted), out of the history of undo
(define (replace-with-failure doc what by n all?)
  ;; Replace All (if all?) or Replace one in a buffer holding doc, with the
  ;; insertion failing at the n-th replacement: whether it failed, the body
  ;; afterwards, and the steps of undo
  (with-search-buffers doc what by
    (lambda (u)
      (ahash-set! (widget 'search-filter-table)
                  (current-buffer) (stree->tree '(attr)))
      (clear-undo-history)
      ((widget 'perform-search*))
      (let* ((old insert-go-to)
             (k 0)
             (failing (lambda (t p)
                        (set! k (+ k 1))
                        (if (= k n) (error "injected failure") (old t p))))
             (r (dynamic-wind
                  (lambda () (set! insert-go-to failing))
                  (lambda ()
                    (catch #t
                      (lambda () (if all? (replace-all) (replace-one)) 'done)
                      (lambda args 'failed)))
                  (lambda () (set! insert-go-to old)))))
        (list r (tm->stree (buffer-get-body u)) (undo-possibilities))))))

(define (test-replace-failures)
  (check-group "replace failures")
  (with-search-mode 'replace
    (lambda ()
      (let* ((doc '(document "a1x a2x a3x"))
             (what '(concat "a" (wildcard "w") "x"))
             (by '(concat "[" (wildcard "w") "]")))
        (for (n '(1 2 3))
          (check= (replace-with-failure doc what by n #t)
                  (list 'failed doc 0)))
        (check= (replace-with-failure doc what by 1 #f) (list 'failed doc 0))
        ;; (without a failure)
        (check= (replace-with-failure doc what by 4 #t)
                '(done (document "[1] [2] [3]") 1))))))

(define (test-replace-filters)
  (check-group "replace with a filter")
  ;; Replace goes on after the hits which the filter of the search rejects
  ;; (a paragraph of them stopped it)
  (with-search-mode 'replace
    (lambda ()
      (with-test-filter '(attr "mode" "math")
        (lambda ()
          (check-replace-rows
            '(((document (math "a") "a" (math "a")) "a" "Z"
               (document (math "Z") "a" (math "Z")))))))
      (with-test-filter '(attr "mode" "text" "language" "french")
        (lambda ()
          (check-replace-rows
            '(((document (with "language" "french" "a") "a"
                         (with "language" "french" "a"))
               "a" "Z"
               (document (with "language" "french" "Z") "a"
                         (with "language" "french" "Z")))))))
      ;; in a source document, also the hidden content, as the search
      ;; (which shows it)
      (with-test-source-mode
        (lambda ()
          (check= (car (replace-all-in-buffer '(document (folded "q" "ab ab"))
                                              "ab" "Z"))
                  '(document (unfolded "q" "Z Z")))))
      ;; a hit which is not a match is left with a message (an error, after
      ;; which the editing did not end), and Replace goes on
      (begin
        (check= (with-search-buffers '(document "xyz a1b")
                  '(concat "a" (wildcard "w") "b") '(concat "<" (wildcard "w") ">")
                  (lambda (u)
                    (ahash-set! (widget 'search-filter-table)
                                (current-buffer) (stree->tree '(attr)))
                    (widget '(set! replace-skip-message #f))
                    (with p (tree->path (buffer-tree))
                      (set-alt-selection "alternate"
                                         (list (append p '(0 0))
                                               (append p '(0 3)))))
                    (replace-one)
                    (list (tm->stree (buffer-get-body u))
                          (string? (widget 'replace-skip-message)))))
                '((document "xyz a1b") #t))))))

;; Replace next to a tag whose border has no cursor position (hidden): when
;; the cut of the match left the tag alone in its paragraph, or the match
;; started right after it, the editor corrected the cursor into the tag,
;; and the replacement went into it (#590); a hit inside it, shown by the
;; search, changed the document outside the history of undo, and cancelled
;; the hits and the reference of the search: Replace one did not go on
;; after its replacement (#591)
(define (search-in-buffer doc what . read-only?)
  ;; the real search of the widget (perform-search*) in a buffer holding
  ;; doc, in source mode if test-source-mode? (read-only if read-only?):
  ;; the body afterwards, the number of steps of undo, the body after one
  ;; undo, whether the hits and the reference of the search are still there
  ;; after the search, and the selected hit (relative to the buffer)
  (with-search-buffers doc what ""
    (lambda (u)
      (ahash-set! (widget 'search-filter-table)
                  (current-buffer) (stree->tree '(attr)))
      (when test-source-mode? (init-env "mode" "src"))
      (clear-undo-history)
      (when (and (pair? read-only?) (car read-only?))
        (buffer-set-read-only u #t))
      ((widget 'perform-search*))
      (let* ((res (tm->stree (buffer-get-body u)))
             (n (undo-possibilities))
             (kept? (and (nnull? (get-alt-selection "alternate"))
                         (nnull? (get-alt-selection "search-reference"))))
             (k (length (tree->path (buffer-tree))))
             (sel (if (selection-active-any?)
                      (list (list-tail (selection-get-start) k)
                            (list-tail (selection-get-end) k))
                      (list))))
        (buffer-set-read-only u #f)
        (undo 0)
        (list res n (tm->stree (buffer-get-body u)) kept? sel)))))

(define (replace-type-undo doc what by)
  ;; Replace in a buffer holding doc, then typeset, type a letter, typeset:
  ;; the body after each step
  (with-search-buffers doc what by
    (lambda (u)
      (ahash-set! (widget 'search-filter-table)
                  (current-buffer) (stree->tree '(attr)))
      (clear-undo-history)
      ((widget 'perform-search*))
      (replace-one)
      (update-forced)
      (let* ((r1 (tm->stree (buffer-get-body u)))
             (n (begin (insert "y") (update-forced) (undo-possibilities)))
             (r2 (tm->stree (buffer-get-body u))))
        (list r1 r2)))))

(define (test-replace-next-to-hidden)
  (check-group "replace next to hidden content")
  (with-search-mode 'replace
    (lambda ()
      ;; the document typeset after the replacement (repro 2 of #704: the
      ;; hidden content alone in the document was replaced by an empty one)
      (check= (list (replace-type-undo '(document (concat "a1x" (hidden "b")))
                                       "a1x" "")
                    (replace-type-undo '(document (concat "a1x" (hidden "b")))
                                       "a1x" "Z")
                    (replace-type-undo '(document (concat "a1x" (hidden "b")))
                                       "a1x" '(document "P" "Q")))
              '(((document (hidden "b"))
                 (document (concat "y" (hidden "b"))))
                ((document (concat "Z" (hidden "b")))
                 (document (concat "Zy" (hidden "b"))))
                ((document "P" (concat "Q" (hidden "b")))
                 (document "P" (concat "Qy" (hidden "b"))))))
      (check-replace-rows
        '(((document (concat "a1x" (hidden "b"))) "a1x" "Z"
           (document (concat "Z" (hidden "b"))))
          ((document (concat (hidden "b") "a1x")) "a1x" "Z"
           (document (concat (hidden "b") "Z")))
          ((document (concat (hidden "b") "a1x" (hidden "c"))) "a1x"
           (strong "Q")
           (document (concat (hidden "b") (strong "Q") (hidden "c"))))
          ((document (concat "a1x" (hidden "b"))) "a1x" ""
           (document (hidden "b")))
          ((document (concat (hidden "b") "a1x")) "a1x" ""
           (document (hidden "b")))
          ;; the hit inside the tag shows it (an undoable change)
          ((document (concat "a1x" (hidden "a2x")))
           (concat "a" (wildcard "w") "x") "Z"
           (document (concat "Z" (shown "Z"))))
          ((document (concat "a1x" (hidden (concat "a2x" (hidden "b")))))
           (concat "a" (wildcard "w") "x") "Z"
           (document (concat "Z" (shown (concat "Z" (hidden "b"))))))
          ;; (as before)
          ((document (concat "a1x" (strong "b"))) "a1x" "Z"
           (document (concat "Z" (strong "b"))))))
      ;; Replace one, which shows the next hit, is one step of undo
      (check= (with-search-buffers '(document (concat "a1x" (hidden "a2x")))
                '(concat "a" (wildcard "w") "x") "Z"
                (lambda (u)
                  (ahash-set! (widget 'search-filter-table)
                              (current-buffer) (stree->tree '(attr)))
                  (clear-undo-history)
                  ((widget 'perform-search*))
                  (replace-one)
                  (let* ((res (tm->stree (buffer-get-body u)))
                         (n (undo-possibilities)))
                    (undo 0)
                    (list res n (tm->stree (buffer-get-body u))))))
              '((document (concat "Z" (shown "a2x"))) 1
                (document (concat "a1x" (hidden "a2x")))))
      ;; in a read-only document, Replace one replaces nothing, but the
      ;; search goes on: the hits are kept, the next one is selected and
      ;; shown (as before, without a step of undo)
      (check= (with-search-buffers '(document (concat "a1x" (hidden "a2x")))
                '(concat "a" (wildcard "w") "x") "Z"
                (lambda (u)
                  (ahash-set! (widget 'search-filter-table)
                              (current-buffer) (stree->tree '(attr)))
                  (clear-undo-history)
                  (buffer-set-read-only u #t)
                  ((widget 'perform-search*))
                  (replace-one)
                  (with r (list (tm->stree (buffer-get-body u))
                                (undo-possibilities)
                                (length (get-alt-selection "alternate"))
                                (selection-active-any?)
                                (cursor-accessible?))
                    (buffer-set-read-only u #f)
                    r)))
              '((document (concat "a1x" (shown "a2x"))) 0 4 #t #t))
      ;; Replace one shows the next hit, Replace All only where it ends
      (with doc '(document (concat "a1x" (hidden "a2x") "a3x"))
        (check= (replace-one-repeatedly doc '(concat "a" (wildcard "w") "x")
                                        "Z")
                '(document (concat "Z" (shown "Z") "Z")))
        (check= (replace-all-in-buffer doc '(concat "a" (wildcard "w") "x")
                                       "Z")
                (list '(document (concat "Z" (hidden "Z") "Z")) doc)))))
  (with-search-mode 'search
    (lambda ()
      ;; going to a hit inside hidden content shows it, in a step of undo,
      ;; and keeps the hits and the reference of the search
      (check= (search-in-buffer '(document (concat "x " (hidden "a2x")))
                                "a2x")
              '((document (concat "x " (shown "a2x"))) 1
                (document (concat "x " (hidden "a2x"))) #t ((0 1 0 0) (0 1 0 3))))
      ;; in a read-only document, shown as before (the end of a step of
      ;; undo would cancel it), and without a step of undo
      (check= (search-in-buffer '(document (concat "x " (hidden "a2x")))
                                "a2x" #t)
              '((document (concat "x " (shown "a2x"))) 0
                (document (concat "x " (shown "a2x"))) #t ((0 1 0 0) (0 1 0 3))))
      (with-test-source-mode
        (lambda ()
          (check= (search-in-buffer '(document (concat (folded "q" "a2x")
                                                       "a3x"))
                                    "a2x")
                  '((document (concat (unfolded "q" "a2x") "a3x")) 1
                    (document (concat (folded "q" "a2x") "a3x")) #t
                    ((0 0 1 0) (0 0 1 3))))))
      ;; a switch which shows a hit may wrap a branch next to it, whose
      ;; hits then move: they are not set again as they were (the search
      ;; which go-to* schedules finds them again)
      (with-search-buffers '(document (concat "x " (tiny-switch (hidden "a2x")
                                                                "a3x")))
        "a" ""
        (lambda (u)
          (ahash-set! (widget 'search-filter-table)
                      (current-buffer) (stree->tree '(attr)))
          ((widget 'perform-search*))
          (check-true
           (list-and (map (lambda (p) (tree-atomic? (path->tree (cDr p))))
                          (get-alt-selection "alternate"))))))
      ;; (nothing to show: nothing changes)
      (check= (search-in-buffer '(document (concat "x " (strong "a2x")))
                                "a2x")
              '((document (concat "x " (strong "a2x"))) 0
                (document (concat "x " (strong "a2x"))) #t ((0 1 0 0) (0 1 0 3)))))))

;; Replace by several paragraphs next to a tag whose border has no cursor
;; position: the editor corrected the cursor into the tag while it made the
;; new paragraph, and refused: the match was cut, and the replacement lost
;; (#675). Where the editor really refuses paragraphs (inside a line, in a
;; cell without paragraphs, in a formula), the match is left, with a message
(define (test-replace-by-paragraphs)
  (check-group "replace by several paragraphs next to a tag")
  (with-search-mode 'replace
    (lambda ()
      (let* ((by '(document "P" "Q"))
             (tab (lambda (x) `(tabular (tformat (table (row (cell ,x)
                                                             (cell "c"))))))))
        (for-each
          (lambda (tag)
            (check-replace-rows
              `(((document (concat "a1x" ,tag)) "a1x" ,by
                 (document "P" (concat "Q" ,tag)))
                ((document (concat ,tag "a1x")) "a1x" ,by
                 (document (concat ,tag "P") "Q"))
                ((document "u" (concat "a1x" ,tag) "v") "a1x" ,by
                 (document "u" "P" (concat "Q" ,tag) "v"))
                ((document "u" (concat ,tag "a1x") "v") "a1x" ,by
                 (document "u" (concat ,tag "P") "Q" "v"))
                ((document (concat ,tag "a1x" ,tag)) "a1x" ,by
                 (document (concat ,tag "P") (concat "Q" ,tag)))
                ;; the match is part of the text next to the tag, or all of
                ;; it with text beyond (#711)
                ((document (concat "z" ,tag "a1xb")) "a1x" ,by
                 (document (concat "z" ,tag "P") "Qb"))
                ((document (concat "ba1x" ,tag "z")) "a1x" ,by
                 (document "bP" (concat "Q" ,tag "z")))
                ((document (concat "z" ,tag "a1x") "next") "a1x" ,by
                 (document (concat "z" ,tag "P") "Q" "next"))
                ;; several matches
                ((document (concat "a1x" ,tag) (concat ,tag "a1x") "a1x")
                 "a1x" ,by
                 (document "P" (concat "Q" ,tag)
                           (concat ,tag "P") "Q" "P" "Q")))))
          `((hidden "b") (freeze "b") (unfreeze "b") (ignore "b") (shown "b")
            (table (row (cell "b"))) (expand* "foo" "b")
            ;; (a border with a cursor position: as before)
            (strong "b") ,(tab "b")))
        (check-replace-rows
          `(;; the tag is not shown: the cursor is no more inside it
            ;; (and it holds paragraphs, which get nothing)
            ((document (concat "a1x" (hidden (document "b" "c")))) "a1x" ,by
             (document "P" (concat "Q" (hidden (document "b" "c")))))
            ;; a place which holds paragraphs, inside a tag
            ((document (quotation (document (concat "a1x" (hidden "b")))))
             "a1x" ,by
             (document (quotation (document "P" (concat "Q" (hidden "b"))))))
            ((document (with "color" "red" (concat "a1x" (hidden "b"))))
             "a1x" ,by
             (document (with "color" "red"
                         (document "P" (concat "Q" (hidden "b"))))))
            ((document ,(tab '(document (concat "a1x" (hidden "b")))))
             "a1x" ,by
             (document ,(tab '(document "P" (concat "Q" (hidden "b"))))))
            ;; a block
            ((document "u" (concat "a1x" (hidden "b")) "v") "a1x"
             (quotation (document "P" "Q"))
             (document "u" (quotation (document "P" "Q")) (hidden "b") "v"))
            ;; a block after text, before a tag holding paragraphs: not in
            ;; the tag, which stays hidden
            ((document (concat "wa1x" (hidden (document "b" "c")))) "a1x"
             (quotation (document "P" "Q"))
             (document "w" (quotation (document "P" "Q"))
                       (hidden (document "b" "c"))))
            ;; no place for paragraphs: all is left
            ,@(map (lambda (x) `((document ,x) "a1x" ,by (document ,x)))
                   (list (tab "a1x") (tab "wa1xw")
                         (tab '(concat "a1x" (hidden "b")))
                         '(concat "t " (strong "a1x"))
                         '(concat "t " (with "color" "red"
                                         (concat "a1x" (hidden "b"))))
                         '(concat "t " (math "a1x+b"))))
            ;; those, among others which are replaced
            ((document (concat "a1x" (hidden "b")) ,(tab "a1x") "a1x") "a1x"
             ,by
             (document "P" (concat "Q" (hidden "b")) ,(tab "a1x") "P" "Q"))
            ;; (a single paragraph goes everywhere, as before)
            ((document ,(tab "a1x")) "a1x" "Z" (document ,(tab "Z")))))
        ;; the message of the match left
        (check= (with-search-buffers `(document ,(tab "a1x")) "a1x" by
                  (lambda (u)
                    (ahash-set! (widget 'search-filter-table)
                                (current-buffer) (stree->tree '(attr)))
                    (widget '(set! replace-skip-message #f))
                    ((widget 'perform-search*))
                    (replace-one)
                    (widget 'replace-skip-message)))
                "Match left: several paragraphs cannot go here")
        ;; Replace one is one step of undo
        (check= (with-search-buffers '(document (concat "a1x" (hidden "b")))
                  "a1x" by
                  (lambda (u)
                    (ahash-set! (widget 'search-filter-table)
                                (current-buffer) (stree->tree '(attr)))
                    (clear-undo-history)
                    ((widget 'perform-search*))
                    (replace-one)
                    (let* ((res (tm->stree (buffer-get-body u)))
                           (n (undo-possibilities)))
                      (undo 0)
                      (list res n (tm->stree (buffer-get-body u))))))
                '((document "P" (concat "Q" (hidden "b"))) 1
                  (document (concat "a1x" (hidden "b")))))))))

;; Replace one, then Replace All: Replace All goes on after the replacement,
;; as Replace one does; it took the hit which ends there, and replaced in
;; the replacement (#596)
(define (replace-one-then-all doc what by)
  ;; the real Replace one then the real Replace All: the body after each
  (with-search-buffers doc what by
    (lambda (u)
      (ahash-set! (widget 'search-filter-table)
                  (current-buffer) (stree->tree '(attr)))
      ((widget 'perform-search*))
      (replace-one)
      (with one (tm->stree (buffer-get-body u))
        (replace-all)
        (list one (tm->stree (buffer-get-body u)))))))

(define (many-hits-document n c)
  ;; n paragraphs of 25 hits of c
  (let* ((para (apply string-append (repeated-list 25 (string-append c " ")))))
    `(document ,@(repeated-list n para))))

(define (repeated-list n x)
  (let loop ((n n) (r (list)))
    (if (<= n 0) r (loop (- n 1) (cons x r)))))

(define (test-replace-followups)
  (check-group "replace one then all, filters, many hits")
  (with-search-mode 'replace
    (lambda ()
      (check= (replace-one-then-all '(document "foo foo foo") "foo" "barfoo")
              '((document "barfoo foo foo") (document "barfoo barfoo barfoo")))
      (check= (replace-one-then-all '(document "a a a") "a" "aa")
              '((document "aa a a") (document "aa aa aa")))
      (check= (replace-one-then-all '(document (concat "x" (math "x") "x"))
                                    "x" "xx")
              '((document (concat "xx" (math "x") "x"))
                (document (concat "xx" (math "xx") "xx"))))
      (check= (replace-one-then-all '(document "a1x a2x a3x")
                                    '(concat "a" (wildcard "w") "x")
                                    '(concat "a" (wildcard "w") "xa"))
              '((document "a1xa a2x a3x") (document "a1xa a2xa a3xa")))
      ;; the filter judges the start of the whole hit, as for the hits which
      ;; the search shows, also for the hits after the first one: the match
      ;; which Replace replaces in it may be in a formula, and only the
      ;; first paragraph was replaced
      (let* ((doc '(document (concat "p" (math "xab") "q")
                             (concat "r" (math "yab") "s")))
             (lone '(document (math "xab") (math "yab")))
             (what '(concat (wildcard "w") "b" (wildcard "v")))
             (by '(concat "<" (wildcard "w") "|" (wildcard "v") ">"))
             (shown (lambda (doc)
                      ;; the hits which the search shows
                      (with-search-buffers doc what by
                        (lambda (u)
                          (ahash-set! (widget 'search-filter-table)
                                      (current-buffer)
                                      (stree->tree test-search-filter))
                          ((widget 'perform-search*))
                          (with q (tree->path (buffer-tree))
                            (map (cut list-tail <> (length q))
                                 (get-alt-selection "alternate"))))))))
        (with-test-filter '(attr "mode" "text")
          (lambda ()
            (check= (shown doc) '((0 0 0) (0 2 1) (1 0 0) (1 2 1)))
            (check= (shown lone) '((0 0) (0 1) (1 0) (1 1)))
            (check= (shown '(document (concat "p" (math "xab") "q"
                                              (math "yab") "s")))
                    '((0 0 0) (0 4 1)))
            (check-replace-rows
              `((,doc ,what ,by
                 (document (concat "p" (math "<xa|>") "q")
                           (concat "r" (math "<ya|>") "s")))
                (,lone ,what ,by
                 (document (math "<xa|>") (math "<ya|>")))
                ;; one hit, the paragraph, with two matches: after the
                ;; replacement in the first formula, the rest of the hit is
                ;; judged as the hit which the search shows (it was left)
                ((document (concat "p" (math "xab") "q" (math "yab") "s"))
                 ,what ,by
                 (document (concat "p" (math "<xa|>") "q" (math "<ya|>")
                                   "s")))
                ((document (concat "p" (math "xabzb")))
                 (concat (wildcard "w") "b") (concat "<" (wildcard "w") ">")
                 (document (concat "p" (math "<xa><z>"))))
                ((document (concat "q" (strong "xabzb")))
                 (concat (wildcard "w") "b") (concat "<" (wildcard "w") ">")
                 (document (concat "q" (strong "<xa><z>"))))))))
        ;; without a filter, Replace does not look for the start of the
        ;; hit which the search shows, which only the filter judges (a
        ;; search of the whole paragraph at each step, #618): the same
        ;; replacements
        (with-test-filter '(attr)
          (lambda ()
            (check-replace-rows
              `(((document (concat "p" (math "xab") "q" (math "yab") "s"))
                 ,what ,by
                 (document (concat "p" (math "<xa|>") "q" (math "<ya|>")
                                   "s")))
                ((document (concat "p" (math "xabzb")))
                 (concat (wildcard "w") "b") (concat "<" (wildcard "w") ">")
                 (document (concat "p" (math "<xa><z>"))))))))
        (check= (map (lambda (filter)
                       (with-search-buffers '(document "a") "a" "b"
                         (lambda (u)
                           (ahash-set! (widget 'search-filter-table)
                                       (current-buffer) (stree->tree filter))
                           ((widget 'search-filter-accepts-all?)))))
                     '((attr) (attr "mode" "text")))
                '(#t #f))
        (with from (lambda (filtered?)
                     (tree-search-from (stree->tree '(document "xazb"))
                                       (stree->tree
                                         '(concat (wildcard "w") "b"))
                                       '() '(0 2) 1 #f filtered?))
          (check= (from #t) '((0 2) (0 4) (0 0)))
          (check= (from #f) '((0 2) (0 4) (0 2))))
        (with-test-filter '(attr "mode" "math")
          (lambda ()
            (check= (shown doc) '())
            (check= (shown lone) '())
            (check-replace-rows `((,doc ,what ,by ,doc) (,lone ,what ,by ,lone))))))
      ;; 12,000 hits: the filter of the hits overflowed the stack of Guile
      ;; (from about 10,000), and Replace All left the document as it was
      (let* ((doc (many-hits-document 480 "a"))
             (exp (many-hits-document 480 "b")))
        (with-test-filter '(attr "mode" "text")
          (lambda ()
            (check= (with-search-buffers doc "a" "b"
                      (lambda (u)
                        (ahash-set! (widget 'search-filter-table)
                                    (current-buffer)
                                    (stree->tree test-search-filter))
                        ;; (the hits which the search shows, all of them)
                        ((widget '(lambda () (perform-search-sub 100000 #t))))
                        (length (get-alt-selection "alternate"))))
                    24000)
            (with r (replace-all-in-buffer doc "a" "b")
              (check= (car r) exp)
              (check= (cadr r) doc))))))))

;; A pattern of several paragraphs next to a block (#687): the cut of the
;; paragraph break between a paragraph and a theorem joins them, and the
;; editor makes a paragraph of the theorem again. The document was as before,
;; the match still there, and Replace All and Replace one found it again and
;; again: they never ended. Such a match is left, with a message, and nothing
;; is inserted. A guard stops Replace All after more steps than twice the
;; size of the document
(define block-left "Match left: the editor keeps it, next to a block")

(define (with-no-change-detection thunk)
  ;; thunk, with Replace blind to a replacement which changes nothing
  (let* ((old (widget 'range-unchanged?)))
    (dynamic-wind
      (lambda () (widget '(set! range-unchanged? (lambda args #f))))
      thunk
      (lambda () ((widget '(lambda (f) (set! range-unchanged? f))) old)))))

(define (test-replace-next-to-blocks)
  (check-group "replace next to a block")
  (with-search-mode 'replace
    (lambda ()
      (check-replace-rows
        '(;; many empty paragraphs: each one is a step of Replace All, which
          ;; the guard counts (it stopped early when it counted characters
          ;; only)
          ((document "" "" "" "" "" "" "" "" "" "") (document "" "") ""
           (document ""))
          ((document "" "" "" "" "" "" "" "" "" "") (document "" "") "Z"
           (document "ZZZZZZZZZ"))
          ((document "a" "" "" "" "" "" "" "" "" "")
           (document "" "" "") ""
           (document "a" ""))
          ;; the cases of the issue
          ((document "x" (theorem (document "ab" "" "" "ef")) "y")
           (document "" "") ""
           (document "x" (theorem (document "abef")) "y"))
          ((document "ab" (theorem (document "" "")) "ef")
           (document "" "") ""
           (document "ab" (theorem (document "")) "ef"))
          ((document "ab" "" (theorem (document "cd")) "" "ef")
           (document "" "") ""
           (document "ab" (theorem (document "cd")) "ef"))
          ;; only breaks next to a block: nothing replaced, also with a
          ;; replacement, which was inserted again and again
          ((document "x" (theorem (document "ab")) "y")
           (document "" "") ""
           (document "x" (theorem (document "ab")) "y"))
          ((document "x" (theorem (document "ab")) "y")
           (document "" "") "Z"
           (document "x" (theorem (document "ab")) "y"))
          ((document (theorem (document "a")) (theorem (document "b")))
           (document "" "") ""
           (document (theorem (document "a")) (theorem (document "b"))))
          ;; the breaks inside the block are replaced
          ((document "x" (theorem (document "ab" "cd")) "y")
           (document "" "") "Z"
           (document "x" (theorem (document "abZcd")) "y"))
          ((document "x" (proof (document "a" "")) "" "y")
           (document "" "") ""
           (document "x" (proof (document "a")) "y"))
          ((document "ab" "" (theorem (document "cd")) "" "ef")
           (document "" "") (document "" "")
           (document "ab" "" (theorem (document "cd")) "" "ef"))
          ;; a table is not a block: the paragraphs are joined
          ((document "x" (tabular (table (row (cell (document "a" "" "b")))))
                     "y")
           (document "" "") ""
           (document (concat "x" (tabular (table (row (cell (document "ab")))))
                             "y")))))
      ;; the message of the match left, by Replace All and by Replace one
      (check= (replace-with-messages
                '(document "x" (theorem (document "ab")) "y")
                '(document "" "") "Z")
              (list '(document "x" (theorem (document "ab")) "y")
                    (list block-left) (list block-left)))
      ;; the guard: blind to the replacements which change nothing, Replace
      ;; All stops, with a message, in one step of undo
      (with-no-change-detection
        (lambda ()
          (for (c '(((document "x" (theorem (document "ab" "" "" "ef")) "y")
                     "")
                    ((document "x" (theorem (document "ab")) "y") "Z")))
            (widget '(set! replace-skip-message #f))
            (with r (replace-all-in-buffer (car c) '(document "" "") (cadr c))
              (check= (cadr r) (car c))
              (check= (widget 'replace-skip-message)
                      "Replace All stopped: it would not end"))))))))

(define (random-block-paragraph d)
  ;; a paragraph: a short string, maybe empty, or a block of paragraphs
  (if (and (> d 0) (< (random-next 10) 3))
      (list (random-pick '(theorem proof))
            `(document ,@(map (lambda (i) (random-block-paragraph (- d 1)))
                              (iota (+ 1 (random-next 3))))))
      (random-pick '("" "" "a" "ab" "b"))))

(define (random-block-case)
  ;; a document of paragraphs and blocks, a pattern of several paragraphs
  (let* ((w '(wildcard "w")) (v '(wildcard "v"))
         (doc (simplified `(document ,@(map (lambda (i)
                                              (random-block-paragraph 2))
                                            (iota (+ 1 (random-next 5)))))))
         (what (random-pick `((document "" "") (document "" "" "")
                              (document ,w "") (document "" ,w)
                              (document "a" "") (document "" "b")
                              (document ,w ,v))))
         (by (random-pick '("" "Z" (document "" "") (document "Z" "")))))
    (list doc what by '())))

(define (random-block-failures n seed)
  ;; the cases among n random ones of random-block-case which fail a check
  ;; of random-case-problems, but for the matches left next to a block
  (set-random-seed! seed)
  (set! random-upper? #f)
  (with-search-mode-value 'replace
    (lambda ()
      (let loop ((i 0) (bad (list)))
        (if (>= i n) (reverse bad)
            (let* ((c (random-block-case))
                   (p (list-filter (random-case-problems c)
                                   (lambda (x)
                                     (!= x (list 'skip block-left))))))
              (loop (+ i 1) (if (null? p) bad (cons (list c p) bad)))))))))

(define (test-replace-random)
  (check-group "replace random")
  (let* ((t0 (texmacs-time))
         (bad (random-replace-failures 200 543)))
    (display* "    replace random: 200 cases in " (- (texmacs-time) t0)
              " ms\n")
    (check= bad '()))
  ;; patterns of several paragraphs among blocks (#687)
  (let* ((t0 (texmacs-time))
         (bad (random-block-failures 60 687)))
    (display* "    replace random blocks: 60 cases in " (- (texmacs-time) t0)
              " ms\n")
    (check= bad '())))

(tm-define (search-test-failures)
  (check-suite "search")
  (test-text)
  (test-text-wildcards)
  (test-tags)
  (test-regions)
  (test-regions-in-arguments)
  (test-formulas)
  (test-formula-wildcards)
  (test-hits-after-a-hit)
  (test-wildcards-and-several-hits)
  (test-hits-of-a-string-after-a-tag)
  (test-overlapping-strings)
  (test-paragraphs)
  (test-limit-and-position)
  (test-position-inside-a-match)
  (test-position-after-a-one-argument-tag)
  (test-middle-strings)
  (test-strings-and-wildcards-first)
  (test-hits-after-a-match)
  (test-speed)
  (test-long-strings)
  (test-long-concats)
  (test-parts-before-wildcards)
  (test-text-same-in-both)
  (test-formulas-in-arguments)
  (test-match-after-a-tag)
  (test-lone-tags)
  (test-search-contains-replace)
  (test-speed-in-arguments)
  (test-replace-wildcards)
  (test-replace-in-buffers)
  (test-replace-filters)
  (test-replace-next-to-hidden)
  (test-replace-by-paragraphs)
  (test-replace-failures)
  (test-replace-empty-matches)
  (test-lone-wildcard)
  (test-replace-region-wildcards)
  (test-replace-followups)
  (test-replace-next-to-blocks)
  (test-replace-random)
  (check-end))
