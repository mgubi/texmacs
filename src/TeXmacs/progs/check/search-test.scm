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
;; The search widget and the replace widget match differently. The search
;; widget sets the preference "search-and-replace" to "off", which allows
;; blank, initial, partial and injective matches: a string of the pattern
;; may match a part of a string, an empty argument matches any argument,
;; and a tag matches a tag whose arguments contain those of the pattern.
;; The replace widget sets it to "on": a string must match a whole string.
;; Both allow cascaded matches (a pattern inside an argument). The checks
;; set the preference in memory only, and restore it: nothing is saved.
;;
;; Wildcards (std 1, std 2, std 3 in the widgets) are (wildcard "x"), ...;
;; at the start or at the end of a concat pattern, a wildcard extends the
;; hit to the start or to the end of the formula (at the end, not always:
;; #453).

(texmacs-module (check search-test)
  (:use (check check-lib)))

(define (with-search-mode mode thunk)
  ;; mode is 'search (the search widget) or 'replace (the replace widget)
  (let* ((which "search-and-replace")
         (had? (cpp-has-preference? which))
         (old (get-preference which)))
    (cpp-set-preference which (if (== mode 'search) "off" "on"))
    (with r (check-run thunk)
      (if had?
          (cpp-set-preference which old)
          (cpp-reset-preference which))
      (when (and (pair? r) (== (car r) 'error))
        (check-report #f "the group" (object->string r))))))

(define (hits doc what)
  (tree-search-tree (stree->tree doc) what '() 1000))

(define (check-both doc what expected)
  ;; the same hits for the search and for the replace widget
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
  ;; case sensitive by default
  (check-both '(document "Cat cat CAT") "cat" '((0 4) (0 7))))

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
                       (math (concat (frac (concat "a+b")
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
  ;; at the top level, inside a fraction, and twice in a formula
  (check-both '(document
                 (concat "B+" "A" (rsub "i") "+C"
                         (frac "A" (concat "A" (rsub "i") "x")))
                 (concat "A" (rsub "j") "A" (rsub "i")))
              '(concat "A" (rsub "i"))
              '((0 1 0) (0 2 1) (0 4 1 0 0) (0 4 1 1 1) (1 2 0) (1 3 1)))
  (check-both '(document (concat "A" (rsub "j"))) '(concat "A" (rsub "k")) '())
  ;; a string of the pattern matches the end or the start of a string
  (check-both '(document (concat "x+A" (rsub "i") "+y"))
              '(concat "+A" (rsub "i") "+")
              '((0 0 1) (0 2 1)))
  ;; two tags in a row, in this order only
  (check-both '(document (concat "f" (rsub "n") (rsup "2") "+"
                                 "f" (rsup "2") (rsub "n")))
              '(concat (rsub "n") (rsup "2"))
              '((0 1 0) (0 2 1)))
  ;; A_ij contains A_i for the search widget only
  (let ((doc '(document (concat "A" (rsub "ij") "+" "A" (rsub "i"))))
        (what '(concat "A" (rsub "i"))))
    (with-search-mode 'search
      (lambda ()
        (check= (hits doc what) '((0 0 0) (0 1 1) (0 3 0) (0 4 1)))))
    (with-search-mode 'replace
      (lambda ()
        (check= (hits doc what) '((0 3 0) (0 4 1)))))))

(define (test-formula-wildcards)
  (check-group "formula wildcards")
  (let ((doc '(document (concat "A" (rsub "n") "+" "B" (rsub "n")
                                "+" "C" (rsub "m")))))
    ;; a leading wildcard extends the hit to the start of the formula
    (check-both doc '(concat (wildcard "x") (rsub "n")) '((0 0 0) (0 1 1)))
    (check-both doc '(concat (wildcard "x") (wildcard "y") (rsub "n"))
                '((0 0 0) (0 1 1)))
    ;; a trailing one to its end
    (check-both doc '(concat "A" (wildcard "x")) '((0 0 0) (0 7 1)))
    (check-both doc '(concat (rsub "n") (wildcard "y")) '((0 1 0) (0 7 1))))
  ;; also when only strings follow a string
  (check-both '(document (concat "A" "+" "B")) '(concat "A" (wildcard "y"))
              '((0 0 0) (0 2 1)))
  ;; in the middle: any content, one string or a tag
  (check-both '(document (concat "f" "(" "x" ")" "+"
                                 "f" "(" (frac "1" "2") ")"))
              '(concat "f" "(" (wildcard "x") ")")
              '((0 0 0) (0 3 1) (0 5 0) (0 8 1)))
  ;; the strings of a formula are its pieces: "f(" is not one
  (check-both '(document (concat "f" "(" "x" ")"))
              '(concat "f(" (wildcard "x") ")")
              '())
  ;; inside a tag of the pattern
  (check-both '(document (concat "A" (rsub "n") "=" (frac "1" "2")))
              '(concat (rsub (wildcard "x")) "=")
              '((0 1 0) (0 2 1)))
  ;; FIXME (#453): a trailing wildcard fails when the rest of the pattern
  ;; ends at a tag and no tag follows in the formula (tree_search.cpp:190,
  ;; the loop on j skips the sub-patterns which end with a wildcard, and
  ;; only a tag sets p2 to the end): (concat (rsub "n") (wildcard "y")) in
  ;; (concat "A" (rsub "n") "+" "B") or in (concat "A" (rsub "n")) gives (),
  ;; expected the formula from the subscript to its end.
  ;; FIXME (#452): a concat pattern ending with an empty string never returns
  ;; (tree_search.cpp:264, search_string finds "" at the same position
  ;; forever and appends hits until the memory is exhausted):
  ;; (concat (rsub "b") "") in (concat "a" (rsub "b") "c").
  ;; a wildcard alone matches whole formulas and strings
  (check-both '(document (concat "A" (rsub "n")) "text")
              '(concat (wildcard "x"))
              '((0 0 0) (0 1 1) (1 0) (1 4))))

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

;; a formula of n terms x_{k+1}+(ab)/(d^e)(y), nested concats
(define (long-formula n)
  (stree->tree
    `(document
       (concat ,@(append-map
                   (lambda (k)
                     (list "x" '(rsub (concat "k" "+" "1")) "+"
                           '(frac (concat "a" "b") (concat "d" (rsup "e")))
                           "(" "y" ")"))
                   (iota n))))))

(define (time-of thunk)
  (let* ((start (texmacs-time))
         (r (thunk)))
    (cons (- (texmacs-time) start) r)))

;; each keystroke of the search widget searches the document again: a long
;; formula takes milliseconds (it took seconds, quadratic in its length)
(define (test-speed)
  (check-group "speed")
  (let ((t (long-formula 400)))
    (for-each
      (lambda (mode)
        (with-search-mode mode
          (lambda ()
            (for-each
              (lambda (what)
                (let ((r (time-of
                           (lambda () (tree-search-tree t what '() 100)))))
                  (check= (cdr r) '())
                  (check-true (< (car r) 500))))
              ;; not a wildcard in the middle which fails late: from each
              ;; start, it still scans to the end of the formula (98 ms)
              (list '(concat "A" (rsub "ij"))
                    '(concat "x" (rsub "k") "-")
                    '(concat (wildcard "x") "q" (rsub "k")))))))
      '(search replace))
    ;; with hits all along the formula
    (with-search-mode 'search
      (lambda ()
        (let ((r (time-of (lambda ()
                            (tree-search-tree t '(concat "x" (rsub "k"))
                                              '() 10000)))))
          (check= (length (cdr r)) 800)
          (check-true (< (car r) 500)))))))

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
