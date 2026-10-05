;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : version-test.scm
;; DESCRIPTION : tests of document comparison and version control
;; COPYRIGHT   : (C) 2026  Massimiliano Gubinelli
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; The suite checks the tools of TeXmacs/progs/version:
;;
;;   - compare-versions (version-compare.scm) merges two trees into one,
;;     where the differences are marked with version-both (old and new
;;     shown together); a part which is only in one of the versions is
;;     paired with version-suppressed; strings are compared word by word,
;;     documents paragraph by paragraph, and other tags child by child
;;     when their arity and their inaccessible children agree;
;;   - the grain of the comparison ("detailed", "block", "rough") is a
;;     preference which version-set-grain saves: the suite sets the
;;     variable of the module directly, and puts it back, so that no
;;     preference is written;
;;   - in a buffer, the differences are visited (version-first-difference
;;     and the others), shown in one of three ways (version-show changes
;;     version-both into version-old or version-new and back), and resolved
;;     (version-retain keeps one of the versions, per difference, for a
;;     selection or for the whole buffer);
;;   - compare-with-older and compare-with-newer compare the buffer with a
;;     file;
;;   - version-tmfs.scm finds the version control tool of a file (svn or
;;     git, by a .svn or .git directory above it) and dispatches to
;;     version-svn.scm or version-git.scm. The git part is checked on a
;;     throwaway repository in the temporary directory, always through
;;     "git -c user.name=... -c user.email=...", never with a global
;;     configuration; svn is only checked on files which are not under
;;     version control.

(texmacs-module (check version-test)
  (:use (check check-lib)
        (version version-compare)
        (version version-tmfs)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Helpers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; the temporary files go to version-test in the temporary directory
(define version-dir
  (string-append (url->system (url-temp-dir)) "/version-test"))

(define (tmp-name name)
  (string-append version-dir "/" name))

(define (tmp-file name)
  (system->url (tmp-name name)))

(define (shell . l)
  (eval-system (apply string-append l)))

(define (reset-dir)
  (shell "rm -rf '" version-dir "'")
  (system-mkdir (system->url version-dir)))

(define (remove-dir)
  (shell "rm -rf '" version-dir "'"))

(define (tm-file body)
  ;; a .tm file with @body (lines of the body, already indented)
  (string-append "<TeXmacs|2.1>\n\n<style|generic>\n\n<\\body>\n"
                 body "</body>\n"))

(define (body) (tree->stree (buffer-get-body (current-buffer))))

(define (at . l)
  ;; the absolute path of @l in the current buffer
  (append (buffer-path) l))

(define (rel p)
  ;; the path @p relative to the current buffer
  (list-tail p (length (buffer-path))))

(define (cursor) (rel (cursor-path)))

(define (innermost-version)
  ;; the version tag around the cursor, or #f
  (and (inside-version?)
       (tree->stree (tree-innermost version-context?))))

(define (with-buffer-body doc thunk)
  ;; run @thunk in a new buffer holding @doc, then close the buffer
  (let* ((old (current-buffer))
         (u (new-buffer)))
    ;; new-buffer shows the buffer in the current window
    (switch-to-buffer* u)
    (buffer-set-body u (stree->tree doc))
    (go-start)
    (update-forced)
    (with r (check-run thunk)
      (when (and (pair? r) (== (car r) 'error))
        (check-report #f "the group" (object->string r)))
      (buffer-close u)
      (when (buffer-exists? old) (switch-to-buffer old)))))

(define (set-body doc)
  (buffer-set-body (current-buffer) (stree->tree doc))
  (go-start)
  (update-forced))

(define (set-grain g)
  ;; version-set-grain would save a preference
  (eval `(set! version-grain ,g) (resolve-module '(version version-compare))))

(define (with-grain g thunk)
  (set-grain g)
  (with r (check-run thunk)
    (set-grain "detailed")
    (when (and (pair? r) (== (car r) 'error))
      (check-report #f "the group" (object->string r)))))

(define (run-group thunk)
  ;; an error in a group counts as one failure
  (with r (check-run thunk)
    (when (and (pair? r) (== (car r) 'error))
      (check-report #f "the group" (object->string r)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Version markup
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; The three version tags form the group version-tag (version-drd.scm),
;; which is a variant group; std-fold.ts renders version-old as the old
;; version only, version-new as the new one, and version-both as both.
(define (test-markup)
  (check-group "markup")
  (check= (version-tag-list) '(version-old version-both version-new))
  (check-true (version-tag? 'version-old))
  (check-true (version-tag? 'version-both))
  (check-true (version-tag? 'version-new))
  (check-false (version-tag? 'version-suppressed))
  (check-false (version-tag? 'concat))
  (check-true (version-context? (stree->tree '(version-new "a" "b"))))
  (check-true (version-context? (stree->tree '(version-both "a" "b"))))
  (check-false (version-context? (stree->tree '(concat "a" "b"))))
  (check-false (version-context? (stree->tree "a")))
  (with-buffer-body
   '(document (concat (version-old "i" "wwwwwwww"))
              (concat (version-new "i" "wwwwwwww"))
              (concat (version-both "i" "wwwwwwww"))
              (concat "i")
              (concat "wwwwwwww"))
   (lambda ()
     (define (width i)
       (with r (tree-bounding-rectangle (path->tree (at i 0)))
         (- (caddr r) (car r))))
     (check-false (inside-version?))
     ;; the old version alone, the new one alone, and both
     (check= (width 0) (width 3))
     (check= (width 1) (width 4))
     (check= (width 2) (+ (width 3) (width 4)))
     (check-true (< (width 0) (width 1)))
     (check= (tree->stree (get-env-tree-at "old-version-color" (at 0 0 0 0)))
             "dark red")
     (check= (tree->stree (get-env-tree-at "new-version-color" (at 0 0 0 0)))
             "dark green")
     (go-to (at 0 0 0 0))
     (check-true (inside-version?))
     (check= (innermost-version) '(version-old "i" "wwwwwwww")))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Comparing strings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Strings are compared word by word; the spaces stay with the words before
;; them, and the unchanged parts are joined again.
(define (test-compare-strings)
  (check-group "compare strings")
  (check= (compare-versions "abc" "abc") "abc")
  (check= (compare-versions "" "") "")
  (check= (compare-versions '(document "a") '(document "a"))
          '(document "a"))
  (check= (compare-versions "a b c" "a x c")
          '(concat "a " (version-both "b " "x ") "c"))
  (check= (compare-versions "hello world" "hello there world")
          '(concat "hello " (version-both (version-suppressed) "there ")
                   "world"))
  (check= (compare-versions "hello there world" "hello world")
          '(concat "hello " (version-both "there " (version-suppressed))
                   "world"))
  (check= (compare-versions "one two" "one two three")
          '(concat "one two" (version-both (version-suppressed)
                                           " three")))
  (check= (compare-versions "a b c d e" "a B c D e")
          '(concat "a " (version-both "b " "B ") "c "
                   (version-both "d " "D ") "e"))
  ;; nothing in common
  (check= (compare-versions "abc" "xyz") '(version-both "abc" "xyz"))
  ;; a string and a concat
  (check= (compare-versions "x" '(concat "x" (em "y")))
          '(concat "x" (version-both (version-suppressed) (em "y"))))
  ;; the space after a tag goes with it
  (check= (compare-versions '(concat "a " (em "b") " c")
                            '(concat "a " (strong "b") " c"))
          '(concat "a " (version-both (concat (em "b") " ")
                                      (concat (strong "b") " "))
                   "c"))
  ;; the same tag is compared inside
  (check= (compare-versions '(concat "a " (em "b c") " d")
                            '(concat "a " (em "b x") " d"))
          '(concat "a " (em (concat "b " (version-both "c" "x"))) " d")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Comparing documents
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Paragraphs are compared one by one; a difference which spans paragraphs
;; holds documents, and a paragraph which is only in one version is paired
;; with (document (version-suppressed)).
(define (test-compare-documents)
  (check-group "compare documents")
  (check= (compare-versions '(document "a" "b" "c") '(document "a" "x" "c"))
          '(document "a" (version-both (document "b") (document "x")) "c"))
  (check= (compare-versions '(document "a" "b") '(document "a" "b" "c"))
          '(document "a" "b" (version-both (document (version-suppressed))
                                           (document "c"))))
  (check= (compare-versions '(document "a" "b" "c") '(document "a" "c"))
          '(document "a" (version-both (document "b")
                                       (document (version-suppressed)))
                     "c"))
  (check= (compare-versions '(document "b" "c") '(document "a" "b" "c"))
          '(document (version-both (document (version-suppressed))
                                   (document "a"))
                     "b" "c"))
  ;; a changed word in a paragraph
  (check= (compare-versions '(document "a" "b c d" "e")
                            '(document "a" "b x d" "e"))
          '(document "a" (concat "b " (version-both "c " "x ") "d") "e"))
  (check= (compare-versions '(document "Hello world." "Second.")
                            '(document "Hello big world." "Second."))
          '(document (concat "Hello " (version-both (version-suppressed)
                                                    "big ")
                             "world.")
                     "Second."))
  ;; a one-paragraph document against several paragraphs
  (check= (compare-versions '(document "x") '(document "y" "z"))
          '(document (version-both (document "x") (document "y" "z"))))
  ;; a string against a document: the string becomes a document
  (check= (compare-versions "x" '(document "x" "y"))
          '(version-both (document "x") (document "x" "y"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Comparing structured documents
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Tags with the same label and arity are compared child by child, unless
;; an inaccessible child differs (as the variable of a with); different
;; labels or arities, graphics and tables of another shape are replaced as
;; a whole.
(define (test-compare-structure)
  (check-group "compare structure")
  ;; sections
  (check= (compare-versions '(section "Intro") '(section "Introduction"))
          '(section (version-both "Intro" "Introduction")))
  (check= (compare-versions '(document (section "Intro") "text")
                            '(document (section "Intro") "more text"))
          '(document (section "Intro")
                     (concat (version-both (version-suppressed) "more ")
                             "text")))
  (check= (compare-versions '(document (section "A") "x" (section "B") "y")
                            '(document (section "A") "x" (section "C") "y"))
          '(document (section "A") "x"
                     (section (version-both "B" "C")) "y"))
  (check= (compare-versions '(document (section "A") "x")
                            '(document (subsection "A") "x"))
          '(document (version-both (document (section "A"))
                                   (document (subsection "A")))
                     "x"))
  ;; environments
  (check= (compare-versions '(theorem (document "a b"))
                            '(theorem (document "a c")))
          '(theorem (document (concat "a " (version-both "b" "c")))))
  ;; mathematics
  (check= (compare-versions '(math (concat "x" "+" "y"))
                            '(math (concat "x" "-" "y")))
          '(math (concat "x" (version-both "+" "-") "y")))
  (check= (compare-versions '(frac "1" "2") '(frac "1" "3"))
          '(frac "1" (version-both "2" "3")))
  (check= (compare-versions '(frac "1" "2") '(sqrt "2"))
          '(version-both (frac "1" "2") (sqrt "2")))
  (check= (compare-versions '(sqrt "2") '(sqrt "2" "3"))
          '(version-both (sqrt "2") (sqrt "2" "3")))
  (check= (compare-versions '(math (concat "a" (rsup "2")))
                            '(math (concat "a" (rsup "3"))))
          '(math (concat "a" (rsup (version-both "2" "3")))))
  ;; with: the variable and its value are not accessible
  (check= (compare-versions '(with "color" "red" "x")
                            '(with "color" "blue" "x"))
          '(version-both (with "color" "red" "x") (with "color" "blue" "x")))
  (check= (compare-versions '(with "color" "red" "x")
                            '(with "color" "red" "y"))
          '(with "color" "red" (version-both "x" "y")))
  ;; tables of the same shape are compared cell by cell
  (check= (compare-versions
           '(tformat (table (row (cell "a") (cell "b"))))
           '(tformat (table (row (cell "a") (cell "c")))))
          '(tformat (table (row (cell "a") (cell (version-both "b" "c"))))))
  (check= (compare-versions
           '(tformat (table (row (cell "a") (cell "b"))))
           '(tformat (table (row (cell "a") (cell "b"))
                            (row (cell "c") (cell "d")))))
          '(version-both
            (tformat (table (row (cell "a") (cell "b"))))
            (tformat (table (row (cell "a") (cell "b"))
                            (row (cell "c") (cell "d"))))))
  (check= (compare-versions
           '(tformat (cwith "1" "1" "1" "1" "cell-halign" "r")
                     (table (row (cell "a"))))
           '(tformat (table (row (cell "a")))))
          '(version-both
            (tformat (cwith "1" "1" "1" "1" "cell-halign" "r")
                     (table (row (cell "a"))))
            (tformat (table (row (cell "a"))))))
  ;; graphics are replaced as a whole
  (check= (compare-versions '(graphics "" (point "0" "0"))
                            '(graphics "" (point "1" "0")))
          '(version-both (graphics "" (point "0" "0"))
                         (graphics "" (point "1" "0"))))
  ;; the preamble is compared separately; the variable of an assign is
  ;; not accessible
  (check= (compare-versions
           '(document (hide-preamble (document (assign "a" "1"))) "x")
           '(document (hide-preamble (document (assign "a" "2"))) "x"))
          '(document (hide-preamble
                      (document (version-both (assign "a" "1")
                                              (assign "a" "2"))))
                     "x"))
  (check= (compare-versions
           '(document "x")
           '(document (hide-preamble (document (assign "a" "2"))) "x"))
          '(document (hide-preamble
                      (document (version-both (document "")
                                              (document (assign "a" "2")))))
                     "x")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Grain of the comparison
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; "rough" replaces the whole tree, "block" compares paragraphs but not
;; their contents, "detailed" is the default.
(define (test-grain)
  (check-group "grain")
  (check-true (version-test-grain? "detailed"))
  (check-false (version-test-grain? "rough"))
  (with-grain "rough"
    (lambda ()
      (check-true (version-test-grain? "rough"))
      (check= (compare-versions "a b c" "a x c")
              '(version-both "a b c" "a x c"))
      (check= (compare-versions '(document "a" "b") '(document "a" "c"))
              '(version-both (document "a" "b") (document "a" "c")))
      (check= (compare-versions "same" "same") "same")))
  (with-grain "block"
    (lambda ()
      (check-true (version-test-grain? "block"))
      (check= (compare-versions "a b c" "a x c")
              '(version-both (document "a b c") (document "a x c")))
      (check= (compare-versions '(document "a" "b") '(document "a" "c"))
              '(document "a" (version-both (document "b") (document "c"))))
      (check= (compare-versions '(document "a" (em "b"))
                                '(document "a" (em "c")))
              '(document "a" (version-both (document (em "b"))
                                           (document (em "c")))))
      ;; the paragraphs of a difference are whole, not split into words
      (check= (compare-versions '(document "b c") '(document "b x"))
              '(document (version-both (document "b c") (document "b x"))))
      (check= (compare-versions '(document "a" "b c d") '(document "a" "b x d"))
              '(document "a" (version-both (document "b c d")
                                           (document "b x d"))))))
  (check-true (version-test-grain? "detailed"))
  (check= (compare-versions '(document "a" "b c") '(document "a" "b x"))
          '(document "a" (concat "b " (version-both "c" "x")))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Navigation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; The cursor visits the arguments of the differences in order: the old
;; version, then the new one, then the next difference.
(define (test-navigation)
  (check-group "navigation")
  (with-buffer-body
   (compare-versions '(document "a" "b c d" "e" "end")
                     '(document "a" "b x d" "f" "end"))
   (lambda ()
     (check= (body)
             '(document "a" (concat "b " (version-both "c " "x ") "d")
                        (version-both (document "e") (document "f"))
                        "end"))
     (check-false (inside-version?))
     (version-first-difference)
     (check= (cursor) '(1 1 0 0))
     (check= (innermost-version) '(version-both "c " "x "))
     (version-next-difference)
     (check= (cursor) '(1 1 1 0))
     (check= (innermost-version) '(version-both "c " "x "))
     (version-next-difference)
     (check= (cursor) '(2 0 0 0))
     (check= (innermost-version)
             '(version-both (document "e") (document "f")))
     (version-next-difference)
     (check= (cursor) '(2 1 0 0))
     ;; there is no further difference
     (version-next-difference)
     (check= (cursor) '(2 1 0 0))
     (version-previous-difference)
     (check= (cursor) '(2 0 0 1))
     (version-previous-difference)
     (check= (cursor) '(1 1 1 2))
     (check= (innermost-version) '(version-both "c " "x "))
     (version-previous-difference)
     (check= (cursor) '(1 1 0 2))
     (version-last-difference)
     (check= (cursor) '(2 1 0 1))
     (check= (innermost-version)
             '(version-both (document "e") (document "f")))
     (version-first-difference)
     (check= (cursor) '(1 1 0 0))))
  ;; without differences the cursor goes to the start or the end
  (with-buffer-body '(document "plain" "text")
    (lambda ()
      (version-first-difference)
      (check= (cursor) '(0 0))
      (check-false (inside-version?))
      (version-last-difference)
      (check= (cursor) '(1 4))
      (check-false (inside-version?))))
  ;; a difference at the very start or end of the document is not skipped
  (with-buffer-body '(document (version-both "p" "q") "x")
    (lambda ()
      (version-first-difference)
      (check= (cursor) '(0 0 0))
      (check= (innermost-version) '(version-both "p" "q"))
      (version-last-difference)
      (check= (cursor) '(0 1 1))))
  (with-buffer-body '(document "x" (version-both "p" "q"))
    (lambda ()
      (version-last-difference)
      (check= (cursor) '(1 1 1))
      (check= (innermost-version) '(version-both "p" "q"))
      (version-first-difference)
      (check= (cursor) '(1 0 0))))
  (with-buffer-body '(document (version-both "a" "b") "x"
                               (version-both (document "p") (document "q")))
    (lambda ()
      (version-last-difference)
      (check= (cursor) '(2 1 0 1))
      (version-first-difference)
      (check= (cursor) '(0 0 0)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Showing the versions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; version-show changes the difference at the cursor, or those in the
;; selection; version-show-paragraph those of the paragraph and
;; version-show-all those of the buffer. The arguments do not change.
(define (test-show)
  (check-group "show")
  (with-buffer-body
   '(document (concat "a " (version-both "b" "c") " d " (version-both "e" "f"))
              (concat "g " (version-both "h" "i"))
              (version-both (document "j") (document "k")))
   (lambda ()
     (go-to (at 0 1 0 0))
     (version-show 'version-old)
     (check= (body)
             '(document
               (concat "a " (version-old "b" "c") " d " (version-both "e" "f"))
               (concat "g " (version-both "h" "i"))
               (version-both (document "j") (document "k"))))
     (version-show 'version-new)
     (check= (tree->stree (tree-ref (buffer-tree) 0 1)) '(version-new "b" "c"))
     (version-show 'version-both)
     (check= (tree->stree (tree-ref (buffer-tree) 0 1)) '(version-both "b" "c"))
     ;; outside a difference, nothing changes
     (go-to (at 0 0 0))
     (version-show 'version-old)
     (check= (tree->stree (tree-ref (buffer-tree) 0 1)) '(version-both "b" "c"))
     ;; the paragraph
     (go-to (at 0 1 0 0))
     (version-show-paragraph 'version-new)
     (check= (body)
             '(document
               (concat "a " (version-new "b" "c") " d " (version-new "e" "f"))
               (concat "g " (version-both "h" "i"))
               (version-both (document "j") (document "k"))))
     ;; the whole buffer
     (version-show-all 'version-old)
     (check= (body)
             '(document
               (concat "a " (version-old "b" "c") " d " (version-old "e" "f"))
               (concat "g " (version-old "h" "i"))
               (version-old (document "j") (document "k"))))
     (version-show-all 'version-both)
     (check= (body)
             '(document
               (concat "a " (version-both "b" "c") " d " (version-both "e" "f"))
               (concat "g " (version-both "h" "i"))
               (version-both (document "j") (document "k"))))
     ;; a selection
     (selection-set (at 0 0 0) (at 0 3 1))
     (check-true (selection-active-any?))
     (check= (map tree->stree (selection-trees))
             '((concat "a " (version-both "b" "c") " d "
                       (version-both "e" "f"))))
     (version-show 'version-new)
     (check= (body)
             '(document
               (concat "a " (version-new "b" "c") " d " (version-new "e" "f"))
               (concat "g " (version-both "h" "i"))
               (version-both (document "j") (document "k"))))
     (selection-cancel))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Retaining a version
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; version-retain keeps the old (0) or new (1) version of the difference at
;; the cursor, or the shown one ('current: the new one of version-both),
;; and moves to the next difference; a version-suppressed part disappears.
;; The concat around is flattened but its strings are not joined.
(define (test-retain)
  (check-group "retain")
  (with-buffer-body
   (compare-versions '(document "a" "b c d" "e") '(document "a" "b x d" "f"))
   (lambda ()
     (version-first-difference)
     (version-retain 1)
     (check= (body)
             '(document "a" (concat "b " "x " "d")
                        (version-both (document "e") (document "f"))))
     ;; the cursor went to the next difference
     (check= (cursor) '(2 0 0 0))
     (check-true (inside-version?))
     (version-retain 0)
     (check= (body) '(document "a" (concat "b " "x " "d") "e"))
     (check-false (inside-version?))))
  (with-buffer-body '(document (concat "a " (version-both "b" "c") " d"))
    (lambda ()
      (go-to (at 0 1 0 0))
      (version-retain 0)
      (check= (body) '(document (concat "a " "b" " d")))))
  (with-buffer-body '(document (concat "a " (version-both "b" "c") " d"))
    (lambda ()
      (go-to (at 0 1 0 0))
      (version-retain 'current)
      (check= (body) '(document (concat "a " "c" " d")))))
  (with-buffer-body '(document (concat "a " (version-new "b" "c") " d"))
    (lambda ()
      (go-to (at 0 1 1 0))
      (version-retain 'current)
      (check= (body) '(document (concat "a " "c" " d")))))
  (with-buffer-body '(document (concat "a " (version-old "b" "c") " d"))
    (lambda ()
      (go-to (at 0 1 0 0))
      (version-retain 'current)
      (check= (body) '(document (concat "a " "b" " d")))))
  ;; suppressed parts
  (with-buffer-body
   '(document "x" (version-both (document "p") (document (version-suppressed)))
              "z")
   (lambda ()
     (go-to (at 1 0 0 0))
     (version-retain 1)
     (check= (body) '(document "x" "z"))
     (check= (cursor) '(1 0))))
  (with-buffer-body
   '(document "x" (version-both (document (version-suppressed)) (document "p"))
              "z")
   (lambda ()
     (go-to (at 1 1 0 0))
     (version-retain 0)
     (check= (body) '(document "x" "z"))))
  (with-buffer-body
   '(document "x" (version-both (document (version-suppressed)) (document "p"))
              "z")
   (lambda ()
     (go-to (at 1 1 0 0))
     (version-retain 1)
     (check= (body) '(document "x" "p" "z"))))
  (with-buffer-body
   '(document (concat "x" (version-both (version-suppressed) "p") "z"))
   (lambda ()
     (go-to (at 0 1 1 0))
     (version-retain 0)
     (check= (body) '(document (concat "x" "z")))))
  (with-buffer-body
   '(document (concat "x" (version-both "p" (version-suppressed)) "z"))
   (lambda ()
     (go-to (at 0 1 0 0))
     (version-retain 0)
     (check= (body) '(document (concat "x" "p" "z")))))
  ;; a selection
  (with-buffer-body
   '(document (concat "a" (version-both "b" "c") "d" (version-both "e" "f") "g")
              (concat "h" (version-both "i" "j")))
   (lambda ()
     (selection-set (at 0 0 0) (at 0 4 1))
     (version-retain 1)
     (check= (body)
             '(document (concat "a" "c" "d" "f" "g")
                        (concat "h" (version-both "i" "j"))))
     (selection-cancel))))

;; version-retain-all resolves all the differences of the buffer: 0 and 1
;; keep the old and new versions, 'current the version shown, 'current-old
;; the old version of version-both and the shown version otherwise.
(define (test-retain-all)
  (check-group "retain all")
  (let ((doc (compare-versions '(document "a" "b c d" "e" "g")
                               '(document "a" "b x d" "f"))))
    (with-buffer-body doc
      (lambda ()
        (version-retain-all 0)
        (check= (body) '(document "a" (concat "b " "c " "d") "e" "g"))
        (set-body doc)
        (version-retain-all 1)
        (check= (body) '(document "a" (concat "b " "x " "d") "f"))
        (set-body doc)
        (version-retain-all 'current)
        (check= (body) '(document "a" (concat "b " "x " "d") "f"))
        (set-body doc)
        (version-show-all 'version-old)
        (version-retain-all 'current)
        (check= (body) '(document "a" (concat "b " "c " "d") "e" "g"))
        (set-body doc)
        (version-retain-all 'current-old)
        (check= (body) '(document "a" (concat "b " "c " "d") "e" "g"))
        (set-body doc)
        (go-to (at 1 1 0 0))
        (version-show 'version-new)
        (version-retain-all 'current-old)
        (check= (body) '(document "a" (concat "b " "x " "d") "e" "g"))
        (check-false (inside-version?)))))
  (with-buffer-body
   (compare-versions '(document "a" "b" "c") '(document "a" "c"))
   (lambda ()
     (check= (body)
             '(document "a" (version-both (document "b")
                                          (document (version-suppressed)))
                        "c"))
     (version-retain-all 1)
     (check= (body) '(document "a" "c")))))

;; reactualize-differences compares again the two versions of the
;; difference at the cursor, with the current grain.
(define (test-reactualize)
  (check-group "reactualize")
  (with-buffer-body '(document (concat "a " (version-both "big dog" "big cat")
                                       " d"))
    (lambda ()
      (go-to (at 0 1 0 0))
      (reactualize-differences)
      (check= (body)
              '(document (concat "a big " (version-both "dog" "cat") " d")))))
  (with-buffer-body '(document (concat "a " (version-both "b" "c") " d"))
    (lambda ()
      (go-to (at 0 1 0 0))
      (reactualize-differences)
      (check= (body) '(document (concat "a " (version-both "b" "c") " d")))))
  (with-buffer-body '(document (concat "a " (version-both "x" "x") " d"))
    (lambda ()
      (go-to (at 0 1 0 0))
      ;; the versions are the same: the text is inserted instead
      (reactualize-differences)
      (check= (body) '(document "a x d")))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Comparing with files
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; compare-with-older puts the differences between a file (the old version)
;; and the buffer (the new one) into the buffer and goes to the first one;
;; compare-with-newer takes the file as the new version.
(define (test-compare-files)
  (check-group "compare files")
  (let ((f (tmp-file "older.tm")))
    (string-save (tm-file "  Hello world.\n\n  Second.\n\n  Third.\n")
                 (url->system f))
    (with-buffer-body '(document "Hello big world." "Second.")
      (lambda ()
        (compare-with-older f)
        (check= (body)
                '(document (concat "Hello "
                                   (version-both (version-suppressed) "big ")
                                   "world.")
                           "Second."
                           (version-both (document "Third.")
                                         (document (version-suppressed)))))
        (check= (cursor) '(0 1 0 0))
        (check= (innermost-version)
                '(version-both (version-suppressed) "big "))
        (version-retain-all 0)
        (check= (body)
                '(document (concat "Hello " "world.") "Second." "Third."))))
    (with-buffer-body '(document "Hello big world." "Second.")
      (lambda ()
        (compare-with-newer f)
        (check= (body)
                '(document (concat "Hello "
                                   (version-both "big " (version-suppressed))
                                   "world.")
                           "Second."
                           (version-both (document (version-suppressed))
                                         (document "Third."))))
        (check= (cursor) '(0 1 0 0))
        (version-retain-all 1)
        (check= (body)
                '(document (concat "Hello " "world.") "Second." "Third."))))
    ;; the same document: nothing to show
    (with-buffer-body '(document "Hello world." "Second." "Third.")
      (lambda ()
        (compare-with-older f)
        (check= (body) '(document "Hello world." "Second." "Third."))
        (check-false (inside-version?))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Files without version control
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; A path at the root of the file system is under no repository, whatever
;; the temporary directory (which may be inside one).
(define (test-unversioned)
  (check-group "unversioned")
  (let ((f (system->url "/no-such-dir-version-test/file.tm")))
    (check-false (svn-active? f))
    (check-false (git-active? f))
    (check-false (version-tool f))
    (check-false (versioned? f))
    (check= (version-status f) "unknown")
    (check-false (version-history f))
    (check-false (version-history* f))
    (check-false (version-supports-history? f))
    (check-false (version-supports-svn-style? f))
    (check-false (version-supports-git-style? f))
    (check= (version-update f) "file is not under version control")
    (check= (version-register f) "file is not under version control")
    (check= (version-unregister f) "file is not under version control")
    (check= (version-commit f "msg") "file is not under version control")
    (check= (version-revision f "1") "")
    (check= (version-beautify-revision f "12345678") "12345678")
    (check-false (version-revision? f))
    (check= (version-get-revision* f) "")))

;; The revisions of a file are tmfs urls tmfs://revision/<rev>/<file>.
(define (test-revision-urls)
  (check-group "revision urls")
  (let* ((f (system->url "/no-such-dir-version-test/file.tm"))
         (s (version-revision-url f "abc"))
         (r (string->url s)))
    (check= s "tmfs://revision/abc/file/no-such-dir-version-test/file.tm")
    ;; a revision with a colon names the file itself
    (check= (version-revision-url f "abc:file/x/y.tm")
            "tmfs://revision/abc/file/x/y.tm")
    (check-true (version-revision? r))
    (check= (version-get-revision r) "abc")
    (check= (version-get-revision* r) "abc")
    (check= (version-head r) f)
    (check= (version-tool* r) #f)
    (check= (tmfs-title s "")
            "file.tm - Revision abc")
    (check-false (version-revision? f))
    (check-false (version-get-revision f))
    (check-false (version-head f))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Git
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define git-dir (tmp-name "repo"))

(define (git-sh . l)
  ;; a git command in the throwaway repository, never configured globally
  (apply shell "cd '" git-dir "' && git -c user.name=tester"
         " -c user.email=tester@example.org " l))

(define (git-available?)
  (string-starts? (shell "git --version 2>/dev/null") "git version"))

(define (test-git)
  (check-group "git")
  (if (not (git-available?))
      (display* "  git is not available, the group is skipped\n")
      (run-group test-git-sub)))

(define (test-git-sub)
  (system-mkdir (system->url git-dir))
  (git-sh "init -q . && git -c user.name=tester"
          " -c user.email=tester@example.org commit -q --allow-empty -m init")
  (let* ((root (system->url git-dir))
         (f (system->url (string-append git-dir "/a.tm")))
         (g (system->url (string-append git-dir "/b.tm")))
         (doc1 (tm-file "  One.\n"))
         (doc2 (tm-file "  One more.\n")))
    (string-save doc1 (url->system f))
    ;; detection of the tool
    (check-true (git-active? f))
    (check-false (svn-active? f))
    (check= (version-tool f) "git")
    (check-true (versioned? f))
    (check-true (version-supports-git-style? f))
    (check-false (version-supports-svn-style? f))
    (check-true (version-supports-history? f))
    (check= (git-root f) git-dir)
    (check= (git-root root) git-dir)
    (check= (git-command f)
            (string-append "git --work-tree=" git-dir
                           " --git-dir=" git-dir "/.git"))
    ;; an untracked file
    (check= (buffer-status f) "??")
    (check= (version-status f) "unknown")
    (check-true (buffer-to-add? f))
    (check-false (buffer-to-unadd? f))
    (check-false (buffer-histed? f))
    (check-false (buffer-has-diff? f))
    (check= (git-status root) '(("??" "a.tm")))
    ;; registered
    (version-register f)
    (check= (buffer-status f) "A ")
    (check= (version-status f) "modified")
    (check-true (buffer-to-unadd? f))
    (check-false (buffer-to-add? f))
    (check= (git-status root) '(("A " "a.tm")))
    (version-unregister f)
    (check= (buffer-status f) "??")
    (version-register f)
    ;; committed
    (git-sh "commit -q -m 'first commit'")
    (check= (buffer-status f) "  ")
    (check= (version-status f) "unmodified")
    (check-true (buffer-histed? f))
    (check-false (buffer-has-diff? f))
    (check= (git-status root) '())
    (let* ((h1 (git-master f))
           (h0 (string-drop-right (git-sh "rev-parse HEAD~1") 1)))
      (check= (string-length h1) 40)
      (check= h1 (string-drop-right (git-sh "rev-parse HEAD") 1))
      (check= (version-beautify-revision f h1) (string-take h1 7))
      (check= (version-revision f h1) doc1)
      (check= (git-commit-parents root h1) (list h0))
      (check= (git-commit-parent root h1) h0)
      (with m (git-commit-message root h1)
        (check= (car m) (string-append "commit " h1))
        (check= (cadr m) "Author: tester <tester@example.org>")
        (check-true (in? "    first commit" m)))
      (check= (git-commit-diff root h0 h1)
              `((7 0 (hlink "a.tm" ,(version-revision-url f h1)) 4)))
      ;; the log, newest first
      (with l (git-log root)
        (check= (length l) 2)
        (check= (map cadr l) '("tester" "tester"))
        (check= (map caddr l) '("first commit" "init"))
        (check= (cadddr (car l))
                `(hlink ,(string-take h1 7) ,(tmfs-url-commit root h1))))
      (check= (tmfs-url-commit root h1)
              (string-append "tmfs://commit/" h1 "/"
                             (url->tmfs-string root)))
      ;; the revision as a tmfs file
      (check= (tmfs-load (version-revision-url f h1)) doc1)
      ;; modified in the working tree
      (string-save doc2 (url->system f))
      (check= (buffer-status f) " M")
      (check= (version-status f) "modified")
      (check-true (buffer-has-diff? f))
      (check-true (buffer-to-add? f))
      (check= (git-status root)
              `((" M" (hlink "a.tm" ,(url->string (url-append root "a.tm"))))))
      (check= (version-revision f h1) doc1)
      ;; a second commit
      (git-sh "commit -q -a -m 'second commit'")
      (let ((h2 (git-master f)))
        (check-false (== h2 h1))
        (check= (git-commit-parent root h2) h1)
        (check= (version-revision f h2) doc2)
        (check= (version-revision f h1) doc1)
        (check= (git-commit-diff root h1 h2)
                `((1 1 (hlink "a.tm" ,(version-revision-url f h2)) 4)))
        ;; the history of a file, newest first, with the current buffer
        ;; on the file (the history is relative to its repository)
        (let ((old (current-buffer)))
          (load-buffer f)
          (switch-to-buffer* f)
          (with h (version-history f)
            (check= (length h) 2)
            (check= (map car h)
                    (list (string-append h2 ":" (url->tmfs-string f))
                          (string-append h1 ":" (url->tmfs-string f))))
            (check= (map cadr h) '("tester" "tester"))
            (check= (map cadddr h) '("second commit" "first commit")))
          (check= (current-git-root) git-dir)
          (check= (git-commit-master) h2)
          ;; the revision url of a history entry
          (check= (version-revision-url f (string-append h1 ":"
                                                         (url->tmfs-string f)))
                  (version-revision-url f h1))
          ;; comparing the buffer with its first revision
          (compare-with-older (string->url (version-revision-url f h1)))
          ;; "One." and "One more." have no word in common
          (check= (body)
                  '(document (version-both (document "One.")
                                           (document "One more."))))
          (version-retain-all 0)
          (check= (body) '(document "One."))
          (buffer-pretend-saved f)
          (buffer-close f)
          (when (buffer-exists? old) (switch-to-buffer old)))
        ;; the history is relative to the repository of the file, also
        ;; when the current buffer is outside it
        (check-false (== (current-git-root) git-dir))
        (check= (map car (version-history f))
                (list (string-append h2 ":" (url->tmfs-string f))
                      (string-append h1 ":" (url->tmfs-string f))))))
    ;; a file which is not in the repository yet
    (string-save doc1 (url->system g))
    (check= (version-status g) "unknown")
    (check= (git-status root) '(("??" "b.tm")))
    (with c (git-status-content root)
      (check= (car c) 'document)
      (check= (tm-ref c 2 0) '(document (tmfs-title "Git Status")
                                        (description-long
                                         (document
                                          (concat (item* "Changes to be committed")
                                                  "")
                                          (concat (item* "Changes not staged for commit")
                                                  "")
                                          (concat (item* "Untracked files")
                                                  (concat "b.tm" (new-line))))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Main
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (version-test-failures)
  (check-suite "version")
  (reset-dir)
  (run-group test-markup)
  (run-group test-compare-strings)
  (run-group test-compare-documents)
  (run-group test-compare-structure)
  (run-group test-grain)
  (run-group test-navigation)
  (run-group test-show)
  (run-group test-retain)
  (run-group test-retain-all)
  (run-group test-reactualize)
  (run-group test-compare-files)
  (run-group test-unversioned)
  (run-group test-revision-urls)
  (test-git)
  (set-grain "detailed")
  (remove-dir)
  (check-end))
