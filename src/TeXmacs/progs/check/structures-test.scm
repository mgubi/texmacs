;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : structures-test.scm
;; DESCRIPTION : tests of the basic data structures seen from Scheme
;; COPYRIGHT   : (C) 2026  Massimiliano Gubinelli
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; The basic structures of TeXmacs which the other suites leave out
;; (lists-test, base-test, trees-test and glue-test cover lists, the base
;; library, hash tables, trees and the glue itself):
;;
;;   - the string routines of the C++ side (Data/String/analyze.cpp and
;;     converter.cpp): searching, replacing, cases, quoting, escapes,
;;     hexadecimal numbers, conversions between encodings, and the strings
;;     in the TeXmacs encoding, whose characters are bytes or <name>;
;;   - urls as data (constructors and accessors, tmfs names and queries of
;;     kernel/texmacs/tm-file-system.scm) and as files in a temporary
;;     directory;
;;   - paths and the cursor moves on trees (Data/Tree/tree_traverse.cpp),
;;     positions and tree pointers which follow the edits of a buffer;
;;   - pattern matching (kernel/regexp/regexp-match.scm) and selection of
;;     subtrees (kernel/regexp/regexp-select.scm);
;;   - logic programming (kernel/logic): rules, queries, unification,
;;     tables, groups and dispatchers;
;;   - secure evaluation, states, list-sort, symbol properties and the
;;     preference declarations (read only, nothing is ever set).
;;
;; Strings are byte strings in the Cork encoding: a character above 127 is
;; made with integer->char.
;;
;; The suite leaves some declarations behind, all with names which start
;; with structures- and which nothing else uses: logic rules (the logic
;; database cannot forget rules) and a symbol property. The grammar rules of
;; the pattern matcher and the preference declarations are removed again.

(texmacs-module (check structures-test)
  (:use (check check-lib)
        (kernel regexp regexp-match)
        (kernel regexp regexp-select)
        (kernel logic logic-bind)
        (kernel logic logic-unify)
        (kernel logic logic-rules)
        (kernel logic logic-query)
        (kernel logic logic-data)
        (kernel texmacs tm-secure)
        (kernel texmacs tm-states)
        (kernel texmacs tm-preferences)
        (kernel texmacs tm-file-system)))

(define e-acute (integer->char 233))         ; Cork e acute
(define E-acute (integer->char 201))         ; Cork E acute
(define (cork . l)
  ;; a string from characters and character codes
  (list->string (map (lambda (x) (if (integer? x) (integer->char x) x)) l)))

(define (run-group name thunk)
  ;; an error in a group counts as one failure, and the suite goes on
  (check-group name)
  (with r (check-run thunk)
    (when (and (pair? r) (== (car r) 'error))
      (check-report #f "the group" (object->string r)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Searching and replacing in strings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; The positions are byte positions, -1 when nothing is found; the
;; backwards search starts at the position where the pattern would begin.
(define (test-string-search)
  (check= (string-occurs? "lo" "hello") #t)
  (check= (string-occurs? "ol" "hello") #f)
  (check= (string-occurs? "" "hello") #t)
  (check= (string-count-occurrences "a" "banana") 3)
  ;; occurrences may overlap
  (check= (string-count-occurrences "aa" "aaaa") 3)
  (check= (string-count-occurrences "x" "") 0)
  (check= (string-search-forwards "an" 0 "banana") 1)
  (check= (string-search-forwards "an" 2 "banana") 3)
  (check= (string-search-forwards "an" 4 "banana") -1)
  (check= (string-search-forwards "" 3 "banana") 3)
  (check= (string-search-forwards "b" 0 "") -1)
  (check= (string-search-backwards "an" 5 "banana") 3)
  (check= (string-search-backwards "an" 2 "banana") 1)
  (check= (string-search-backwards "an" 0 "banana") -1)
  (check= (string-search-backwards "an" 10 "banana") 3)
  ;; the longest suffix of the first string which starts the second
  (check= (string-overlapping "abcde" "cdefg") 3)
  (check= (string-overlapping "aaa" "aa") 2)
  (check= (string-overlapping "abc" "xyz") 0)
  (check= (string-overlapping "" "abc") 0)
  (check= (string-replace "banana" "an" "AN") "bANANa")
  (check= (string-replace "aaa" "aa" "b") "ba")
  (check= (string-replace "abc" "x" "y") "abc")
  (check= (string-replace "a.b.c" "." "") "abc")
  ;; FIXME: string-replace loops forever on an empty pattern
  ;; (replace in Data/String/analyze.cpp:1263 advances by N(what)= 0):
  ;; (string-replace "abc" "" "x") never returns, expected "abc" or an
  ;; error.
  (check= (string-find-non-alpha "abc def" 0 #t) 3)
  (check= (string-find-non-alpha "abc def" 5 #f) 3)
  (check= (string-find-non-alpha "abc" 0 #t) -1)
  (check= (string-find-non-alpha "abc" 3 #f) -1)
  (check= (cpp-string-tokenize "a::b::c" "::") '("a" "b" "c"))
  (check= (cpp-string-tokenize "::a" "::") '("" "a"))
  (check= (cpp-string-recompose '("a" "b") "--") "a--b")
  (check= (cpp-string-recompose '() "--") ""))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Character classes, cases and sets of characters
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; The cases are those of the Cork encoding: the accented letters change
;; case too (e acute 233 <-> E acute 201).
(define (test-string-case)
  (check= (string-alpha? "abc") #t)
  (check= (string-alpha? "aBc") #t)
  (check= (string-alpha? "ab1") #f)
  (check= (string-alpha? "") #f)
  (check= (string-locase-alpha? "abc") #t)
  (check= (string-locase-alpha? "aBc") #f)
  (check= (string-locase-alpha? "") #f)
  (check= (upcase-first "hello world") "Hello world")
  (check= (upcase-first "1abc") "1abc")
  (check= (upcase-first "") "")
  (check= (locase-first "Hello World") "hello World")
  (check= (locase-first "") "")
  (check= (upcase-all "Hello, World 1") "HELLO, WORLD 1")
  (check= (locase-all "Hello, World 1") "hello, world 1")
  (check= (upcase-all (cork #\c #\a #\f 233)) (cork #\C #\A #\F 201))
  (check= (locase-all (cork #\C #\A #\F 201)) (cork #\c #\a #\f 233))
  (check= (upcase-first (cork 233 #\t #\e)) (cork 201 #\t #\e))
  ;; strings as sets of characters
  (check= (string-union "abc" "cd") "abcd")
  (check= (string-union "" "ab") "ab")
  (check= (string-minus "abcabc" "b") "acac")
  (check= (string-minus "abc" "") "abc")
  (check= (string-minus "abc" "cba") ""))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Quoting, escapes and spaces
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (test-string-quoting)
  ;; string-quote writes a Scheme string literal, string-unquote reads it
  (check= (string-quote "abc") "\"abc\"")
  (check= (string-quote "") "\"\"")
  (check= (string-quote "a\"b\\c") "\"a\\\"b\\\\c\"")
  (check= (string-unquote "\"a\\\"b\\\\c\"") "a\"b\\c")
  (check= (string-unquote "\"\"") "")
  (check= (string-unquote "abc") "abc")
  (check= (string-unquote "\"abc") "\"abc")
  (for (s (list "" "x\"y" "a\\b" "\\\"" "plain"))
    (check= (string-unquote (string-quote s)) s))
  ;; escape-generic protects the bytes 2, 5 and 27 with an escape (27)
  (check= (escape-generic "abc") "abc")
  (check= (escape-generic (cork #\a 2 #\b)) (cork #\a 27 2 #\b))
  (check= (escape-generic (cork 27)) (cork 27 27))
  ;; escape-verbatim turns newlines and tabs to spaces, drops control bytes
  (check= (escape-verbatim (cork #\a #\newline #\b #\tab #\c 7)) "a b c")
  (check= (escape-verbatim "") "")
  ;; escape-shell protects the characters special to a POSIX shell
  (check= (escape-shell "abc") "abc")
  (check= (escape-shell "a b(c)$d") "a\\ b\\(c\\)\\$d")
  (check= (escape-shell "x\ny") "x\\ny")
  (check= (escape-shell "a&b<c>d?") "a\\&b\\<c\\>d\\?")
  ;; escape-to-ascii writes the bytes above 127 as \xhh, which
  ;; unescape-guile reads back
  (check= (escape-to-ascii "abc") "abc")
  (check= (escape-to-ascii (cork #\c #\a #\f 233)) "caf\\xe9")
  (check= (unescape-guile "caf\\xe9") (cork #\c #\a #\f 233))
  (check= (unescape-guile "a\\x41b") "aAb")
  (check= (unescape-guile "\\x4") "\\x4")
  (check= (unescape-guile "a\\b") "a\\b")
  (with s (cork #\a 200 #\b 255)
    (check= (unescape-guile (escape-to-ascii s)) s))
  ;; spaces
  (check= (string-trim-spaces-left "  a b  ") "a b  ")
  (check= (string-trim-spaces-right "  a b  ") "  a b")
  (check= (string-trim-spaces "  a b  ") "a b")
  (check= (string-trim-spaces "   ") "")
  (check= (string-trim-spaces "") "")
  (check= (xml-unspace "  a   b  " #t #t) "a b")
  (check= (xml-unspace "  a   b  " #f #f) " a b "))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Numbers and encodings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (test-string-encodings)
  ;; hexadecimal numbers: upper case digits, padded to a fixed number of
  ;; digits (the higher digits are dropped)
  (check= (integer->hexadecimal 0) "0")
  (check= (integer->hexadecimal 255) "FF")
  (check= (integer->hexadecimal -255) "-FF")
  (check= (integer->hexadecimal 4096) "1000")
  (check= (integer->padded-hexadecimal 10 4) "000A")
  (check= (integer->padded-hexadecimal 255 2) "FF")
  (check= (integer->padded-hexadecimal 4096 2) "00")
  (check= (hexadecimal->integer "ff") 255)
  (check= (hexadecimal->integer "FF") 255)
  (check= (hexadecimal->integer "-1A") -26)
  (check= (hexadecimal->integer "") 0)
  (for (n (list 0 1 15 16 4095 123456))
    (check= (hexadecimal->integer (integer->hexadecimal n)) n))
  ;; conversions
  (check= (string-convert "abc" "Cork" "UTF-8") "abc")
  (check= (string-convert (cork 233) "Cork" "UTF-8") (cork 195 169))
  (check= (string-convert (cork 195 169) "UTF-8" "Cork") (cork 233))
  (check= (utf8->html "a<b") "a&lt;b")
  (check= (html->utf8 "a&lt;b") "a<b")
  (check= (utf8->html (cork 195 169)) "&eacute;")
  (check= (sourcecode->cork "abc") "abc")
  (check= (cork->sourcecode "abc") "abc")
  (check= (guess-wencoding "abc") "ASCII")
  ;; xml names escape the characters which they cannot hold as _code_
  (check= (tm->xml-name "a:b") "a:b")
  (check= (tm->xml-name "a_b") "a_95_b")
  (check= (tm->xml-name "1a") "_49_a")
  (check= (xml-name->tm "_49_a") "1a")
  (check= (xml-name->tm "a-b") "a-b")
  (for (s (list "a_b" "1a" "x y" "plain"))
    (check= (xml-name->tm (tm->xml-name s)) s))
  ;; the TeXmacs encoding of < and >
  (check= (string->tmstring "a<b>c") "a<less>b<gtr>c")
  (check= (string->tmstring "") "")
  (check= (tmstring->string "a<less>b<gtr>c") "a<b>c")
  (check= (tmstring->string (string->tmstring "<x> < y >")) "<x> < y >")
  ;; object->tmstring writes an object as a string
  (check= (object->tmstring "x") "\"x\"")
  (check= (object->tmstring '(a "b" 1)) "(a \"b\" 1)"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Differences between strings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; string-differences gives the changed ranges as start1 end1 start2 end2,
;; string-distance the number of changed characters.
(define (test-string-differences)
  (check= (string-differences "abc" "abc") '())
  (check= (string-differences "abc" "abXc") '(2 2 2 3))
  (check= (string-differences "abXc" "abc") '(2 3 2 2))
  (check= (string-differences "abc" "abcd") '(3 3 3 4))
  (check= (string-distance "abc" "abc") 0)
  (check= (string-distance "abc" "abXc") 1)
  (check= (string-distance "abcdef" "abXYZef") 3)
  ;; FIXME: a replacement which keeps the length is no difference
  ;; (differences in Data/String/analyze.cpp:1598 tests i1 == i2 && j1 == j2
  ;; where it means that both ranges are empty, i1 == j1 && i2 == j2):
  ;; (string-differences "abcdef" "abXdef") gives (), expected (2 3 2 3);
  ;; (string-distance "abcdef" "abXYef") gives 0, expected 2.
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Strings in the TeXmacs encoding
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; A character is a byte or a symbol <name>.
(define (test-tmstrings)
  (check= (tmstring-length "a<alpha>b") 3)
  (check= (tmstring-length "") 0)
  (check= (tmstring-length "abc") 3)
  (check= (tmstring-ref "a<alpha>b" 0) "a")
  (check= (tmstring-ref "a<alpha>b" 1) "<alpha>")
  (check= (tmstring-ref "a<alpha>b" 2) "b")
  (check= (tmstring-ref "abc" 5) "")
  (check= (tmstring-reverse-ref "a<alpha>b" 0) "b")
  (check= (tmstring-reverse-ref "a<alpha>b" 1) "<alpha>")
  (check= (tmstring-reverse-ref "abc" 2) "a")
  (check= (tmstring->list "a<alpha>b") '("a" "<alpha>" "b"))
  (check= (tmstring->list "") '())
  (check= (list->tmstring '("a" "<alpha>" "b")) "a<alpha>b")
  (check= (list->tmstring '()) "")
  ;; byte positions of the next and the previous character
  (check= (string-next "a<alpha>b" 0) 1)
  (check= (string-next "a<alpha>b" 1) 8)
  (check= (string-previous "a<alpha>b" 8) 1)
  (check= (string-previous "a<alpha>b" 9) 8)
  ;; splitting at spaces, between words, or else in the middle
  (check= (tmstring-split "hello world") '("hello" " " "world"))
  (check= (tmstring-split "abcd") '("ab" "cd"))
  (check= (tmstring-split "a<alpha>") '("a" "<alpha>"))
  ;; cases of the universal encoding, also of Greek letters
  (check= (tmstring-upcase-all "abc<alpha>") "ABC<Alpha>")
  (check= (tmstring-locase-all "ABC<Alpha>") "abc<alpha>")
  (check= (tmstring-upcase-first "<alpha>bc") "<Alpha>bc")
  (check= (tmstring-locase-first "ABC") "aBC")
  (check= (tmstring-unaccent-all (cork #\c #\a #\f 233)) "cafe")
  (check= (tmstring-letter? "a") #t)
  (check= (tmstring-letter? "<alpha>") #t)
  (check= (tmstring-letter? "1") #f)
  (check= (tmstring-before? "a" "b") #t)
  (check= (tmstring-before? "b" "a") #f)
  ;; bold math letters become ordinary letters
  (check= (downgrade-math-letters "<b-a>") "a"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Urls as data
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (us u) (url->string u))

(define (test-url-syntax)
  ;; parts of a file name
  (check= (us (url-head "a/b/c.tm")) "a/b")
  (check= (us (url-tail "a/b/c.tm")) "c.tm")
  (check= (url-suffix "a/b/c.tm") "tm")
  (check= (url-suffix "a/b/c") "")
  (check= (url-suffix "a/b/c.tar.gz") "gz")
  (check= (url-basename "a/b/c.tm") "c")
  (check= (url-basename "a/b/c.tar.gz") "c.tar")
  (check= (us (url-glue "a/b" ".tm")) "a/b.tm")
  (check= (us (url-unglue "a/b.tm" 3)) "a/b")
  (check= (us (url-glue (url-unglue "x/y.tex" 4) ".pdf")) "x/y.pdf")
  ;; relative names
  (check= (us (url-relative "/a/b/c.tm" "d.tm")) "/a/b/d.tm")
  (check= (us (url-relative "/a/b/c.tm" "../d.tm")) "/a/d.tm")
  (check= (us (url-delta "/a/b/c.tm" "/a/d/e.tm")) "../d/e.tm")
  (check= (us (url-delta "/a/b/c.tm" "/a/b/e.tm")) "e.tm")
  (check= (url-descends? "/a/b/c" "/a") #t)
  (check= (url-descends? "/a/b" "/a/b") #t)
  (check= (url-descends? "/a" "/a/b/c") #f)
  (check= (url-descends? "/ab" "/a") #f)
  (check= (us (url-append "a/b" "../c")) "a/c")
  (check= (us (url-append "/a/" "b")) "/a/b")
  (check= (us (url-parent)) "..")
  ;; roots and protocols
  (check= (url-rooted? "/a/b") #t)
  (check= (url-rooted? "a/b") #f)
  (check= (url-rooted? "http://www.texmacs.org/a") #t)
  (check= (url-rooted-web? "http://www.texmacs.org/a") #t)
  (check= (url-rooted-web? "/a") #f)
  (check= (url-rooted-tmfs? "tmfs://help/a") #t)
  (check= (url-rooted-tmfs? "/a") #f)
  (check= (url-rooted-protocol? "http://x.org/a" "http") #t)
  (check= (url-rooted-protocol? "http://x.org/a" "ftp") #f)
  (check= (url-root "http://www.texmacs.org/a") "http")
  (check= (url-root "/a/b") "default")
  (check= (url-root "a/b") "")
  (check= (us (url-unroot "http://www.texmacs.org/a/b")) "www.texmacs.org/a/b")
  (check= (us (root->url "http")) "http:/")
  ;; the structure of an url
  (check= (url-atomic? "a") #t)
  (check= (url-atomic? "a/b") #f)
  (check= (url-concat? "a/b") #t)
  (check= (url-concat? "a") #f)
  (check= (url-or? (url-or "a" "b")) #t)
  (check= (url-or? "a") #f)
  ;; the names in an url tree are symbols
  (check= (url->stree "a/b") '(concat a b))
  (check= (url->stree (url-or "a" "b")) '(or a b))
  (check= (url->stree "/a/b") '(concat (root default) (concat a b)))
  (check= (url->stree "http://x.org/a")
          (list 'concat '(root http) (list 'concat (string->symbol "x.org") 'a)))
  (check= (us (url-ref (url-or "a" (url-or "b" "c")) 1)) "a")
  (check= (us (url-expand (url-or "a" (url-or "b" "c")))) "a:b:c")
  (check= (us (url-factor (url-or "a/b" "a/c"))) "a/{b:c}")
  (check= (us (url-wildcard "*.tm")) "*.tm")
  (check= (us (url-append "a" (url-wildcard "*.tm"))) "a/*.tm")
  (check= (url-none? (url-none)) #t)
  (check= (url-none? "a") #f)
  (check= (us (url-none)) "{}")
  ;; formats from the suffix
  (check= (url-format "a.tm") "texmacs")
  (check= (url-format "a.tex") "latex")
  (check= (url-format "a.html") "html")
  (check= (url-format "a.scm") "scheme")
  ;; system and unix names
  (check= (url->system (system->url "/a/b c/d")) "/a/b c/d")
  (check= (url->unix (unix->url "/a/b")) "/a/b")
  (check= (us (unix->url "a/b/c")) "a/b/c")
  (check= (us (url-unix "/a" "b")) "/a/b")
  (check= (url-secure? "/a/b") #f)
  (check= (url-scratch? (url-scratch "a" ".tm" 3)) #t)
  (check= (us (url-tail (url-scratch "a" ".tm" 3))) "a3.tm")
  (check= (url-scratch? "/a/b.tm") #f))

;; names on the TeXmacs file system and their queries
(define (test-tmfs-names)
  (check= (tmfs-decompose-name "tmfs://help/a/b") '("help" "a/b"))
  (check= (tmfs-decompose-name "abc") '("file" "abc"))
  (check= (tmfs-decompose-name (string->url "tmfs://grep/x")) '("grep" "x"))
  (check-true (tmfs-pair? "a/b"))
  (check= (tmfs-pair? "ab") #f)
  (check= (tmfs-car "a/b/c") "a")
  (check= (tmfs-cdr "a/b/c") "b/c")
  (check= (tmfs-car "abc") #f)
  (check= (tmfs-cdr "abc") #f)
  (check= (tmfs->list "a/b/c") '("a" "b" "c"))
  (check= (tmfs->list "a") '("a"))
  (check= (list->tmfs '("a" "b" "c")) "a/b/c")
  (check= (list->tmfs (tmfs->list "x/y/z")) "x/y/z")
  (check= (strip-colon "c:/a/b") "c/a/b")
  (check= (strip-colon "/a/b") "/a/b")
  (check= (url->tmfs-string "/a/b.tm") "file/a/b.tm")
  (check= (url->tmfs-string "a/b.tm") "here/a/b.tm")
  (check= (url->tmfs-string "http://x.org/a") "http/x.org/a")
  (check= (url->tmfs-string (url-append (get-texmacs-path) "doc/x.tm"))
          "tm/doc/x.tm")
  (check= (us (tmfs-string->url "file/a/b.tm")) "/a/b.tm")
  (check= (us (tmfs-string->url "here/a/b.tm")) "a/b.tm")
  (check= (us (tmfs-string->url "http/x.org/a")) "http://x.org/a")
  (check= (tmfs-string->url "tm/doc/x.tm")
          (url-append (get-texmacs-path) "doc/x.tm"))
  ;; queries: colons are escaped as %3A
  (check= (query->list "a=1&b=x%3Ay&c") '(("a" . "1") ("b" . "x:y") ("c" . "")))
  (check= (list->query (list (cons "a" "x:y") (cons "b" "2"))) "a=x%3Ay&b=2")
  (check= (query->list (list->query (list (cons "k" "a:b=c"))))
          '(("k" . "a:b=c")))
  (check= (query-ref "a=1&b=2" "b") "2")
  (check= (query-ref "a=1&b=2" "c") ""))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Files in a temporary directory
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define structures-dir
  (string-append (url->system (url-temp-dir)) "/structures"))

(define (tmp-dir) (system->url structures-dir))
(define (tmp-file name) (system->url (string-append structures-dir "/" name)))

(define (tails l) (sort (map (lambda (u) (us (url-tail u))) l) string<?))

(define (test-url-files)
  (check= (url-directory? (tmp-dir)) #t)
  (string-save "abc" (tmp-file "a.txt"))
  (string-append-to-file "def" (tmp-file "a.txt"))
  (check= (string-load (tmp-file "a.txt")) "abcdef")
  (check= (url-size (tmp-file "a.txt")) 6)
  (check= (url-exists? (tmp-file "a.txt")) #t)
  (check= (url-regular? (tmp-file "a.txt")) #t)
  (check= (url-directory? (tmp-file "a.txt")) #f)
  (check= (url-link? (tmp-file "a.txt")) #f)
  (check= (url-test? (tmp-file "a.txt") "fr") #t)
  (check= (url-test? (tmp-file "a.txt") "d") #f)
  (check= (url-exists? (tmp-file "none.txt")) #f)
  (check= (url-size (tmp-file "none.txt")) -1)
  (system-copy (tmp-file "a.txt") (tmp-file "b.txt"))
  (check= (string-load (tmp-file "b.txt")) "abcdef")
  (system-move (tmp-file "b.txt") (tmp-file "c.tm"))
  (check= (url-exists? (tmp-file "b.txt")) #f)
  (check= (string-load (tmp-file "c.tm")) "abcdef")
  (check= (tails (url-read-directory (tmp-dir) "*")) '("a.txt" "c.tm"))
  (check= (tails (url-read-directory (tmp-dir) "*.tm")) '("c.tm"))
  (system-mkdir (tmp-file "sub"))
  (string-save "x" (tmp-file "sub/d.txt"))
  (check= (url-directory? (tmp-file "sub")) #t)
  (check= (url-test? (tmp-file "sub") "d") #t)
  (check= (tails (url-read-directory (tmp-dir) "*"))
          '("a.txt" "c.tm" "sub"))
  ;; concretizing gives the system name of an existing file
  (check= (url-concretize (tmp-file "sub/d.txt"))
          (string-append structures-dir "/sub/d.txt"))
  (check= (url->system (url-concretize* (tmp-file "sub/d.txt")))
          (string-append structures-dir "/sub/d.txt"))
  ;; wildcards, alternatives and searches
  (check= (tails (url->list (url-expand (url-complete
                   (url-append (tmp-dir) (url-wildcard "*.txt")) "r"))))
          '("a.txt"))
  (let ((both (url-or (tmp-dir) (tmp-file "sub"))))
    (check= (us (url-resolve (url-append both "d.txt") "r"))
            (us (tmp-file "sub/d.txt")))
    (check= (us (url-resolve (url-append both "a.txt") "r"))
            (us (tmp-file "a.txt")))
    (check= (url-none? (url-resolve (url-append both "zz.txt") "r")) #t))
  ;; any number of directories followed by a file name
  (let ((any (url-append (tmp-dir) (url-any))))
    (check= (map us (url->list (url-expand
                                (url-complete (url-append any "d.txt") "fr"))))
            (list (us (tmp-file "sub/d.txt"))))
    (check= (map us (url->list (url-expand
                                (url-complete (url-append any "a.txt") "fr"))))
            (list (us (tmp-file "a.txt"))))
    (check= (us (url-resolve (url-append any "d.txt") "r"))
            (us (tmp-file "sub/d.txt")))
    (check= (url-none? (url-resolve (url-append any "zz.txt") "r")) #t))
  (check= (us (url-grep "def" (url-append (tmp-dir) (url-wildcard "*.txt"))))
          (us (tmp-file "a.txt")))
  (check= (url-none? (url-grep "zzz" (url-append (tmp-dir)
                                                 (url-wildcard "*.txt"))))
          #t)
  ;; a search upwards, which stops at the directory named in the list
  (check= (us (url-search-upwards (tmp-file "sub") "a.txt" '("structures")))
          (us (tmp-file "a.txt")))
  (check= (url-none? (url-search-upwards (tmp-file "sub") "zz.txt"
                                         '("structures")))
          #t)
  ;; removal
  (system-remove (tmp-file "c.tm"))
  (check= (url-exists? (tmp-file "c.tm")) #f)
  (system-rmdir-recursive (tmp-file "sub"))
  (check= (url-exists? (tmp-file "sub")) #f)
  (check= (tails (url-read-directory (tmp-dir) "*")) '("a.txt")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Sorting, symbol properties and states
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (test-sort-and-properties)
  ;; list-sort is a stable merge sort
  (check= (list-sort '(3 1 2 5 4) <) '(1 2 3 4 5))
  (check= (list-sort '() <) '())
  (check= (list-sort '(1) <) '(1))
  (check= (list-sort '("b" "c" "a") string<?) '("a" "b" "c"))
  (check= (list-sort '((b . 1) (a . 1) (c . 0))
                     (lambda (x y) (<= (cdr x) (cdr y))))
          '((c . 0) (b . 1) (a . 1)))
  (with l '(4 2 3)
    (list-sort l <)
    (check= l '(4 2 3)))
  ;; symbol properties
  (set-symbol-prop! 'structures-test-symbol 'color "red")
  (check= (symbol-prop 'structures-test-symbol 'color) "red")
  (check= (symbol-prop 'structures-test-symbol 'size) #f)
  (set-symbol-prop! 'structures-test-symbol 'color "blue")
  (check= (symbol-prop 'structures-test-symbol 'color) "blue")
  ;; states: lists of slots and properties
  (let ((s (state-create '(((a 1) (b 2)) ((c 3)) ()))))
    (check= (state-names s) '(a b c))
    (check= (state-type s 'a) 'slot)
    (check= (state-type s 'c) 'prop)
    (check= (state-type s 'd) #f)
    (check= (state-read s 'b) 2)
    (check= (state-read s 'c) 3)
    (state-write s 'b 5)
    (check= (state-read s 'b) 5)
    (check= (state-read s 'a) 1)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Paths
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (test-paths)
  ;; orders: path-less? puts a node before its descendants, path-inf?
  ;; compares positions which are not above one another
  (check= (path-less? '(0 1) '(0 2)) #t)
  (check= (path-less? '(0 2) '(0 1)) #f)
  (check= (path-less? '(0) '(0 1)) #t)
  (check= (path-less? '(0 1) '(0 1)) #f)
  (check= (path-less? '(0 5) '(1)) #t)
  (check= (path-less-eq? '(0 1) '(0 1)) #t)
  (check= (path-less-eq? '(0 2) '(0 1)) #f)
  (check= (path-inf? '(0 1) '(0 2)) #t)
  (check= (path-inf? '(0) '(0 1)) #f)
  (check= (path-inf? '(0 1) '(0 1)) #f)
  (check= (path-inf-eq? '(0 1) '(0 1)) #t)
  (check= (path-inf-eq? '(0 1) '(1)) #t)
  (check= (path-inf-eq? '(1) '(0 1)) #f)
  (check= (path-strip '(1 2 3) '(1 2)) '(3))
  ;; cursor positions in a document of strings
  (let ((t (stree->tree '(document "abc" "de"))))
    (check= (path-start t '()) '(0 0))
    (check= (path-end t '()) '(1 2))
    (check= (path-start t '(1)) '(1 0))
    (check= (path-end t '(0)) '(0 3))
    (check= (path-next t '(0 1)) '(0 2))
    (check= (path-next t '(0 3)) '(1 0))
    (check= (path-next t '(1 2)) '(1 2))
    (check= (path-previous t '(0 2)) '(0 1))
    (check= (path-previous t '(1 0)) '(0 3))
    (check= (path-previous t '(0 0)) '(0 0)))
  ;; words, nodes and tags
  (let ((t (stree->tree '(document "ab cd ef"))))
    (check= (path-next-word t '(0 0)) '(0 2))
    (check= (path-previous-word t '(0 7)) '(0 6)))
  (let ((t (stree->tree '(document "a" (em "b") "c" (strong "d")))))
    (check= (path-next-tag t '(0 0) '(strong)) '(3 0 0))
    (check= (path-next-tag t '(0 0) '(strong em)) '(1 0 0))
    (check= (path-previous-tag t '(3 0 1) '(em)) '(1 0 1))
    ;; no further tag: the path stays
    (check= (path-next-tag t '(3 0 1) '(strong)) '(3 0 1)))
  (check= (path-next-node (stree->tree '(document "abc" (strong "de"))) '(0 0))
          '(1 0))
  (let ((t (stree->tree '(document (frac "a" "b")))))
    ;; arguments are given by the path of the child
    (check= (path-next-argument t '(0 0)) '(0 1 0))
    (check= (path-previous-argument t '(0 1)) '(0 0 1))
    (check= (path-next-argument t '(0 1)) '()))
  (check= (path-previous-section
           (stree->tree '(document (section "A") "x" (section "B") "y"))
           '(3 0))
          '(2)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Positions and tree pointers in a buffer
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (edit-step thunk)
  ;; one user action, as the event loop wraps a key press
  (archive-state)
  (start-editing)
  (with r (thunk)
    (end-editing)
    (update-forced)
    r))

(define-macro (edit . body)
  `(edit-step (lambda () ,@body)))

(define (at . l) (append (buffer-path) l))
(define (rel p) (and (list? p) (list-tail p (length (buffer-path)))))
(define (body-tree) (buffer-get-body (current-buffer)))
(define (body) (tree->stree (body-tree)))

(define (with-buffer-body doc thunk)
  ;; run @thunk in a new buffer holding @doc, then close the buffers it
  ;; opened and go back to the buffer before
  (let* ((old (current-buffer))
         (before (buffer-list))
         (u (new-buffer)))
    (switch-to-buffer* u)
    (buffer-set-body u (stree->tree doc))
    (go-start)
    (update-forced)
    (with r (check-run thunk)
      (when (and (pair? r) (== (car r) 'error))
        (check-report #f "the buffer" (object->string r)))
      (for (b (buffer-list))
        (when (nin? b before) (buffer-close b)))
      (when (buffer-exists? old) (switch-to-buffer old)))))

(define (test-paths-in-buffer)
  (with-buffer-body '(document "abc" (strong "de"))
    (lambda ()
      (check= (rel (path-start (root-tree) (at))) '(0 0))
      (check= (rel (path-next (root-tree) (at 0 3))) '(1 0))
      (check= (rel (path-next (root-tree) (at 1 0))) '(1 0 0))
      (check= (rel (path-next (root-tree) (at 1 0 0))) '(1 0 1))
      (check= (rel (path-next (root-tree) (at 1 0 2))) '(1 1))
      (check= (rel (path-previous (root-tree) (at 1 1))) '(1 0 2))
      (check= (rel (path-end (root-tree) (at))) '(1 1))))
  (with-buffer-body '(document "ab cd" (section "S") "x" (section "T"))
    (lambda ()
      (check= (rel (path-next-word (root-tree) (at 0 0))) '(0 2))
      (check= (rel (path-next-node (root-tree) (at 0 0))) '(1 0))
      (check= (rel (path-next-tag (root-tree) (at 0 0) '(section))) '(1 0 0))
      (check= (rel (path-previous-section (root-tree) (at 2 1))) '(2 1))))
  (with-buffer-body '(document (frac "a" "b"))
    (lambda ()
      (check= (rel (path-next-argument (root-tree) (at 0 0))) '(0 1 0))
      (check= (rel (path-previous-argument (root-tree) (at 0 1))) '(0 0 1))
      (check= (rel (path-next (root-tree) (at 0 0 1))) '(0 1 0)))))

;; A position follows the edits made before it and stays for the edits made
;; after it.
(define (test-positions)
  (with-buffer-body '(document "hello world")
    (lambda ()
      (let ((pos (position-new (at 0 6))))
        (check= (rel (position-get pos)) '(0 6))
        (edit (go-to (at 0 0)) (insert "XY"))
        (check= (body) '(document "XYhello world"))
        (check= (rel (position-get pos)) '(0 8))
        (edit (go-to (at 0 10)) (insert "ZZ"))
        (check= (body) '(document "XYhello woZZrld"))
        (check= (rel (position-get pos)) '(0 8))
        (edit (tree-remove! (tree-ref (body-tree) 0) 0 2))
        (check= (body) '(document "hello woZZrld"))
        (check= (rel (position-get pos)) '(0 6))
        (position-set pos (at 0 2))
        (check= (rel (position-get pos)) '(0 2))
        (edit (go-to (at 0 0)) (insert "Z"))
        (check= (rel (position-get pos)) '(0 3))
        (position-delete pos))
      ;; a new paragraph before the position moves it down
      (let ((pos (position-new (at 0 3))))
        (edit (tree-insert! (body-tree) 0 '("first")))
        (check= (body) '(document "first" "Zhello woZZrld"))
        (check= (rel (position-get pos)) '(1 3))
        (position-delete pos))
      ;; without a path, a position is made at the cursor
      (edit (go-to (at 1 2)))
      (let ((pos (position-new)))
        (check= (rel (position-get pos)) '(1 2))
        (position-delete pos)))))

;; A tree pointer follows its subtree when the tree around it changes.
(define (test-tree-pointers)
  (with-buffer-body '(document "a" "b")
    (lambda ()
      (let ((tp (tree->tree-pointer (tree-ref (body-tree) 1))))
        (check= (tree->stree (tree-pointer->tree tp)) "b")
        (edit (tree-insert! (body-tree) 0 '("z")))
        (check= (body) '(document "z" "a" "b"))
        (check= (tree->stree (tree-pointer->tree tp)) "b")
        (check= (rel (tree->path (tree-pointer->tree tp))) '(2))
        (edit (tree-remove! (body-tree) 0 2))
        (check= (body) '(document "b"))
        (check= (rel (tree->path (tree-pointer->tree tp))) '(0))
        ;; an assignment of the subtree keeps the pointer on it
        (edit (tree-set! (body-tree) 0 "new"))
        (check= (tree->stree (tree-pointer->tree tp)) "new")
        (tree-pointer-detach tp)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Pattern matching
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; match? gives #f, or the list of the possible bindings of the variables
;; 'x of the pattern ((()) when there are none).
(define (test-match)
  (check= (match? '() '()) '(()))
  (check= (match? '(a b c) '(a b c)) '(()))
  (check= (match? '(a b c) '(a b)) #f)
  ;; :* matches any tail, :%n exactly n elements
  (check= (match? '(a b c) '(a :*)) '(()))
  (check= (match? '(f) '(f :*)) '(()))
  (check= (match? '(a b c) '(a :%2)) '(()))
  (check= (match? '(a b c) '(a :%1)) #f)
  (check= (match? '(a b c) '(:%1 b :%1)) '(()))
  (check= (match? '(f 1 2 3) '(f :%1 :*)) '(()))
  ;; variables
  (check= (match? '(a b c) '(a 'x c)) '(((x . b))))
  (check= (match? '(a b b) '(a 'x 'x)) '(((x . b))))
  (check= (match? '(a b c) '(a 'x 'x)) #f)
  (check= (match? 'x ''v) '(((v . x))))
  (check= (match? '(frac "a" "b") '(frac 'x 'y)) '(((y . "b") (x . "a"))))
  (check= (match? '(f (g 1) 2) '(f (g 'x) 'y)) '(((y . 2) (x . 1))))
  ;; predicates
  (check= (match? 3 ':number?) '(()))
  (check= (match? "x" ':number?) #f)
  (check= (match? '(a 1 2) '(a :number? :number?)) '(()))
  (check= (match? '(f (g 1)) '(f (g :number?))) '(()))
  ;; trees and strings are matched through their children and characters
  (with m (match? (stree->tree '(frac "a" "b")) '(frac 'x 'y))
    (check= (length m) 1)
    (check= (tree->stree (assoc-ref (car m) 'x)) "a")
    (check= (tree->stree (assoc-ref (car m) 'y)) "b"))
  (check= (match? "ab" '(#\a #\b)) '(()))
  (check= (match? "ab" '(#\a #\c)) #f)
  ;; special patterns
  (check= (match? '(a 1 2) '(a (:repeat :number?))) '(()))
  (check= (match? '(a) '(a (:repeat :number?))) '(()))
  (check= (match? '(a 1 x) '(a (:repeat :number?))) #f)
  (check= (match? 'b '(:or a b)) '(()))
  (check= (match? 'c '(:or a b)) #f)
  (check= (match? '(f 1) '(:and (f :*) (:%1 :number?))) '(()))
  (check= (match? '(f x) '(:and (f :*) (:%1 :number?))) #f)
  (check= (match? '(f 1 2) '(f (:group :number? :number?))) '(()))
  (check= (match? '(f x) '(f (:not :number?))) '(()))
  (check= (match? '(f 1) '(f (:not :number?))) #f)
  (check= (match? '(a b) '(a (:not b))) #f)
  (check= (match? ':x '(:quote :x)) '(()))
  (check= (match? '(f :x) '(f (:quote :x))) '(()))
  (check= (match? '(f :y) '(f (:quote :x))) #f)
  ;; match with initial bindings, and bindings-add
  (check= (match '(1 2) '(:number? 'y) '((y . 2))) '(((y . 2))))
  (check= (match '(1 3) '(:number? 'y) '((y . 2))) '())
  (check= (bindings-add '((x . 1)) 'x 1) '((x . 1)))
  (check= (bindings-add '((x . 1)) 'x 2) #f)
  (check= (bindings-add '() 'y 3) '((y . 3)))
  ;; grammars: the alternatives of a rule are joined by :or
  (define-regexp-grammar
    (:structures-test-ab a b)
    (:structures-test-abs (:repeat :structures-test-ab)))
  (check= (match? 'a ':structures-test-ab) '(()))
  (check= (match? 'c ':structures-test-ab) #f)
  (check= (match? '(x a) '(x :structures-test-ab)) '(()))
  (check= (match? '(a b b a) '(:structures-test-abs)) '(()))
  (check= (match? '(a c) '(:structures-test-abs)) #f)
  (ahash-remove! match-term :structures-test-ab)
  (ahash-remove! match-term :structures-test-abs)
  (check= (ahash-ref match-term :structures-test-ab) #f))

;; tm-select gives the subtrees at the end of a path pattern, of which the
;; elements are child indices, tags (the children with that tag), :%n (any
;; n levels down), :* (any number of levels) and special patterns.
(define (test-select)
  (with doc '(document (strong "a") "b" (em "c") (strong "d"))
    (check= (tm-select doc '(strong)) '((strong "a") (strong "d")))
    (check= (tm-select doc '(strong 0)) '("a" "d"))
    (check= (tm-select doc '(1)) '("b"))
    (check= (tm-select doc '(7)) '())
    (check= (tm-select doc '(:%1)) '((strong "a") "b" (em "c") (strong "d")))
    (check= (tm-select doc '((:exclude strong))) '((em "c")))
    (check= (tm-select doc '((:range 1 3))) '("b" (em "c")))
    (check= (tm-select doc '((:or strong em))) '((strong "a") (strong "d") (em "c")))
    (check= (tm-select doc '((:and-not :%1 em))) '((strong "a") "b" (strong "d")))
    (check= (tm-select doc '((:and-not strong (:range 0 1)))) '((strong "d")))
    (check= (tm-select doc '((:and (:or strong em) em))) '((em "c")))
    (check= (tm-select doc '((:group strong 0))) '("a" "d"))
    (check= (tm-select doc '(:%0)) (list doc))
    (check= (tm-select doc '()) (list doc)))
  (with doc '(document (x (y "a")) (y "b"))
    (check= (tm-select doc '(:* y)) '((y "b") (y "a")))
    (check= (tm-select doc '(:* y 0)) '("b" "a"))
    (check= (tm-select doc '(:%2)) '((y "a") "b"))
    (check= (tm-select doc '(:first)) '((x (y "a"))))
    (check= (tm-select doc '(:last)) '((y "b"))))
  (check= (tm-select '(f 1 2) '((:match (f :number? :number?)))) '((f 1 2)))
  (check= (tm-select '(f 1 x) '((:match (f :number? :number?)))) '())
  (check= (tm-select '(f (g 1) (h 2)) '(:%1 (:match (:%1 :number?))))
          '((g 1) (h 2)))
  (check= (tm-select '(f 1 2) '((:replace (g 3)))) '((g 3)))
  (check= (tm-select "abc" '(:%1)) '())
  ;; on trees, the selected subtrees are the trees themselves
  (let* ((t (stree->tree '(document (strong "a"))))
         (l (tm-select t '(strong 0))))
    (check= (length l) 1)
    (check-true (tree? (car l)))
    (check= (tree->stree (car l)) "a")
    (check-true (tree-eq? (car l) (tree-ref t 0 0))))
  ;; select with two arguments is tm-select
  (check= (select '(document (strong "a")) '(strong 0)) '("a"))
  (check= (tm-ref '(document (strong "a") "b") 0 0) "a")
  (check= (tm-ref '(document (strong "a") "b") 1) "b")
  (check= (tm-ref '(document (strong "a") "b") 5) #f))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Logic programming
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; The rules cannot be removed again from the logic database: their names
;; start with structures-test- so that nothing else sees them.
(logic-rules
  ((structures-test-son% joris piet))
  ((structures-test-son% piet opa))
  ((structures-test-daughter% geeske opa))
  ((structures-test-child% 'x 'y) (structures-test-son% 'x 'y))
  ((structures-test-child% 'x 'y) (structures-test-daughter% 'x 'y))
  ((structures-test-desc% 'x 'y) (structures-test-child% 'x 'y))
  ((structures-test-desc% 'x 'z)
   (structures-test-child% 'x 'y) (structures-test-desc% 'y 'z)))

(logic-rules
  (assume structures-test-family%)
  ((structures-test-cousin% joris jekke)))

(logic-table structures-test-color%
  (apple red)
  (banana yellow)
  ((:or lemon lime) green))

(logic-group structures-test-fruit%
  apple banana)

(logic-dispatcher structures-test-op%
  (plus +)
  (first car))

(define (test-logic)
  ;; free variables and bindings
  (check= (free-variable 'x) ''x)
  (check= (free-variable? ''x) #t)
  (check= (free-variable? 'x) #f)
  (check= (free-variable? '(quote x y)) #f)
  (check= (bind-substitute '(f 'x (g 'y) 'z) '((x . 1) (y . 2)))
          '(f 1 (g 2) 'z))
  (check= (bind-expand '((x f 'y) (y . 2))) '((x f 2) (y . 2)))
  ;; unification
  (check= (logic-unify '(f 'x b) '(f a 'y)) '(((y . b) (x . a))))
  (check= (logic-unify '(f 'x) '(f (g 'y))) '(((x g 'y))))
  (check= (logic-unify '(f a) '(f a)) '(()))
  (check= (logic-unify '(f a) '(f b)) #f)
  (check= (logic-unify '(f 'x) '(g 'x)) #f)
  ;; FIXME: a variable which occurs twice cannot be unified: bind-unify
  ;; calls unify, which kernel/logic/logic-bind.scm:52 does not import
  ;; (it is defined in logic-unify.scm, which uses logic-bind):
  ;; (logic-unify '(f 'x 'x) '(f a a)) raises "Unbound variable: unify",
  ;; expected (((x . a))); so does (logic-query (structures-test-son% 'x 'x)),
  ;; expected ().
  ;; queries
  (check= (logic-query (structures-test-son% joris piet)) '(()))
  (check= (logic-query (structures-test-son% piet joris)) '())
  (check= (logic-query (structures-test-son% joris 'y)) '(((y . piet))))
  (check= (logic-query (structures-test-child% 'x opa))
          '(((x . piet)) ((x . geeske))))
  (check= (logic-query (structures-test-desc% joris 'y))
          '(((y . piet)) ((y . opa))))
  (check= (logic-query (structures-test-desc% joris opa)) '(()))
  (check= (logic-query (structures-test-desc% opa joris)) '())
  (check= (query '(structures-test-son% 'a 'b))
          '(((b . piet) (a . joris)) ((b . opa) (a . piet))))
  ;; a rule which assumes a condition holds only under that condition
  (check= (logic-query (structures-test-cousin% joris 'x)) '())
  (check= (logic-query (structures-test-cousin% joris 'x)
                       structures-test-family%)
          '(((x . jekke))))
  ;; facts and functional relations
  (check= (logic-holds? '(structures-test-son% joris piet)) #t)
  (check= (logic-holds? '(structures-test-son% joris opa)) #f)
  (check= (logic-apply '(structures-test-son% joris)) 'piet)
  (check= (logic-apply '(structures-test-son% nobody)) #f)
  (check= (logic-apply-list '(structures-test-son% joris)) '(piet))
  (check= (logic-apply-list '(structures-test-child% opa)) '())
  ;; tables, groups and dispatchers
  (check= (logic-ref structures-test-color% 'apple) 'red)
  (check= (logic-ref structures-test-color% 'lemon) 'green)
  (check= (logic-ref structures-test-color% 'lime) 'green)
  (check= (logic-ref structures-test-color% 'cherry) #f)
  (check= (logic-ref-list structures-test-color% 'banana) '(yellow))
  (with name 'structures-test-color%
    (check= (logic-ref ,name 'apple) 'red))
  (check= (logic-in? 'apple structures-test-fruit%) #t)
  (check= (logic-in? 'cherry structures-test-fruit%) #f)
  (check= (logic-test? structures-test-fruit% 'banana) #t)
  (check= (logic-ref structures-test-op% 'plus) +)
  (check= (logic-dispatch structures-test-op% 'plus 1 2) 3)
  (check= (logic-dispatch structures-test-op% 'plus 1 2 3) 6)
  ;; FIXME: the form with one object, which dispatches on its car, is taken
  ;; when one argument follows the key instead of none (kernel/logic/
  ;; logic-data.scm:160 tests (= (length args) 1) where it means
  ;; (null? args)): (logic-dispatch structures-test-op% '(first 2 3))
  ;; raises "Wrong type to apply: #f", expected first, and
  ;; (logic-dispatch structures-test-op% 'first '(5 6)) raises an error
  ;; (car of the symbol first), expected 5.
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Secure evaluation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (test-secure)
  (check= (secure? '(+ 1 2)) #t)
  (check= (secure? "text") #t)
  (check= (secure? 'symbol) #t)
  (check= (secure? '(system "ls")) #f)
  (check= (secure? '(string-append "a" (system "ls"))) #f)
  (check= (secure? '(if (string? "x") (string-append "a" "b") 3)) #t)
  (check= (secure? '(cond ((null? x) 1) (else 2))) #t)
  (check= (secure? '(lambda (x) (car x))) #t)
  (check= (secure? '(lambda (x) (system x))) #f)
  (check= (secure? '(with x 1 (+ x 1))) #t)
  (check= (secure? '(quote (system "ls"))) #t)
  (check= (secure? '(quasiquote (a (unquote (+ 1 2))))) #t)
  (check= (secure? '(quasiquote (a (unquote (system "ls"))))) #f)
  (check= (secure-eval '(+ 1 2)) 3)
  (check= (secure-eval '(string-append "a" "b")) "ab")
  (check= (secure-eval '(system "ls")) #f))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Preferences: declarations and reading, never setting
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define structures-pref "structures-test color")
(define structures-pref-obj "structures-test list")
(define structures-pref-seen #f)

(define (forget-preference which)
  (ahash-remove! preferences-default which)
  (ahash-remove! preferences-call-back which))

(define (test-preferences)
  ;; a declaration gives the default value and calls the call back, but
  ;; stores nothing
  (define-preferences
    ("structures-test color" "blue"
     (lambda (which val) (set! structures-pref-seen (list which val))))
    ("structures-test list" '(1 2) (lambda (which val) (noop))))
  (check= structures-pref-seen (list structures-pref "blue"))
  (check= (get-preference structures-pref) "blue")
  (check= (cpp-has-preference? structures-pref) #f)
  (check= (ahash-ref preferences-default structures-pref) "blue")
  ;; a value which is not a string is read back as an object
  (check= (get-preference structures-pref-obj) '(1 2))
  ;; the first declaration wins
  (define-preferences
    ("structures-test color" "red" (lambda (which val) (noop))))
  (check= (get-preference structures-pref) "blue")
  (check= (preference-on? structures-pref) #f)
  (check= (get-boolean-preference structures-pref) #f)
  (check= (get-preference "structures-test undeclared") "default")
  ;; pretty names
  (define-preference-names "structures-test color"
    ("blue" "Blue color"))
  (check= (get-pretty-preference structures-pref) "Blue color")
  (check= (ahash-ref preference-decode-table
                     (cons structures-pref "Blue color"))
          "blue")
  (ahash-remove! preference-encode-table (cons structures-pref "blue"))
  (ahash-remove! preference-decode-table (cons structures-pref "Blue color"))
  (check= (get-pretty-preference structures-pref) "blue")
  (forget-preference structures-pref)
  (forget-preference structures-pref-obj)
  (check= (get-preference structures-pref) "default")
  ;; look and feel
  (check= (has-look-and-feel? (look-and-feel)) #t)
  (check= (has-look-and-feel? (string-append "no-" (look-and-feel))) #f)
  (check= (has-look-and-feel? (list "structures-test-none" (look-and-feel))) #t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The suite
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (structures-test-failures)
  (:synopsis "Run the tests of the basic structures, return the failures")
  (check-suite "structures")
  (run-group "string search" test-string-search)
  (run-group "string cases" test-string-case)
  (run-group "quoting and escapes" test-string-quoting)
  (run-group "numbers and encodings" test-string-encodings)
  (run-group "string differences" test-string-differences)
  (run-group "tm strings" test-tmstrings)
  (run-group "url syntax" test-url-syntax)
  (run-group "tmfs names" test-tmfs-names)
  (when (url-exists? (tmp-dir)) (system-rmdir-recursive (tmp-dir)))
  (system-mkdir (tmp-dir))
  (run-group "url files" test-url-files)
  (system-rmdir-recursive (tmp-dir))
  (run-group "sorting, properties and states" test-sort-and-properties)
  (run-group "paths" test-paths)
  (run-group "paths in a buffer" test-paths-in-buffer)
  (run-group "positions" test-positions)
  (run-group "tree pointers" test-tree-pointers)
  (run-group "pattern matching" test-match)
  (run-group "selection" test-select)
  (run-group "logic programming" test-logic)
  (run-group "secure evaluation" test-secure)
  (run-group "preferences" test-preferences)
  (check-end))
