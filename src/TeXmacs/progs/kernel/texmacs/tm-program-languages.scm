;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : tm-program-languages.scm
;; DESCRIPTION : the program languages of TeXmacs
;; COPYRIGHT   : (C) 2026  Massimiliano Gubinelli
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; The one list of the program languages (the values of prog-language), from
;; which the rest of TeXmacs takes them: the modes in-<name>% and
;; in-prog-<name>% (tm-modes.scm), the groups of the code tags (text-drd.scm),
;; the Insert menus (text-menu.scm), the languages of the code blocks of the
;; Markdown and docx/odt converters (convert/tools/code-languages.scm), and
;; the highlighting and the editing by tree-sitter (progs/treesit). Still
;; written elsewhere for a new language: the macros of its tags
;; (packages/environment/env-program.ts), its format and <name>-lang.scm
;; (plugins/code), its grammar (misc/tree-sitter) and, if it has one, the
;; port of the external scanner of its grammar (progs/treesit).
;;
;; An entry is (name property...), with the properties
;;   (label "C++")        its name in the menus (none: not in the menus)
;;   (tags inline block)  the tags of inline code and of blocks of code
;;   (grammar "cpp")      its grammar of tree-sitter, in misc/tree-sitter
;;   (comment "//")       the start of a line comment
;;   (names "cpp" ...)    its names in other formats (the fences of Markdown,
;;                        docx/odt): the first is written, all are read
;; The order is that of the groups of the code tags.

(texmacs-module (kernel texmacs tm-program-languages))

(define program-language-table
  '(("scheme" (label "Scheme") (tags scm scm-code) (grammar "scheme")
     (comment ";") (names "scheme" "scm" "lisp" "guile"))
    ("cpp" (label "C++") (tags cpp cpp-code) (grammar "cpp") (comment "//")
     (names "cpp" "c" "c++" "cc" "h" "hpp"))
    ("dot" (tags dot-lang dot-code) (names "dot"))
    ("mathemagix" (label "Mathemagix") (tags mmx mmx-code)
     (names "mathemagix" "mmx"))
    ("r" (label "R") (tags r r-code) (grammar "r") (comment "#") (names "r"))
    ("fortran" (label "Fortran") (tags fortran fortran-code)
     (names "fortran"))
    ("octave" (tags octave-lang octave-code) (names "octave" "matlab"))
    ("python" (label "Python") (tags python python-code) (grammar "python")
     (comment "#") (names "python" "py" "python3"))
    ("julia" (label "Julia") (tags julia julia-code) (grammar "julia")
     (comment "#") (names "julia"))
    ("java" (label "Java") (tags java java-code) (grammar "java")
     (comment "//") (names "java"))
    ("javascript" (label "JavaScript") (tags javascript-lang javascript-code)
     (grammar "javascript") (comment "//") (names "javascript" "js"))
    ("json" (tags json-lang json-code) (grammar "json") (names "json"))
    ("scala" (label "Scala") (tags scala scala-code) (names "scala"))
    ("scilab" (label "Scilab") (tags scilab scilab-code) (names "scilab"))
    ("shell" (label "Shell") (tags shell shell-code) (grammar "bash")
     (comment "#") (names "sh" "bash" "shell" "zsh" "console"))
    ("go" (label "Go") (tags go-lang go-code) (grammar "go") (comment "//")
     (names "go" "golang"))
    ("lean" (label "Lean") (tags lean lean-code) (grammar "lean")
     (comment "--") (names "lean" "lean4"))
    ("lua" (label "Lua") (tags lua lua-code) (grammar "lua") (comment "--")
     (names "lua"))
    ("rust" (label "Rust") (tags rust rust-code) (grammar "rust")
     (comment "//") (names "rust" "rs"))
    ("typescript" (label "TypeScript") (tags typescript-lang typescript-code) (grammar "typescript") (comment "//")
     (names "typescript" "ts" "tsx"))
    ("html" (label "HTML") (tags html-lang html-code) (grammar "html")
     (names "html" "xhtml"))
    ("css" (label "CSS") (tags css-lang css-code) (grammar "css")
     (names "css"))
    ("toml" (label "TOML") (tags toml-lang toml-code) (grammar "toml") (comment "#")
     (names "toml"))
    ("ocaml" (label "OCaml") (tags ocaml-lang ocaml-code) (grammar "ocaml")
     (names "ocaml" "ml"))
    ("latex" (label "LaTeX") (tags latex-lang latex-code) (grammar "latex") (comment "%")
     (names "latex" "tex"))
    ;; C has a grammar, which the tests use; its code is written as C++
    ("c" (grammar "c") (comment "//"))))

(define-public (program-languages)
  ;; the names of the program languages
  (map car program-language-table))

(define-public (program-language-ref name key)
  ;; the values of the property key of the language name, or #f
  (let* ((l (assoc name program-language-table))
         (p (and l (assq key (cdr l)))))
    (and p (cdr p))))

(define-public (program-language-label name)
  (let ((v (program-language-ref name 'label))) (and v (car v))))

(define-public (program-language-inline-tag name)
  (let ((v (program-language-ref name 'tags))) (and v (car v))))

(define-public (program-language-block-tag name)
  (let ((v (program-language-ref name 'tags))) (and v (cadr v))))

(define-public (program-language-grammar name)
  (let ((v (program-language-ref name 'grammar))) (and v (car v))))

(define-public (program-language-comment name)
  (let ((v (program-language-ref name 'comment))) (and v (car v))))

(define-public (program-language-names name)
  (or (program-language-ref name 'names) '()))

(define-public (program-language-menu-names)
  ;; the languages in the menus, in the order of their labels
  (sort (list-filter (program-languages) program-language-label)
        (lambda (a b) (string<? (program-language-label a)
                                (program-language-label b)))))

(define-public (program-language-grammars)
  ;; the grammars of tree-sitter, each once
  (let loop ((l (program-languages)) (acc '()))
    (if (null? l) (reverse acc)
        (let ((g (program-language-grammar (car l))))
          (loop (cdr l) (if (and g (not (member g acc))) (cons g acc) acc))))))

;; the languages with a grammar are highlighted by tree-sitter
(when (defined? 'tree-sitter-wrap-language)
  (for-each (lambda (name)
              (when (program-language-grammar name)
                (tree-sitter-wrap-language name)))
            (program-languages)))

(define-public (program-language-mode-items)
  ;; the items of texmacs-modes for the modes of the languages
  (apply append
         (map (lambda (name)
                (let ((in (string->symbol (string-append "in-" name "%")))
                      (in-prog (string->symbol (string-append "in-prog-" name "%"))))
                  `((,in (== (get-env "prog-language") ,name))
                    (,in-prog #t in-prog% ,in))))
              (program-languages))))
