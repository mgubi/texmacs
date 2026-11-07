
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : code-format.scm
;; DESCRIPTION : common code formats
;; COPYRIGHT   : (C) 2022  Darcy Shen, Joris van der Hoeven
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (code-format))

;; The formats of source code: as plain lines of code, in the encoding
;; SourceCode, their documents and snippets are converted in both directions
;; by the same functions. The program language of a format is in
;; kernel/texmacs/tm-program-languages.scm.

(define (texmacs->source-code x . opts)
  (texmacs->verbatim x (acons "texmacs->verbatim:encoding" "SourceCode" '())))

(define-macro (define-code-format name title . suffixes)
  ;; the format name of source code, with its converters
  (let ((doc (string->symbol (string-append (symbol->string name) "-document")))
        (snippet (string->symbol (string-append (symbol->string name) "-snippet"))))
    `(begin
       (define-format ,name
         (:name ,title)
         (:suffix ,@suffixes))
       (converter texmacs-tree ,doc
         (:function texmacs->source-code))
       (converter ,doc texmacs-tree
         (:function code->texmacs))
       (converter texmacs-tree ,snippet
         (:function texmacs->source-code))
       (converter ,snippet texmacs-tree
         (:function code-snippet->texmacs)))))

(define-code-format cpp "C++ source code" "cpp" "cc" "hpp" "hh")
(define-code-format c "C source code" "c" "h")
(define-code-format julia "Julia source code" "jl")
(define-code-format java "Java source code" "java")
(define-code-format scala "Scala source code" "scala" "sc" "sbt")
(define-code-format javascript "JavaScript source code" "js")
(define-code-format dot "Graphviz dot source code" "dot" "gv")
(define-code-format json "JSON" "json")
(define-code-format csv "CSV" "csv")
(define-code-format go "Go source code" "go")
(define-code-format lean "Lean source code" "lean")
(define-code-format lua "Lua source code" "lua")
(define-code-format rust "Rust source code" "rs")
(define-code-format typescript "TypeScript source code" "tsx" "mts" "cts")
(define-code-format css "CSS style sheet" "css")
(define-code-format toml "TOML" "toml")
(define-code-format yaml "YAML" "yaml" "yml")
(define-code-format haskell "Haskell source code" "hs")
(define-code-format ocaml "OCaml source code" "ml" "mli")
(define-code-format shell "Shell source code" "sh" "bash" "zsh")
