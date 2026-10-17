
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; C++ source files
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-format cpp
  (:name "C++ source code")
  (:suffix "cpp" "cc" "hpp" "hh"))

(define (texmacs->cpp x . opts)
  (texmacs->verbatim x (acons "texmacs->verbatim:encoding" "SourceCode" '())))

(define (cpp->texmacs x . opts)
  (code->texmacs x))

(define (cpp-snippet->texmacs x . opts)
  (code-snippet->texmacs x))

(converter texmacs-tree cpp-document
  (:function texmacs->cpp))

(converter cpp-document texmacs-tree
  (:function cpp->texmacs))
  
(converter texmacs-tree cpp-snippet
  (:function texmacs->cpp))

(converter cpp-snippet texmacs-tree
  (:function cpp-snippet->texmacs))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Julia source files
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-format julia
  (:name "Julia source code")
  (:suffix "jl"))
  
(define (texmacs->julia x . opts)
  (texmacs->verbatim x (acons "texmacs->verbatim:encoding" "SourceCode" '())))

(define (julia->texmacs x . opts)
  (code->texmacs x))

(define (julia-snippet->texmacs x . opts)
  (code-snippet->texmacs x))

(converter texmacs-tree julia-document
  (:function texmacs->julia))

(converter julia-document texmacs-tree
  (:function julia->texmacs))
  
(converter texmacs-tree julia-snippet
  (:function texmacs->julia))

(converter julia-snippet texmacs-tree
  (:function julia-snippet->texmacs))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Java source files
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-format java
  (:name "Java source code")
  (:suffix "java"))

(define (texmacs->java x . opts)
  (texmacs->verbatim x (acons "texmacs->verbatim:encoding" "SourceCode" '())))

(define (java->texmacs x . opts)
  (code->texmacs x))

(define (java-snippet->texmacs x . opts)
  (code-snippet->texmacs x))

(converter texmacs-tree java-document
  (:function texmacs->java))

(converter java-document texmacs-tree
  (:function java->texmacs))
  
(converter texmacs-tree java-snippet
  (:function texmacs->java))

(converter java-snippet texmacs-tree
  (:function java-snippet->texmacs))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Scala source files
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-format scala
  (:name "Scala source code")
  (:suffix "scala" "sc" "sbt"))

(define (texmacs->scala x . opts)
  (texmacs->verbatim x (acons "texmacs->verbatim:encoding" "SourceCode" '())))

(define (scala->texmacs x . opts)
  (code->texmacs x))

(define (scala-snippet->texmacs x . opts)
  (code-snippet->texmacs x))

(converter texmacs-tree scala-document
  (:function texmacs->scala))

(converter scala-document texmacs-tree
  (:function scala->texmacs))
  
(converter texmacs-tree scala-snippet
  (:function texmacs->scala))

(converter scala-snippet texmacs-tree
  (:function scala-snippet->texmacs))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; JavaScript source files
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-format javascript
  (:name "JavaScript source code")
  (:suffix "js"))

(define (texmacs->javascript x . opts)
  (texmacs->verbatim x (acons "texmacs->verbatim:encoding" "SourceCode" '())))

(define (javascript->texmacs x . opts)
  (code->texmacs x))

(define (javascript-snippet->texmacs x . opts)
  (code-snippet->texmacs x))

(converter texmacs-tree javascript-document
  (:function texmacs->javascript))

(converter javascript-document texmacs-tree
  (:function javascript->texmacs))
  
(converter texmacs-tree javascript-snippet
  (:function texmacs->javascript))

(converter javascript-snippet texmacs-tree
  (:function javascript-snippet->texmacs))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Graphviz dot source files
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-format dot
  (:name "Graphviz dot source code")
  (:suffix "dot" "gv"))

(define (texmacs->dot x . opts)
  (texmacs->verbatim x (acons "texmacs->verbatim:encoding" "SourceCode" '())))

(define (dot->texmacs x . opts)
  (code->texmacs x))

(define (dot-snippet->texmacs x . opts)
  (code-snippet->texmacs x))

(converter texmacs-tree dot-document
  (:function texmacs->dot))

(converter dot-document texmacs-tree
  (:function dot->texmacs))
  
(converter texmacs-tree dot-snippet
  (:function texmacs->dot))

(converter dot-snippet texmacs-tree
  (:function dot-snippet->texmacs))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; JSON source files
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-format json
  (:name "JSON")
  (:suffix "json"))

(define (texmacs->json x . opts)
  (texmacs->verbatim x (acons "texmacs->verbatim:encoding" "SourceCode" '())))

(define (json->texmacs x . opts)
  (code->texmacs x))

(define (json-snippet->texmacs x . opts)
  (code-snippet->texmacs x))

(converter texmacs-tree json-document
  (:function texmacs->json))

(converter json-document texmacs-tree
  (:function json->texmacs))
  
(converter texmacs-tree json-snippet
  (:function texmacs->json))

(converter json-snippet texmacs-tree
  (:function json-snippet->texmacs))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CSV source files
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-format csv
  (:name "CSV")
  (:suffix "csv"))

(define (texmacs->csv x . opts)
  (texmacs->verbatim x (acons "texmacs->verbatim:encoding" "SourceCode" '())))

(define (csv->texmacs x . opts)
  (code->texmacs x))

(define (csv-snippet->texmacs x . opts)
  (code-snippet->texmacs x))

(converter texmacs-tree csv-document
  (:function texmacs->csv))

(converter csv-document texmacs-tree
  (:function csv->texmacs))
  
(converter texmacs-tree csv-snippet
  (:function texmacs->csv))

(converter csv-snippet texmacs-tree
  (:function csv-snippet->texmacs))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Go source files
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-format go
  (:name "Go source code")
  (:suffix "go"))

(define (texmacs->go x . opts)
  (texmacs->verbatim x (acons "texmacs->verbatim:encoding" "SourceCode" '())))

(define (go->texmacs x . opts)
  (code->texmacs x))

(define (go-snippet->texmacs x . opts)
  (code-snippet->texmacs x))

(converter texmacs-tree go-document
  (:function texmacs->go))

(converter go-document texmacs-tree
  (:function go->texmacs))

(converter texmacs-tree go-snippet
  (:function texmacs->go))

(converter go-snippet texmacs-tree
  (:function go-snippet->texmacs))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Lean source files
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-format lean
  (:name "Lean source code")
  (:suffix "lean"))

(define (texmacs->lean x . opts)
  (texmacs->verbatim x (acons "texmacs->verbatim:encoding" "SourceCode" '())))

(define (lean->texmacs x . opts)
  (code->texmacs x))

(define (lean-snippet->texmacs x . opts)
  (code-snippet->texmacs x))

(converter texmacs-tree lean-document
  (:function texmacs->lean))

(converter lean-document texmacs-tree
  (:function lean->texmacs))

(converter texmacs-tree lean-snippet
  (:function texmacs->lean))

(converter lean-snippet texmacs-tree
  (:function lean-snippet->texmacs))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Lua source files
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-format lua
  (:name "Lua source code")
  (:suffix "lua"))

(define (texmacs->lua x . opts)
  (texmacs->verbatim x (acons "texmacs->verbatim:encoding" "SourceCode" '())))

(define (lua->texmacs x . opts)
  (code->texmacs x))

(define (lua-snippet->texmacs x . opts)
  (code-snippet->texmacs x))

(converter texmacs-tree lua-document
  (:function texmacs->lua))

(converter lua-document texmacs-tree
  (:function lua->texmacs))

(converter texmacs-tree lua-snippet
  (:function texmacs->lua))

(converter lua-snippet texmacs-tree
  (:function lua-snippet->texmacs))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Rust source files
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-format rust
  (:name "Rust source code")
  (:suffix "rs"))

(define (texmacs->rust x . opts)
  (texmacs->verbatim x (acons "texmacs->verbatim:encoding" "SourceCode" '())))

(define (rust->texmacs x . opts)
  (code->texmacs x))

(define (rust-snippet->texmacs x . opts)
  (code-snippet->texmacs x))

(converter texmacs-tree rust-document
  (:function texmacs->rust))

(converter rust-document texmacs-tree
  (:function rust->texmacs))

(converter texmacs-tree rust-snippet
  (:function texmacs->rust))

(converter rust-snippet texmacs-tree
  (:function rust-snippet->texmacs))
