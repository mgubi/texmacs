;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : c-lang.scm
;; DESCRIPTION : the C Language
;; COPYRIGHT   : (C) 2026  Massimiliano Gubinelli
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; The keywords of C11 (and C23: bool, true, false, nullptr...), as for the
;; tables of C++ (cpp-lang.scm); the comments are those of default-lang.

(texmacs-module (c-lang)
  (:use (prog default-lang)))

(tm-define (parser-feature lan key)
  (:require (and (== lan "c") (== key "keyword")))
  `(,(string->symbol key)
    (constant
      "false" "true" "NULL" "nullptr"
      "EOF" "stdin" "stdout" "stderr")
    (constant_type
      "bool" "char" "double" "float" "int" "long" "short" "signed"
      "unsigned" "void" "size_t" "ptrdiff_t" "wchar_t"
      "_Bool" "_Complex" "_Imaginary")
    (declare_type "enum" "struct" "union" "typedef")
    (keyword
      "alignas" "alignof" "auto" "const" "constexpr" "extern" "inline"
      "register" "restrict" "sizeof" "static" "static_assert"
      "thread_local" "typeof" "typeof_unqual" "volatile"
      "_Alignas" "_Alignof" "_Atomic" "_Generic" "_Noreturn"
      "_Static_assert" "_Thread_local")
    (keyword_conditional
      "break" "case" "continue" "default" "do" "else" "for" "goto" "if"
      "switch" "while")
    (keyword_control
      "return")))

(tm-define (parser-feature lan key)
  (:require (and (== lan "c") (== key "operator")))
  `(,(string->symbol key)
    (operator
      "+" "-" "/" "*" "%" ;; Arith
      "|" "&" "^" "~" ;; Bit
      "<<" ">>"
      "==" "!=" "<" ">" "<=" ">=" "&&" "||" "!" ;; Boolean
      "+=" "-=" "/=" "*=" "%=" "|=" "&=" "^=" "<<=" ">>=" ;; Assignment
      "=" ":" "?" ";" "," "++" "--")
    (operator_special "->")
    (operator_field ".")
    (operator_openclose "{" "[" "(" ")" "]" "}")))

(tm-define (parser-feature lan key)
  (:require (and (== lan "c") (== key "number")))
  `(,(string->symbol key)
    (bool_features
     "prefix_0x" "prefix_0b"
     "sci_notation")
    (suffix
     (long "l" "L" "ll" "LL")
     (unsigned "u" "U")
     (float "f" "F"))))

(tm-define (parser-feature lan key)
  (:require (and (== lan "c") (== key "string")))
  `(,(string->symbol key)
    (bool_features
     "hex_with_8_bits" "octal_upto_3_digits")
    (escape_sequences "\\" "\"" "'" "a" "b" "f" "n" "r" "t" "v" "0")))

(tm-define (parser-feature lan key)
  (:require (and (== lan "c") (== key "preprocessor")))
  `(,(string->symbol key)
    (directives
     "define" "undef" "include" "embed"
     "if" "ifdef" "ifndef" "else" "elif" "elifdef" "elifndef" "endif"
     "line" "error" "warning" "pragma")))

;; Preferences for syntax highlighting (those of C++)

(define (notify-c-pref var val)
  (syntax-read-preferences "c"))

(define-preferences
  ("syntax:c:none" "black" notify-c-pref)
  ("syntax:c:comment" "dark grey" notify-c-pref)
  ("syntax:c:keyword" "dark magenta" notify-c-pref)
  ("syntax:c:error" "dark red" notify-c-pref)
  ("syntax:c:preprocessor" "dark brown" notify-c-pref)
  ("syntax:c:preprocessor_directive" "dark green" notify-c-pref)
  ("syntax:c:constant_type" "#4040c0" notify-c-pref)
  ("syntax:c:constant_number" "#4040c0" notify-c-pref)
  ("syntax:c:constant_string" "dark red" notify-c-pref))
