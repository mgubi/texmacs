;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : ts-chars.scm
;; DESCRIPTION : the character classes of C, for the external scanners
;; COPYRIGHT   : (C) 2026  Massimiliano Gubinelli
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; iswspace, iswalpha... on code points, as the scanners in C use them (the
;; ASCII characters as in C, the others by their Unicode properties).

(texmacs-module (treesit ts-chars)
  (:use (treesit ts-base) (treesit ts-nfa)))

(define-public (ts-wspace? c)
  (or (= c 32) (and (>= c 9) (<= c 13)) (= c #x85) (= c #xa0) (= c #x1680)
      (and (>= c #x2000) (<= c #x200a)) (= c #x2028) (= c #x2029)
      (= c #x202f) (= c #x205f) (= c #x3000)))

(define-public (ts-wdigit? c) (and (>= c 48) (<= c 57)))

(define-public (ts-walpha? c)
  (cond ((< c 128) (or (and (>= c 65) (<= c 90)) (and (>= c 97) (<= c 122))))
        (else (cs-contains? (ts-unicode-property "Alphabetic") c))))

(define-public (ts-walnum? c) (or (ts-wdigit? c) (ts-walpha? c)))

(define-public (ts-wxdigit? c)
  (or (ts-wdigit? c) (and (>= c 65) (<= c 70)) (and (>= c 97) (<= c 102))))
