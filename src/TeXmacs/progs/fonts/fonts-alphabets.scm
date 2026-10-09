
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : fonts-alphabets.scm
;; DESCRIPTION : the families whose letters are those of another alphabet
;; COPYRIGHT   : (C) 2026  Massimiliano Gubinelli
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; The families "cal", "Euler" and "Bbb" (with their variants) were TeX
;; fonts whose letters are calligraphic, fraktur or blackboard bold (cmsy,
;; rsfs, eufm, msbm, bbm...). Tau has no TeX fonts: they are the letters of
;; these alphabets in Latin Modern Math (alphabet_font.cpp).

(texmacs-module (fonts fonts-alphabets))

(set-font-rules
  `(((cal $v $a $b $s $d) (alphabet cal latinmodern-math $s $d))
    ((cal* $v $a $b $s $d) (alphabet cal latinmodern-math $s $d))
    ((cal** $v $a $b $s $d) (alphabet cal latinmodern-math $s $d))

    ((Euler $v $a $b $s $d) (alphabet frak latinmodern-math $s $d))

    ((Bbb $v $a $b $s $d) (alphabet bbb latinmodern-math $s $d))
    ((Bbb* $v $a $b $s $d) (alphabet bbb latinmodern-math $s $d))
    ((Bbb** $v $a $b $s $d) (alphabet bbb latinmodern-math $s $d))
    ((Bbb*** $v $a $b $s $d) (alphabet bbb latinmodern-math $s $d))
    ((Bbb**** $v $a $b $s $d) (alphabet bbb latinmodern-math $s $d))))
