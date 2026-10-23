
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : emoji-test.scm
;; DESCRIPTION : tests of the typesetting of emoji
;; COPYRIGHT   : (C) 2026  Massimiliano Gubinelli
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; The emoji are drawn with the color font which comes with TeXmacs,
;; Twemoji Mozilla (TeXmacs/fonts/truetype/twemoji, chosen in smart_font.cpp):
;; every emoji of it is one em wide, so that its width tells whether it was
;; found (a character which no font has is written out as its code, several
;; em wide). A sequence (a family joined by U+200D, a flag of two regional
;; indicators, a skin tone, a keycap, a symbol followed by U+FE0F) is one
;; piece of text for the typesetter (emoji_sequence_end, language.cpp) and
;; one ligature of the font (its ccmp feature, unicode_font.cpp), one em
;; wide; what is left of a sequence takes no room. The colors of the layers
;; (COLR) cannot be seen from here: see the samples in docs/color-fonts.md.

(texmacs-module (check emoji-test)
  (:use (check check-lib)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Helpers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (width t)
  ;; the logical width of @t typeset as a line, in tmpt
  (with r (tree->stree (texmacs-expand `(box-info ,t "w")))
    (string->number (cadr r))))

(define (near? x y tol) (<= (abs (- x y)) tol))

(define (em) (length-decode "1fn"))

(define (one-em? t)
  ;; @t is typeset as one emoji
  (near? (width t) (em) (/ (em) 20)))

(define smile "<#1F600>")
(define rocket "<#1F680>")
(define man "<#1F468>")
(define woman "<#1F469>")
(define girl "<#1F467>")
(define zwj "<#200D>")
(define vs16 "<#FE0F>")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Single emoji
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (test-single)
  (check-group "single emoji")
  (check-true (one-em? smile))
  (check-true (one-em? rocket))
  ;; in every style of the text, and in formulas
  (check-true (one-em? `(with "font-series" "bold" ,smile)))
  (check-true (one-em? `(with "font-shape" "italic" ,smile)))
  (check-true (one-em? `(with "font-family" "ss" ,smile)))
  (check-true (one-em? `(with "font-family" "tt" ,smile)))
  (check-true (one-em? `(with "mode" "math" ,smile)))
  ;; side by side: one em each
  (check-true (near? (width (string-append smile rocket smile))
                     (* 3 (em)) (/ (em) 10)))
  ;; at another size, in proportion
  (check-true (near? (width `(with "font-base-size" "20" ,smile))
                     (* 2 (em)) (/ (em) 10))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Sequences
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (test-sequences)
  (check-group "sequences")
  ;; a family: three people joined by U+200D
  (check-true (one-em? (string-append man zwj woman zwj girl)))
  ;; a flag: two regional indicators, and two flags side by side
  (check-true (one-em? "<#1F1EE><#1F1F9>"))
  (check-true (near? (width "<#1F1EE><#1F1F9><#1F1EB><#1F1F7>")
                     (* 2 (em)) (/ (em) 10)))
  ;; a skin tone
  (check-true (one-em? "<#1F44D><#1F3FD>"))
  ;; a keycap, from a digit or a sign of the text
  (check-true (one-em? (string-append "#" vs16 "<#20E3>")))
  (check-true (one-em? (string-append "1" vs16 "<#20E3>")))
  ;; a heart, which is a symbol unless followed by U+FE0F
  (check-true (one-em? (string-append "<#2764>" vs16)))
  ;; in a sentence
  (check-true (near? (width (string-append "a" man zwj woman zwj girl "b"))
                     (+ (width "ab") (em)) (/ (em) 10)))
  ;; a man and a woman joined are no sequence of the font: two emoji
  (check-true (near? (width (string-append man zwj woman))
                     (* 2 (em)) (/ (em) 10))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; What is left of a sequence
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (test-marks)
  (check-group "marks alone")
  (check-true (near? (width (string-append "x" vs16 "y")) (width "xy") 2))
  (check-true (near? (width (string-append "x" zwj "y")) (width "xy") 2))
  (check-true (near? (width (string-append "x<#FE0E>y")) (width "xy") 2))
  ;; a joiner at the end, or between emoji which make no ligature
  (check-true (one-em? (string-append smile zwj)))
  (check-true (near? (width (string-append smile zwj rocket))
                     (* 2 (em)) (/ (em) 10))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; All tests
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (run-group thunk)
  (with r (check-run thunk)
    (when (and (pair? r) (== (car r) 'error))
      (check-report #f "the group" (object->string r)))))

(tm-define (emoji-test-failures)
  (:synopsis "Run the tests of the emoji and return the failures")
  (check-suite "emoji")
  (run-group test-single)
  (run-group test-sequences)
  (run-group test-marks)
  (check-end))
