
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : kbd-handlers.scm
;; DESCRIPTION : Default keyboard handlers
;; COPYRIGHT   : (C) 2010  Joris van der Hoeven
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (kernel gui kbd-handlers)
  (:use (kernel texmacs tm-define)))

(tm-define ShiftMask     256)
(tm-define LockMask      512)
(tm-define ControlMask  1024)
(tm-define Mod1Mask     2048)
(tm-define Mod2Mask     4096)
(tm-define Mod3Mask     8192)
(tm-define Mod4Mask    16384)
(tm-define Mod5Mask    32768)

(tm-define (keyboard-press key time)
  (key-press key))

(tm-define (keyboard-focus has-focus? time)
  (noop))

(tm-define (mouse-event key x y mods time data)
  ;;(display* "mouse-event " key ", " x ", " y ", " mods ", " time ", " data "\n")
  (mouse-any key x y mods (+ time 0.0) data))

(tm-define (mouse-drop-event x y obj)
  ;;(display* "mouse-drop-event " x ", " y ", " (tm->stree obj) "\n")
  (insert obj))

(tm-define (dropped-text s)
  (:synopsis "The tree of the plain text @s (UTF-8) dropped on a document")
  ;; The interfaces give a dropped text (TMView, QTMWidget, the SDL drops
  ;; of Vue) to this function, which reads it as a pasted plain text is
  ;; read (verbatim-snippet): the lines become paragraphs, the tabs spaces,
  ;; and a dot accent, macron or breve of the text stays a character
  ;; (#866, #918), instead of the newline, tab and backspace of Cork.
  (convert s "verbatim-snippet" "texmacs-tree"))

(tm-define (kbd-insert s)
  (insert s))

(define delayed-keyboard-serial 0)

(tm-define (delayed-keyboard-command x t)
  (:synopsis "The command which presses the pre-edit @x after 100 ms idle")
  ;; Only the last pre-edit is pressed: the command of an earlier one which
  ;; has not run yet does nothing. The pre-edits are numbered, since several
  ;; can come at the same time @t: a dead key typed after another one ends
  ;; the first pre-edit and starts the next one in the same event (#870).
  ;; The command takes the idle time and returns, as for exec-delayed-pause,
  ;; the delay before it is called again, or #t when it is done.
  (set! delayed-keyboard-serial (+ delayed-keyboard-serial 1))
  (with serial delayed-keyboard-serial
    (lambda (idle)
      (cond ((!= serial delayed-keyboard-serial) #t)
            ((< idle 100) (- 100 idle))
            (else (keyboard-press x t) #t)))))

(tm-define (delayed-keyboard-press x t)
  (with cmd (delayed-keyboard-command x t)
    (exec-delayed-pause (lambda () (cmd (idle-time))))))
