
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

;; The pre-edit waiting to be pressed, (key time), and the end of a pre-edit
;; ("pre-edit:") which it superseded and which is pressed just before it.
(define delayed-keyboard-pending #f)
(define delayed-keyboard-end #f)
(define delayed-keyboard-serial 0)

(define (pre-edit-end? x)
  ;; the empty pre-edit which ends a composition ("pre-edit:")
  (with s (substring x 9 (string-length x))
    (or (== s "")
        (and (string-ends? s ":")
             (string->number (substring s 0 (- (string-length s) 1)))
             #t))))

(define (delayed-keyboard-flush)
  ;; press the pending pre-edit now, after the end it superseded; its
  ;; delayed command stops at its next poll
  (let ((pending delayed-keyboard-pending) (end delayed-keyboard-end))
    (set! delayed-keyboard-pending #f)
    (set! delayed-keyboard-end #f)
    (when pending
      (set! delayed-keyboard-serial (+ delayed-keyboard-serial 1))
      (if (and end (not (pre-edit-end? (car pending))))
          (keyboard-press (car end) (cadr end)))
      (keyboard-press (car pending) (cadr pending)))))

(tm-define (delayed-keyboard-press x t)
  (:synopsis "Press the pre-edit @x once the user has been idle for 100 ms")
  ;; Only the last pre-edit is pressed: the command of an earlier one which
  ;; has not run yet stops at its next poll. The pre-edits are numbered,
  ;; since several can come at the same time @t: a dead key typed after
  ;; another one ends the first pre-edit and starts the next one in the
  ;; same event (#870).
  ;; The end of a pre-edit is never skipped, since it resets the state of
  ;; the editor (pre_edit_skip, edit_keyboard.cpp): when a newer pre-edit
  ;; comes, the end is pressed just before it. A committed key flushes
  ;; them first (committed-keyboard-press).
  (set! delayed-keyboard-serial (+ delayed-keyboard-serial 1))
  (if (pre-edit-end? x) (set! delayed-keyboard-end (list x t)))
  (set! delayed-keyboard-pending (list x t))
  (with serial delayed-keyboard-serial
    (delayed
      (:while (== serial delayed-keyboard-serial))
      (:idle 100)
      (delayed-keyboard-flush))))

(tm-define (committed-keyboard-press key t)
  (:synopsis "Press the key @key, which is not a pre-edit, at once")
  ;; The pending pre-edits are pressed first, so that the key is read in
  ;; the state they leave, as when the user types slowly (#955). Else a key
  ;; typed less than 100 ms after the end of the pre-edit of an accent
  ;; inserted at once (math) was still read as its commit: a letter
  ;; composed after a dead caron was lost, the commit of a second dead
  ;; tilde dropped. The order of the interfaces is kept: the end of a
  ;; pre-edit comes after the commit in Qt, before it in Cocoa and SDL.
  (delayed-keyboard-flush)
  (keyboard-press key t))
