
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : ai-sessions.scm
;; DESCRIPTION : the focus bar of the sessions of the AI engines
;; COPYRIGHT   : (C) 2026  Massimiliano Gubinelli
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; The icons of a session of a chatbot, after those of all the sessions: its
;; model, the reasoning, Insert answer (focus-ai-icons, init-ai.scm).
;;
;; (A module of its own, loaded once by the plug-in, whose file is loaded
;; again when a key is given: its definitions would else be made again, and
;; the icons shown as many times. It uses the module whose definition it
;; overloads, which is so loaded before it.)

(texmacs-module (tools ai ai-sessions)
  (:use (dynamic session-menu)))

(define (ai-session-field? t)
  (and (field-context? t)
       (in? (get-env "prog-language") (ai-models))))

(tm-menu (focus-extra-icons t)
  (:require (ai-session-field? t))
  (dynamic (former t))
  (dynamic (focus-ai-icons (get-env "prog-language"))))
