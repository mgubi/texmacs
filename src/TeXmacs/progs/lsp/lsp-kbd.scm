;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : lsp-kbd.scm
;; DESCRIPTION : keys and menus of the language servers
;; COPYRIGHT   : (C) 2026  Massimiliano Gubinelli
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; The keys are those of other editors: F12 for the definition, F8 for the
;; diagnostics; tab completes (lsp-edit, kbd-variant).

(texmacs-module (lsp lsp-kbd)
  (:use (kernel gui kbd-define)
        (lsp lsp-edit)))

(kbd-map
  (:mode in-prog?)
  ("F12" (lsp-goto-definition))
  ("S-F12" (lsp-hover))
  ("F8" (lsp-next-diagnostic #t))
  ("S-F8" (lsp-next-diagnostic #f)))

(menu-bind lsp-menu
  ((check "Follow source files" "v" (lsp-on?))
   (toggle-preference "lsp"))
  ---
  ("Go to definition" (lsp-goto-definition))
  ("Information at cursor" (lsp-hover))
  ("Complete" (lsp-complete))
  ("Next diagnostic" (lsp-next-diagnostic #t))
  ("Previous diagnostic" (lsp-next-diagnostic #f))
  ---
  ("Restart server" (lsp-restart))
  ("Server log" (lsp-show-log))
  ("Stop all servers" (lsp-stop-all)))
