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

;; The keys are those of other editors where TeXmacs leaves them free: F12
;; for the definition, F8 for the diagnostics; tab completes (lsp-edit,
;; kbd-variant); A-F formats, A-R renames, A-Q proposes the fixes, A-D lists
;; the diagnostics (F2, the key of renames elsewhere, is taken).

(texmacs-module (lsp lsp-kbd)
  (:use (kernel gui kbd-define)
        (lsp lsp-edit)
        (lsp lsp-actions)))

(kbd-map
  (:mode in-prog?)
  ("F12" (lsp-goto-definition))
  ("S-F12" (lsp-hover))
  ("F8" (lsp-next-diagnostic #t))
  ("S-F8" (lsp-next-diagnostic #f))
  ("A-F" (lsp-format))
  ("A-R" (interactive lsp-rename))
  ("A-Q" (lsp-quick-fix))
  ("A-D" (lsp-show-diagnostics-list)))

(menu-bind lsp-menu
  ((check "Follow source files" "v" (lsp-on?))
   (toggle-preference "lsp"))
  ---
  ("Go to definition" (lsp-goto-definition))
  ("Information at cursor" (lsp-hover))
  ("Complete" (lsp-complete))
  ("Next diagnostic" (lsp-next-diagnostic #t))
  ("Previous diagnostic" (lsp-next-diagnostic #f))
  ("List of diagnostics" (lsp-show-diagnostics-list))
  ---
  ("Format" (lsp-format))
  ("Rename symbol..." (interactive lsp-rename))
  ("Quick fix" (lsp-quick-fix))
  ---
  ((check "Information on mouse rest" "v" (lsp-mouse-hover-on?))
   (toggle-preference "lsp:mouse hover"))
  ---
  ("Restart server" (lsp-restart))
  ("Server log" (lsp-show-log))
  ("Stop all servers" (lsp-stop-servers)))
