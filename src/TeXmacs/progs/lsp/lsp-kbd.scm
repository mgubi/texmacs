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
;; kbd-variant); std . proposes the fixes, std F formats, std E renames,
;; std M lists the diagnostics (F2, the key of renames elsewhere, is taken),
;; std U the references of a symbol, std O the outline of the file, std P
;; the symbols of the project, std H the calls of a function.
;; NOTE: no A- keys: on macOS option composes a character (option q is
;; a ligature), and the key never reaches the editor.

(texmacs-module (lsp lsp-kbd)
  (:use (kernel gui kbd-define)
        (prog prog-menu)
        (lsp lsp-edit)
        (lsp lsp-actions)
        (lsp lsp-navigate)
        (lsp lsp-decorate)))

(tm-define (lsp-followed?)
  (:synopsis "Does a language server follow the current buffer?")
  (and (lsp-on?) (lsp-buffer-followed?)))

;; the keys are those of the buffers which a server follows: elsewhere F12
;; and S-F12 keep their meaning (the slides, the folds)
(texmacs-modes
  (in-lsp% (lsp-followed?) in-prog%))

(kbd-map
  (:mode in-lsp?)
  ("F12" (lsp-goto-definition))
  ("S-F12" (lsp-hover))
  ("F8" (lsp-next-diagnostic #t))
  ("S-F8" (lsp-next-diagnostic #f))
  ("std F" (lsp-format))
  ("std E" (interactive lsp-rename))
  ("std ." (lsp-quick-fix))
  ("std M" (lsp-show-diagnostics-list))
  ("std U" (lsp-references))
  ("std O" (lsp-outline))
  ("std P" (interactive lsp-find-symbol))
  ("std H" (lsp-call-hierarchy)))

(menu-bind lsp-menu
  ((check "Follow source files" "v" (lsp-on?))
   (toggle-preference "lsp"))
  ---
  ("Go to definition" (lsp-goto-definition))
  ("Find references" (lsp-references))
  ("Outline" (lsp-outline))
  ("Find symbol in project..." (interactive lsp-find-symbol))
  ("Calls" (lsp-call-hierarchy))
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
  ((check "Highlight the symbol at the cursor" "v"
          (!= (get-preference "lsp:highlight") "off"))
   (toggle-preference "lsp:highlight"))
  ((check "Colours of the server" "v"
          (!= (get-preference "lsp:semantic colors") "off"))
   (toggle-preference "lsp:semantic colors"))
  ((check "Inlay hints" "v" (!= (get-preference "lsp:inlay hints") "off"))
   (toggle-preference "lsp:inlay hints"))
  ---
  ("Restart server" (lsp-restart))
  ("Server log" (lsp-show-log))
  ("Stop all servers" (lsp-stop-servers)))

;; The focus bar of a source file which a server follows: after the name of
;; the language, the server and its state (its menu), its work (indexing...),
;; the signature which is typed (the current argument in bold) and the
;; numbers of errors and warnings (their list)

(define (status-label st)
  (with name (car st)
    (case (cadr st)
      ((starting) (string-append name ": starting"))
      ((ready) name)
      ((stopped) (string-append name ": stopped"))
      (else name))))

(define (problems-label st)
  (let ((e (list-ref st 3)) (w (list-ref st 4)))
    (cond ((and (== e 0) (== w 0)) "No problems")
          ((== w 0) (if (== e 1) "1 error" (string-append (number->string e)
                                                          " errors")))
          ((== e 0) (if (== w 1) "1 warning"
                        (string-append (number->string w) " warnings")))
          (else (string-append (number->string e)
                               (if (== e 1) " error, " " errors, ")
                               (number->string w)
                               (if (== w 1) " warning" " warnings"))))))

(tm-menu (lsp-focus-icons)
  (with st (lsp-focus-status)
    (assuming st
      //
      (mini #t
        (=> (balloon (eval (status-label st)) "The language server")
            (link lsp-menu)))
      (assuming (caddr st)
        (mini #t (inert ((eval (utf8->cork (caddr st))) (noop)))))
      (assuming (!= (cadr st) 'none)
        (mini #t
          ((balloon (eval (problems-label st)) "List of diagnostics")
           (lsp-show-diagnostics-list))))
      (with sig (lsp-focus-signature)
        (assuming sig
          //
          (mini #t
            (text (car sig))
            (bold (text (cadr sig)))
            (text (caddr sig))))))))

(tm-menu (focus-code-icons t)
  (dynamic (former t))
  (dynamic (lsp-focus-icons)))
