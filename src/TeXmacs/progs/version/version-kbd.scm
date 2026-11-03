
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : version-kbd.scm
;; DESCRIPTION : keyboard shortcuts for versioning
;; COPYRIGHT   : (C) 2010  Joris van der Hoeven
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (version version-kbd)
  (:use (generic generic-kbd)
	(version version-compare)
        (version git-widgets)))

(texmacs-modes
  (in-git-document% (git-context? (current-buffer)) with-versioning-tool%))

;; The shortcuts make the same checks as the menus, and explain why they
;; do nothing otherwise

(define (shortcut-refused msg)
  (set-message msg "Git")
  #f)

(define (shortcut-root)
  (with root (current-git-root)
    (cond ((not (git-available?))
           (shortcut-refused
            "Git was not found: see Version -> Git preferences"))
          ((not root) (shortcut-refused "Not in a Git working tree"))
          ((not (git-trusted? root))
           (shortcut-refused (string-append "This repository is not trusted: "
                                            "use Version -> Use Git in this "
                                            "folder")))
          (else root))))

(define (shortcut-compare)
  (and-with root (shortcut-root)
    (with u (current-buffer)
      (cond ((or (url-rooted-tmfs? u) (not (git-texmacs-file? u)))
             (shortcut-refused "Only TeXmacs documents can be compared"))
            ((in? (git-file-state u) '(untracked added))
             (shortcut-refused "This document is not in the last commit"))
            (else (git-compare-with u "HEAD"))))))

(kbd-map
  (:mode in-git-document?)
  ("version g" (when (shortcut-root) (git-open-tool)))
  ("version c" (and-with root (shortcut-root)
                 (if (git-simple-mode?)
                     (git-interactive-save-snapshot root)
                     (git-interactive-commit))))
  ("version y" (and-with root (shortcut-root) (git-sync root)))
  ("version s" (and-with root (shortcut-root) (git-show-status root)))
  ("version =" (shortcut-compare)))

(kbd-map
  (:mode with-versioning-tool?)
  ("version home" (version-first-difference))
  ("version pageup" (version-previous-difference))
  ("version pagedown" (version-next-difference))
  ("version end" (version-last-difference))
  ("version up" (version-previous-difference))
  ("version down" (version-next-difference))
  ("version |" (version-show 'version-both))
  ("version left" (version-show 'version-old))
  ("version right" (version-show 'version-new))
  ("version return" (version-retain 'current))
  ("version 1" (version-retain 0))
  ("version 2" (version-retain 1)))

(kbd-map
  (:mode in-versioning?)
  ("C-home" (version-first-difference))
  ("C-pageup" (version-previous-difference))
  ("C-pagedown" (version-next-difference))
  ("C-end" (version-last-difference))
  ("C-up" (version-previous-difference))
  ("C-down" (version-next-difference))
  ("C-|" (version-show 'version-both))
  ("C-left" (version-show 'version-old))
  ("C-right" (version-show 'version-new))
  ("C-;" (version-show-paragraph 'version-both))
  ("C-[" (version-show-paragraph 'version-old))
  ("C-]" (version-show-paragraph 'version-new))
  ("C-:" (version-show-all 'version-both))
  ("C-{" (version-show-all 'version-old))
  ("C-}" (version-show-all 'version-new))
  ("C-return" (version-retain 'current))
  ("C-1" (version-retain 0))
  ("C-2" (version-retain 1))
  ("C-c" (version-retain-all 'current-old)))

(tm-define (kbd-control-enter t shift?)
  (:require (and (tree-is-buffer? t) (in-versioning?)))
  (version-retain 'current))
