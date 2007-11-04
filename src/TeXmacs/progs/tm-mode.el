
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : tm-mode.el
;; DESCRIPTION : for editing TeXmacs scheme files with Emacs
;; COPYRIGHT   : (C) 2002  Joris van der Hoeven, David Allouche
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This software falls under the GNU general public license and comes WITHOUT
;; ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for details.
;; If you don't have this file, write to the Free Software Foundation, Inc.,
;; 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Define lists of special keywords
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq nullary-keywords
  '(cond call/cc values define-preferences menu-dynamic
    case-lambda kbd-map kbd-wildcards kbd-commands kbd-symbols
    define-grammar drd-rule drd-rules assume texmacs-modes
    delayed dialogue on-entry on-exit widget-delayed
    association-tile bar concat dense-bar dense-tile document
    header-bar sequence short-bar short-tile tile))

(setq nullary-misc
  '(:use :inherit))

(setq nullary-indent
  (append nullary-keywords nullary-misc))

(setq unary-keywords
  '(with-result
    and-let* setup-append-if
    while for repeat when
    drd-group drd-table drd-dispatcher
    with-cc with-aux with-mutator
    with-action with-module with-cursor with-server
    dialogue-user widget-with
    aspect block-input button form
    input internal short-input))

(setq unary-definitions
  '(texmacs-module provide-public
    define-group define-macro define-public-macro
    tm-define tm-define-macro tm-property request-handler
    tm-build tm-build-macro tm-build-widget
    menu-bind menu-extend define-table define-format))

(setq unary-no-highlight
  '(format interactive))

(setq unary-indent
  (append unary-keywords unary-definitions unary-no-highlight))

(setq binary-keywords
  '(with with-global and-with with-innermost receive
    with-environment with-environment* converter
    hidden-input pagelet radio-button toggle-button))

(setq binary-indent
  binary-keywords)

(setq ternary-keywords
  '(ahash-with canvas-input))

(setq ternary-indent
  ternary-keywords)

(setq other-keywords
  '(define-secure-symbols map-in-order link promise plugin-configure
    plugin-input-converters use-modules export import-from inherit-modules
    lazy-menu lazy-keyboard lazy-define lazy-format lazy-input-converter
    form-cancel form-done form-next form-previous radio suggestions toggle))

(setq highlight-definitions
  unary-definitions)

(setq highlight-keywords
  (append nullary-keywords unary-keywords unary-definitions
	  binary-keywords ternary-keywords other-keywords))

(setq highlight-native
  '(let let* begin for if else while define define-macro
    define-public define-public-macro))

(setq highlight-any
  (append highlight-definitions highlight-keywords highlight-native))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Define TeXmacs style
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(add-hook 'scheme-mode-hook '(lambda () (texmacs-style)))

(defun texmacs-style ()
  (set-fill-column 79)
  (setq comment-column 40)
  (auto-fill-mode 1)
  (font-lock-add-keywords 'scheme-mode
   (list
    (cons
     (concat "\\<\\("
      (mapconcat 'symbol-name highlight-keywords "\\|") "\\)\\>")
     'font-lock-keyword-face)
    (cons
     (concat "(\\("
      (mapconcat 'symbol-name highlight-definitions "\\|")
      "\\)\\>[ 	]*\\((?\\)\\(\\sw+\\)\\>")
     '(3 font-lock-function-name-face))
    (cons
     (concat "(\\("
      (mapconcat 'symbol-name
       '(converter) "\\|")
      "\\)\\>[ 	]*\\((?\\)\\(\\sw+ \\sw+\\)\\>")
     '(3 font-lock-function-name-face))
    '("\\<\\(\\sw+%\\)\\>" . font-lock-type-face)))
  (dolist (s ternary-indent)
    (put s 'scheme-indent-function 3))
  (dolist (s binary-indent)
    (put s 'scheme-indent-function 2))
  (dolist (s unary-indent)
    (put s 'scheme-indent-function 1))
  (dolist (s nullary-indent)
    (put s 'scheme-indent-function 0)))
