
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : ts-highlight.scm
;; DESCRIPTION : syntax highlighting of programs by tree-sitter
;; COPYRIGHT   : (C) 2026  Massimiliano Gubinelli
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; The program languages of TeXmacs which tree-sitter highlights
;; (Plugins/TreeSitter/ts_highlight.cpp) ask for their highlighter at their
;; first use; it is made here, once the tables of the language are ready:
;; the query misc/tree-sitter/<lan>/highlights.scm and the colour class of
;; each of its captures.

(texmacs-module (treesit ts-highlight)
  (:use (treesit ts-registry)))

(define-preferences
  ("tree-sitter highlighting" "on" noop))

;; program language of TeXmacs -> language of tree-sitter
(define-public treesit-highlight-languages
  '(("scheme" . "scheme") ("json" . "json") ("c" . "c")))

;; capture name -> colour class (initialize_color_encodings in
;; System/Language/language.cpp); the longest prefix of the name decides,
;; names without class (@variable...) keep the colour of the text
(define-public treesit-capture-classes
  '(("comment" . "comment")
    ("string" . "constant_string")
    ("string.escape" . "constant_char")
    ("string.special.key" . "declare_identifier")
    ("escape" . "constant_char")
    ("character" . "constant_char")
    ("number" . "constant_number")
    ("float" . "constant_number")
    ("boolean" . "constant")
    ("constant" . "constant")
    ("keyword" . "keyword")
    ("keyword.control" . "keyword_control")
    ("keyword.conditional" . "keyword_conditional")
    ("keyword.directive" . "preprocessor_directive")
    ("preproc" . "preprocessor_directive")
    ("function" . "variable_function")
    ("function.builtin" . "constant_function")
    ("function.special" . "preprocessor")
    ("function.macro" . "preprocessor")
    ("type" . "declare_type")
    ("label" . "declare_identifier")
    ("property" . "variable_identifier")
    ("module" . "declare_module")
    ("operator" . "operator")
    ("punctuation.bracket" . "operator_openclose")
    ("punctuation.delimiter" . "operator_field")
    ("delimiter" . "operator_field")
    ("error" . "error")))

(define-public (treesit-capture-class name)
  ;; the colour class of a capture name, or ""
  (let loop ((l treesit-capture-classes) (best "") (len -1))
    (if (null? l) best
        (let* ((p (caar l)) (n (string-length p)))
          (if (and (> n len)
                   (or (== name p)
                       (and (> (string-length name) n)
                            (string-starts? name p)
                            (== (string-ref name n) #\.))))
              (loop (cdr l) (cdar l) n)
              (loop (cdr l) best len))))))

(define (query-url ts)
  (url-concretize
   (string-append "$TEXMACS_PATH/misc/tree-sitter/" ts "/highlights.scm")))

(define-public (treesit-install-highlighter lan id)
  ;; give the highlighter of lan to the C++ side, if its query compiles
  (let* ((ts (assoc-ref treesit-highlight-languages lan))
         (file (and ts (query-url ts)))
         (q (and id file (url-exists? file)
                 (tree-sitter-query-new id (string-load file)))))
    (and q (>= q 0)
         (begin
           (tree-sitter-set-highlighter
            lan q (map treesit-capture-class (tree-sitter-query-capture-names q)))
           #t))))

(define (refresh-buffers)
  ;; typeset again the open buffers: their lines were coloured before
  (for (b (buffer-list))
    (when (nnull? (buffer->views b))
      (with-buffer b (update-current-buffer)))))

(tm-define (treesit-highlighter-request lan)
  ;; called by C++ at the first use of lan; when the tables are cached the
  ;; highlighter is installed at once, else after their generation
  (let ((ts (assoc-ref treesit-highlight-languages lan)))
    (when (and ts (== (get-preference "tree-sitter highlighting") "on")
               (treesit-supported? ts))
      (let ((id (treesit-language ts)))
        (if id (treesit-install-highlighter lan id)
            (treesit-prepare ts (lambda (id)
                                  (when (and id (treesit-install-highlighter lan id))
                                    (refresh-buffers)))))))))

(tm-define (treesit-schedule-updates)
  ;; called by C++ when lines changed colour without being modified
  (delayed
    (:idle 1)
    (for (t (tree-sitter-pending-updates))
      (let ((p (tree->path t)))
        (when p (update-path p))))))
