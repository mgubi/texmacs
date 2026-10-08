
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : init-markdown.scm
;; DESCRIPTION : setup Markdown converters
;; COPYRIGHT   : (C) 2026  Massimiliano Gubinelli
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (convert markdown init-markdown))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Markdown
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Markdown is text with a few conventions: a text is not recognized as
;; Markdown by its contents, only a file by its suffix.

(define-format markdown
  (:name "Markdown")
  (:suffix "md" "markdown" "mkd"))

(lazy-define (convert markdown markdownin) parse-markdown-snippet)
(lazy-define (convert markdown markdownin) parse-markdown-document)
(lazy-define (convert markdown markdownout) serialize-markdown)
(lazy-define (convert markdown markdowntm) markdown->texmacs)
(lazy-define (convert markdown tmmarkdown) texmacs->markdown)

(converter markdown-document markdown-stree
  (:function parse-markdown-document))

(converter markdown-stree markdown-document
  (:function serialize-markdown))

(converter markdown-snippet markdown-stree
  (:function parse-markdown-snippet))

(converter markdown-stree markdown-snippet
  (:function serialize-markdown))

(converter markdown-stree texmacs-stree
  (:function markdown->texmacs))

(converter texmacs-stree markdown-stree
  (:function-with-options texmacs->markdown)
  (:option "texmacs->markdown:html" "on")
  (:option "texmacs->markdown:front-matter" "on"))
