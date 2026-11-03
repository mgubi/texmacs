
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : code-navigate.scm
;; DESCRIPTION : the outline of program code and the definitions
;; COPYRIGHT   : (C) 2026  Massimiliano Gubinelli
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; The outline of the code at the cursor and the definition of the name at
;; the cursor, from the providers (code-features.scm, src/docs/code-features.md):
;;   outline     (procedure) the entries (level kind name detail path) of the
;;               code at the cursor, in its order: path is where the cursor
;;               goes (a server: its symbols, which it sees in the file;
;;               tree-sitter: the definitions of its tags, also in the code
;;               blocks of documents)
;;   definition  (procedure) goes to the definition of the name at the
;;               cursor, #t if it did (a server: also in the other files;
;;               tree-sitter: in the same code)
;; The servers come first when one follows the file, and tree-sitter where
;; there is none, or when the server finds nothing.

(texmacs-module (code code-navigate)
  (:use (code code-features)
        (code code-fold)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Definitions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (code-go-to-definition)
  (:synopsis "Goes to the definition of the name at the cursor")
  (when (not (code-ask 'definition))
    (set-message "No definition found" "")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The outline
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (code-outline-entries)
  (:synopsis "The outline of the code at the cursor: (level kind name detail path)")
  (or (code-ask 'outline) '()))

(define (entry-label e)
  (string-append (caddr e) " (" (cadr e) ")"))

;; A menu with thousands of items takes seconds to make, and nobody can read
;; it: the page of the outline has more of them.
(define definitions-menu-max 500)

(tm-define (code-definitions-menu-items)
  (:synopsis "The entries of the menu Definitions, then 'all if some are left out")
  (with l (code-outline-entries)
    (if (> (length l) definitions-menu-max)
        (rcons (list-head l definitions-menu-max) 'all)
        l)))

(tm-menu (code-definitions-menu)
  ;; NOTE: a menu is made with its parent, also behind a ->, but its for is
  ;; only evaluated when it opens: the outline, which may take a while, must
  ;; stay inside it
  (for (e (code-definitions-menu-items))
    (assuming (== e 'all)
      ---
      ("More definitions" (code-outline)))
    (assuming (!= e 'all)
      ((eval (string-append (make-string (* 2 (car e)) #\space) (entry-label e)))
       (go-to (list-ref e 4))))))

(define outline-buffer #f)      ; the buffer whose outline is on the page
(define outline-refresh? #f)

(define (outline-page) (aux-name "Outline"))

(tm-define (code-go-to buffer p)
  (:synopsis "Goes to the place @p of the buffer @buffer")
  (:secure #t)
  (with u (string->url buffer)
    (when (buffer-exists? u)
      (switch-to-buffer u)
      (go-to p))))

;; Each line of the page takes about 70 KB once typeset, kept while the page is
;; open, and the page is made again after each edit: it says when it stops.
(define outline-page-max 5000)

(tm-define (code-outline-body u entries*)
  (:synopsis "The body of the page of the outline with @entries* of the file @u")
  (let* ((n (length entries*))
         (more? (> n outline-page-max))
         (entries (if more? (list-head entries* outline-page-max) entries*)))
    `(document
      (strong (concat "Outline of " (verbatim ,(url->system (url-tail u)))))
      ,@(if more?
            (list (string-append "The first " (number->string outline-page-max)
                                 " of " (number->string n) " definitions: "
                                 "Search and Go to definition reach the others"))
            '())
      ,@(if (null? entries) '("No definitions")
            (map (lambda (e)
                   `(action (concat
                             ,(make-string (* 4 (car e)) #\space)
                             (with "color" "#606060" ,(cadr e))
                             " "
                             (strong (verbatim ,(caddr e)))
                             ,@(if (== (cadddr e) "") '()
                                   (list " " `(with "color" "#606060"
                                                (verbatim ,(cadddr e))))))
                            ,(string-append "(code-go-to "
                                            (object->string (url->string u)) " '"
                                            (object->string (list-ref e 4)) ")")))
                 entries)))))

(define (outline-document u entries)
  `(document (style (tuple "generic"))
             (body ,(code-outline-body u entries))))

(tm-define (code-outline)
  (:synopsis "Shows the outline of the code at the cursor on a page")
  ;; (the page follows the buffer as long as it is open)
  (if (not (code-available? 'outline))
      (set-message "No outline for this code" "")
      (let ((u (current-buffer))
            (entries (code-outline-entries)))
        (set! outline-buffer u)
        (open-auxiliary "Outline" (outline-document u entries)))))

(define (refresh-outline)
  (set! outline-refresh? #f)
  (when outline-buffer
    (if (not (and (buffer-exists? (outline-page))
                  (buffer-exists? outline-buffer)))
        (set! outline-buffer #f)
        (when (== (current-buffer) outline-buffer)
          (let ((u outline-buffer)
                (entries (code-outline-entries)))
            ;; (the page is made again, the cursor stays in the code)
            (aux-set-document "Outline" (outline-document u entries)))))))

(tm-define (notify-buffer-changed name)
  (former name)
  (when (and outline-buffer (== name (url->string outline-buffer))
             (not outline-refresh?))
    (set! outline-refresh? #t)
    (delayed
      (:idle 1000)
      (refresh-outline))))
