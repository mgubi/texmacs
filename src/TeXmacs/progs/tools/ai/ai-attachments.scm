
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : ai-attachments.scm
;; DESCRIPTION : the files attached to the questions of the AI sessions
;; COPYRIGHT   : (C) 2026  Massimiliano Gubinelli
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; A question of a session carries its files as (ai-attachment name file)
;; (session.ts: the name in a frame; file the path on this system, Cork as
;; every string of a document). They stay with the question: an earlier
;; question sends them again with the conversation.
;;
;; The text files (sources, LaTeX, Markdown, BibTeX, TeXmacs documents as
;; LaTeX) are put after the text of the question (ai-serialize); the
;; images and the PDF files are parts of the message, which ai.cpp makes
;; for each engine (ai-session-attachments in init-ai.scm gives them).

(texmacs-module (tools ai ai-attachments))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The kinds of files
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define ai-image-types
  '(("png" . "image/png") ("jpg" . "image/jpeg") ("jpeg" . "image/jpeg")
    ("gif" . "image/gif") ("webp" . "image/webp")))

(define ai-text-suffixes
  '("txt" "text" "tex" "ltx" "sty" "cls" "bib" "bst" "md" "markdown" "org"
    "rst" "csv" "tsv" "json" "xml" "html" "htm" "svg" "yaml" "yml" "toml"
    "ini" "cfg" "log" "py" "scm" "lisp" "el" "c" "h" "cc" "cpp" "cxx" "hpp"
    "m" "mm" "java" "js" "rs" "go" "rb" "jl" "r" "lua" "sh" "hs" "ml" "f90"
    "sage" "mac" "gp" "mpl" "asy" "mp" "ts"))

(define (ai-file-suffix file)
  (locase-all (url-suffix (system->url file))))

;; "image", "pdf", "text" or "texmacs" (a document, sent as LaTeX), #f
(tm-define (ai-attachment-kind file)
  (with s (ai-file-suffix file)
    (cond ((assoc s ai-image-types) "image")
          ((== s "pdf") "pdf")
          ((== s "tm") "texmacs")
          ((in? s ai-text-suffixes) "text")
          (else #f))))

(tm-define (ai-attachment-mime file)
  (with s (ai-file-suffix file)
    (cond ((assoc s ai-image-types) => cdr)
          ((== s "pdf") "application/pdf")
          (else "text/plain"))))

;; the largest files sent: those of the engines (Claude takes 32 MB a
;; request, Gemini 20 MB of parts inline; a text as the document is)
(define ai-binary-max (* 20 1024 1024))
(define ai-text-max 400000) ; characters

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The attachments of a question
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (ai-attachment-stree? x)
  (and (tm-func? x 'ai-attachment 2) (string? (cadr x)) (string? (caddr x))))

;; the files of a question (an stree), as (name file) in UTF-8, in order
(tm-define (ai-stree-attachments x)
  (cond ((ai-attachment-stree? x)
         (list (list (cork->utf8 (cadr x)) (cork->utf8 (caddr x)))))
        ((pair? x) (append-map ai-stree-attachments (cdr x)))
        (else '())))

;; the question without its files
(tm-define (ai-strip-attachments x)
  (cond ((not (pair? x)) x)
        ((ai-attachment-stree? x) "")
        (else (cons (car x) (map ai-strip-attachments (cdr x))))))

(define (ai-file-text file)
  (with u (system->url file)
    (if (== (ai-attachment-kind file) "texmacs")
        (catch #t
          (lambda ()
            (convert (string-load u) "texmacs-document" "latex-document"
                     (cons "texmacs->latex:encoding" "utf-8")))
          (lambda args ""))
        (string-load u))))

;; the text files of a question, as they come after its text
(tm-define (ai-attachments-text l)
  (apply string-append
         (map (lambda (a)
                (let* ((name (car a)) (file (cadr a))
                       (kind (ai-attachment-kind file)))
                  (if (and (in? kind '("text" "texmacs"))
                           (url-exists? (system->url file)))
                      (with s (ai-file-text file)
                        (string-append
                         "\n\nThe attached file " name
                         (if (== kind "texmacs")
                             " (a TeXmacs document, as LaTeX)" "")
                         ":\n\n```\n"
                         (if (> (string-length s) ai-text-max)
                             (string-append (substring s 0 ai-text-max)
                                            "\n[...]")
                             s)
                         "\n```"))
                      "")))
              l)))

;; the files of a question which ai.cpp sends as parts of its message, as
;; "mime<tab>name<tab>file"
(tm-define (ai-binary-attachments l)
  (with bin (list-filter l (lambda (a)
                             (in? (ai-attachment-kind (cadr a)) '("image" "pdf"))))
    (map (lambda (a)
           (string-append (ai-attachment-mime (cadr a)) "\t" (car a)
                          "\t" (cadr a)))
         bin)))

;; what keeps the files of a question from being sent to an engine, or #f
(tm-define (ai-attachments-problem name l)
  (list-or
   (map (lambda (a)
          (let* ((n (car a)) (file (cadr a))
                 (kind (ai-attachment-kind file))
                 (u (system->url file)))
            (cond ((not (url-exists? u))
                   (string-append "the attached file " n " is not there any more ("
                                  file ")"))
                  ((not kind)
                   (string-append "the attached file " n
                                  " is neither a text, an image nor a PDF file"))
                  ((and (in? kind '("image" "pdf"))
                        (> (url-size u) ai-binary-max))
                   (string-append "the attached file " n " is larger than 20 MB"))
                  ((and (== kind "pdf")
                        (in? name '("open-mistral-7b" "ollama" "albert")))
                   (string-append "the attached file " n
                                  " is a PDF file, which this engine does not take"
                                  " (attach its text, or ask another engine)"))
                  ((and (== kind "image") (== name "albert"))
                   (string-append "the attached file " n
                                  " is an image, which this engine does not take"))
                  (else #f))))
        l)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The files which may be sent
;;
;; A document says which files are attached (their names and where they
;; are), and the frame shows only the name: a document received from
;; someone else could attach any file of this system, which a question of
;; its session would send. A file is sent only once it was attached in this
;; TeXmacs (Attach, a drop), or the user allowed it when a question would
;; send it (ai-request-attachment-problem, init-ai.scm).
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define ai-allowed-files (make-ahash-table)) ; file (UTF-8) -> #t

(tm-define (ai-allow-files l)
  (for-each (lambda (a) (ahash-set! ai-allowed-files (cadr a) #t)) l))

;; the attachments (name file) of l which were neither attached nor allowed
(tm-define (ai-files-not-allowed l)
  (list-filter l (lambda (a) (not (ahash-ref ai-allowed-files (cadr a))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Attaching a file
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (ai-input-field? t)
  (and (tree-in? t '(input input-text input-math unfolded-io folded-io
                     unfolded-io-text folded-io-text unfolded-io-math
                     folded-io-math))
       (> (tree-arity t) 1)))

;; the question of an executable fold of a chatbot (script-input lan session
;; question answer, ai-folds.scm)
(define (ai-fold-question? t)
  (and (tree-up t) (== (tree-index t) 2)
       (defined? 'ai-fold?) (ai-fold? (tree-up t))))

;; the input of the field of the cursor (or the question of its fold), if
;; the cursor is in it
(tm-define (ai-cursor-input)
  (let* ((p (cursor-path))
         (t (path->tree (cDr p))))
    (let loop ((t t))
      (cond ((not t) #f)
            ((and (tree-up t) (ai-input-field? (tree-up t))
                  (== (tree-index t) 1))
             t)
            ((ai-fold-question? t) t)
            ((tree-up t) (loop (tree-up t)))
            (else #f)))))

(tm-define (ai-insert-attachment u)
  (let* ((file (url->system u))
         (name (url->system (url-tail u))))
    (if (not (ai-attachment-kind file))
        (set-message (string-append name " is neither a text, an image "
                                    "nor a PDF file")
                     "Attach a file")
        (begin
          (ai-allow-files (list (list name file)))
          (insert `(ai-attachment ,(utf8->cork name) ,(utf8->cork file)))
          ;; (the cursor after it, where the question goes on)
          (with t (tree-innermost 'ai-attachment)
            (when t (tree-go-to t :end)))))))

(tm-define (ai-attach-file)
  (if (not (ai-cursor-input))
      (set-message "Put the cursor in a question of the session first"
                   "Attach a file")
      (choose-file ai-insert-attachment "Attach a file" "generic")))

;; Files dropped while the cursor is in a question are attached to it, as
;; Attach does, instead of their names being inserted (the default of
;; kbd-handlers.scm); what is dropped elsewhere, or is not all files which
;; can be attached, is inserted as before. (The question is the one of the
;; cursor, as the editor puts a drop at the cursor.)
(define (ai-dropped-files x)
  ;; the files of what is dropped, or #f
  (let* ((l (cond ((string? x) (list x))
                  ((tm-func? x 'concat) (cdr x))
                  ((tm-func? x 'document) (cdr x))
                  (else (list x))))
         (files (map (lambda (y)
                       (cond ((string? y) (cork->utf8 y))
                             ((and (tm-func? y 'image) (string? (cadr y)))
                              (cork->utf8 (cadr y)))
                             (else #f)))
                     (list-filter l (lambda (y) (!= y ""))))))
    (and (nnull? files)
         (list-and (map (lambda (f)
                          (and f (string-starts? f "/")
                               (url-exists? (system->url f))
                               (not (url-directory? (system->url f)))
                               (ai-attachment-kind f)))
                        files))
         files)))

(tm-define (mouse-drop-event x y obj)
  (:require (and (ai-cursor-input) (ai-dropped-files (tm->stree obj))))
  (for-each (lambda (f) (ai-insert-attachment (system->url f)))
            (ai-dropped-files (tm->stree obj))))
