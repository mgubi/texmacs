;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : lsp-documents.scm
;; DESCRIPTION : the documents of language servers, and their diagnostics
;; COPYRIGHT   : (C) 2026  Massimiliano Gubinelli
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; A server knows the documents which are open (didOpen), by their URI;
;; each change gives it the whole new text (didChange without a range,
;; which all servers accept) with a higher version, and only when the text
;; has changed. The diagnostics which the servers publish are kept by URI,
;; and lsp-diagnostics-changed is called for each publication (the editor
;; will show them).
;;
;; The buffers: a source file (the code style, a line in each paragraph)
;; is a document of the server of its language (prog-language), for the
;; project around it: the nearest directory with a .git, a
;; compile_commands.json... or else its own directory.

(texmacs-module (lsp lsp-documents)
  (:use (lsp lsp-base)
        (lsp lsp-client)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Documents
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (sync-option c key)
  ;; the option @key of textDocumentSync in the capabilities of the server
  ;; (a number is the kind of change, with open and close)
  (with s (lsp-ref (lsp-client-capabilities c) "textDocumentSync")
    (cond ((lsp-number-value s)
           (cond ((== key "openClose") (lsp-boolean (> (lsp-number-value s) 0)))
                 ((== key "change") s)
                 (else #f)))
          ((func? s 'attr) (lsp-ref s key))
          (else #f))))

(define (sync-kind c)
  ;; 0: no changes, 1: whole texts, 2: changes (to which whole texts are
  ;; sent too); servers which do not say are given the changes
  (if (lsp-client-running? c)
      (or (lsp-number-value (sync-option c "change")) 1)
      1))

(define (text-document uri)
  (lsp-object "uri" uri))

(tm-define (lsp-document-open? c uri)
  (and (ahash-ref (lsp-client-ref c 'documents) uri) #t))

(tm-define (lsp-document-version c uri)
  (with d (ahash-ref (lsp-client-ref c 'documents) uri)
    (and d (car d))))

(tm-define (lsp-document-open c uri lan-id text)
  (:synopsis "Opens the document @uri, in the language @lan-id, with @text")
  ;; (a document which is open already is changed)
  (if (lsp-document-open? c uri)
      (lsp-document-change c uri text)
      (begin
        (ahash-set! (lsp-client-ref c 'documents) uri (list 1 text lan-id))
        (lsp-notify c "textDocument/didOpen"
                    (lsp-object "textDocument"
                                (lsp-object "uri" uri
                                            "languageId" lan-id
                                            "version" (lsp-number 1)
                                            "text" text))))))

(tm-define (lsp-document-change c uri text)
  (:synopsis "The document @uri now has @text: sent if it has changed")
  (with d (ahash-ref (lsp-client-ref c 'documents) uri)
    (when (and d (!= (cadr d) text) (> (sync-kind c) 0))
      (with v (+ (car d) 1)
        (ahash-set! (lsp-client-ref c 'documents) uri (list v text (caddr d)))
        (lsp-notify c "textDocument/didChange"
                    (lsp-object "textDocument"
                                (lsp-object "uri" uri
                                            "version" (lsp-number v))
                                "contentChanges"
                                (lsp-array (lsp-object "text" text))))))))

(tm-define (lsp-document-save c uri)
  (:synopsis "The document @uri has been saved")
  (let* ((d (ahash-ref (lsp-client-ref c 'documents) uri))
         (save (sync-option c "save")))
    (when (and d save (not (lsp-null? save))
               (not (== save '(json-boolean "false"))))
      (lsp-notify c "textDocument/didSave"
                  (if (lsp-true? (lsp-ref save "includeText"))
                      (lsp-object "textDocument" (text-document uri)
                                  "text" (cadr d))
                      (lsp-object "textDocument" (text-document uri)))))))

(tm-define (lsp-document-close c uri)
  (:synopsis "Closes the document @uri")
  (when (lsp-document-open? c uri)
    (ahash-remove! (lsp-client-ref c 'documents) uri)
    (lsp-notify c "textDocument/didClose"
                (lsp-object "textDocument" (text-document uri)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Diagnostics
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; uri -> the list of the diagnostics last published (strees)
(define diagnostics (make-ahash-table))

(tm-define (lsp-diagnostics uri)
  (:synopsis "The diagnostics last published for @uri")
  (or (ahash-ref diagnostics uri) '()))

(tm-define (lsp-diagnostics-changed uri)
  (:synopsis "Called when the diagnostics of @uri have been published")
  (noop))

(tm-define (lsp-diagnostic-severity d)
  (:synopsis "The severity of the diagnostic @d: error, warning, info, hint")
  (with n (lsp-number-value (lsp-ref d "severity"))
    (cond ((== n 2) 'warning)
          ((== n 3) 'info)
          ((== n 4) 'hint)
          (else 'error))))

(tm-define (lsp-diagnostic-message d)
  (:synopsis "The message of the diagnostic @d, in Cork")
  (utf8->cork (or (lsp-ref d "message") "")))

(tm-define (lsp-diagnostic-range d lines)
  (:synopsis "The start and end of @d in @lines, as (line offset line offset)")
  (append (lsp-position->line+offset lines (lsp-ref d "range" "start"))
          (lsp-position->line+offset lines (lsp-ref d "range" "end"))))

(lsp-notification-set! "textDocument/publishDiagnostics" 'diagnostics
  (lambda (c params)
    (with uri (lsp-ref params "uri")
      (when (string? uri)
        (ahash-set! diagnostics uri (lsp-items (lsp-ref params "diagnostics")))
        (lsp-diagnostics-changed uri)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Buffers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define root-markers
  '(".git" "compile_commands.json" "compile_flags.txt" "pyproject.toml"
    "setup.py" "package.json" "Cargo.toml" "go.mod" "build.sbt" "pom.xml"))

(tm-define (lsp-project-root path)
  (:synopsis "The directory of the project of the file at the system @path")
  (let* ((dir (url-head (system->url path)))
         (top dir))
    (let loop ((d dir) (i 0))
      (cond ((> i 64) (url->system top))
            ((list-or (map (lambda (m) (url-exists? (url-append d m)))
                           root-markers))
             (url->system d))
            (else
             (with up (url-head d)
               (if (or (== (url->system up) (url->system d))
                       (== (url->system up) ""))
                   (url->system top)
                   (loop up (+ i 1)))))))))

(tm-define (lsp-language-id lan path)
  (:synopsis "The languageId of a file at @path in the language @lan")
  (with suffix (locase-all (url-suffix (system->url path)))
    (cond ((and (== lan "cpp") (== suffix "c")) "c")
          ((== lan "javascript")
           (if (in? suffix '("ts" "mts" "cts")) "typescript" "javascript"))
          (else lan))))

(tm-define (lsp-buffer-language)
  (:synopsis "The language of the current buffer, if it is source code")
  ;; the format of a source file is its language (cpp, python...); the
  ;; environment of a file which has just been loaded is not merged yet
  (let* ((u (current-buffer))
         (f (and (not (url-rooted-tmfs? u)) (url-format u)))
         (lan (get-init "prog-language")))
    (cond ((and (string? f) (lsp-server-known? f)) f)
          ((and (== (get-init "mode") "prog") (string? lan)
                (!= lan "") (!= lan "verbatim"))
           lan)
          (else #f))))

(tm-define (lsp-buffer-path)
  (:synopsis "The system path of the file of the current buffer, or #f")
  (with u (current-buffer)
    (and (not (url-rooted-tmfs? u))
         (not (url-none? u))
         (url->system u))))

(tm-define (lsp-buffer-uri)
  (with p (lsp-buffer-path)
    (and p (lsp-path->uri p))))

(tm-define (lsp-buffer-lines)
  (:synopsis "The lines of the current buffer, in Cork")
  (lsp-lines (buffer-get-body (current-buffer))))

(tm-define (lsp-buffer-client)
  (:synopsis "The client of the server of the current buffer, or #f")
  (let ((lan (lsp-buffer-language))
        (p (lsp-buffer-path)))
    (and lan p (lsp-client-for lan (lsp-project-root p)))))

(tm-define (lsp-buffer-open)
  (:synopsis "Opens the current buffer on the server of its language")
  ;; the client, or #f when there is none
  (with c (lsp-buffer-client)
    (when c
      (lsp-document-open c (lsp-buffer-uri)
                         (lsp-language-id (lsp-buffer-language)
                                          (lsp-buffer-path))
                         (lsp-lines->text (lsp-buffer-lines))))
    c))

(tm-define (lsp-buffer-sync)
  (:synopsis "Gives the text of the current buffer to its server")
  (let ((c (lsp-buffer-client))
        (uri (lsp-buffer-uri)))
    (when (and c uri)
      (if (lsp-document-open? c uri)
          (lsp-document-change c uri (lsp-lines->text (lsp-buffer-lines)))
          (lsp-buffer-open)))))

(tm-define (lsp-buffer-close)
  (:synopsis "Closes the current buffer on its server")
  (let* ((lan (lsp-buffer-language))
         (p (lsp-buffer-path))
         (c (and lan p (lsp-client-find lan (lsp-project-root p))))
         (uri (lsp-buffer-uri)))
    (when (and c uri) (lsp-document-close c uri))))

(tm-define (lsp-cursor-position)
  (:synopsis "The position of the cursor in the current buffer, or #f")
  ;; in a source file, the cursor is at an offset of a line (paragraph)
  (let* ((p (cursor-path))
         (b (buffer-path))
         (r (and (list-starts? p b) (list-tail p (length b)))))
    (and r (= (length r) 2)
         (lsp-position (lsp-buffer-lines) (car r) (cadr r)))))
