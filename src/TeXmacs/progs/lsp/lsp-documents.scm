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

(define (the-buffer opt)
  (if (pair? opt) (car opt) (current-buffer)))

(tm-define (lsp-buffer-language . opt)
  (:synopsis "The language of the buffer (current by default), if it is code")
  ;; the format of a source file is its language (cpp, python...); the
  ;; environment of a file which has just been loaded is not merged yet
  (let* ((u (the-buffer opt))
         (f (and (not (url-rooted-tmfs? u)) (url-format u))))
    (cond ((and (string? f) (lsp-server-known? f)) f)
          ((and (== u (current-buffer)) (== (get-init "mode") "prog"))
           (with lan (get-init "prog-language")
             (and (string? lan) (!= lan "") (!= lan "verbatim") lan)))
          (else #f))))

(tm-define (lsp-buffer-path . opt)
  (:synopsis "The system path of the file of the buffer, or #f")
  (with u (the-buffer opt)
    (and (not (url-rooted-tmfs? u))
         (not (url-none? u))
         (url->system u))))

(tm-define (lsp-buffer-uri . opt)
  (with p (apply lsp-buffer-path opt)
    (and p (lsp-path->uri p))))

(tm-define (lsp-buffer-lines . opt)
  (:synopsis "The lines of the buffer, in Cork")
  (lsp-lines (buffer-get-body (the-buffer opt))))

(tm-define (lsp-buffer-client . opt)
  (:synopsis "The client of the server of the buffer (started if needed)")
  (let ((lan (apply lsp-buffer-language opt))
        (p (apply lsp-buffer-path opt)))
    (and lan p (lsp-client-for lan (lsp-project-root p)))))

(tm-define (lsp-buffer-client* . opt)
  (:synopsis "The running client of the server of the buffer, or #f")
  (let ((lan (apply lsp-buffer-language opt))
        (p (apply lsp-buffer-path opt)))
    (and lan p (lsp-client-find lan (lsp-project-root p)))))

(tm-define (lsp-buffer-open . opt)
  (:synopsis "Opens the buffer on the server of its language")
  ;; the client, or #f when there is none
  (with c (apply lsp-buffer-client opt)
    (when (and c (lsp-client-alive? c))
      (lsp-document-open c (apply lsp-buffer-uri opt)
                         (lsp-language-id (apply lsp-buffer-language opt)
                                          (apply lsp-buffer-path opt))
                         (lsp-lines->text (apply lsp-buffer-lines opt))))
    c))

(tm-define (lsp-buffer-sync . opt)
  (:synopsis "Gives the text of the buffer to its server (opened if needed)")
  (let ((c (apply lsp-buffer-client opt))
        (uri (apply lsp-buffer-uri opt)))
    (when (and c uri (lsp-client-alive? c))
      (if (lsp-document-open? c uri)
          (lsp-document-change c uri
                               (lsp-lines->text (apply lsp-buffer-lines opt)))
          (apply lsp-buffer-open opt)))
    c))

(tm-define (lsp-buffer-close . opt)
  (:synopsis "Closes the buffer on its server")
  (let ((c (apply lsp-buffer-client* opt))
        (uri (apply lsp-buffer-uri opt)))
    (when (and c uri) (lsp-document-close c uri))))

(tm-define (lsp-cursor-position)
  (:synopsis "The position of the cursor in the current buffer, or #f")
  ;; in a source file, the cursor is at an offset of a line (paragraph)
  (let* ((p (cursor-path))
         (b (buffer-path))
         (r (and (list-starts? p b) (list-tail p (length b)))))
    (and r (= (length r) 2)
         (lsp-position (lsp-buffer-lines) (car r) (cadr r)))))
