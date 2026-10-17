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
           ;; (as other clients do: the saves too, without their text)
           (cond ((== key "openClose") (lsp-boolean (> (lsp-number-value s) 0)))
                 ((== key "change") s)
                 ((== key "save") (lsp-object))
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
        (when (open-close? c)
          (lsp-notify c "textDocument/didOpen"
                      (lsp-object "textDocument"
                                  (lsp-object "uri" uri
                                              "languageId" lan-id
                                              "version" (lsp-number 1)
                                              "text" text)))))))

(define (open-close? c)
  ;; does the server take didOpen and didClose? (yes unless it says no)
  (!= (sync-option c "openClose") '(json-boolean "false")))

;; Incremental changes: the old and the new text have a common start and a
;; common end (bytes of utf8, at the boundaries of characters); what lies
;; between them in the old text is replaced by what lies between them in
;; the new one, and the range is given as positions (line, UTF-16 units)

(define (continuation? s i)
  ;; is the byte @i of the utf8 string @s inside a character?
  (and (< i (string-length s))
       (with c (char->integer (string-ref s i))
         (and (>= c 128) (< c 192)))))

(define block 4096)

(define (common-start a b)
  ;; (by blocks first: a byte loop over a long file is slow in Scheme)
  (let* ((n (min (string-length a) (string-length b)))
         (i0 (let blocks ((i 0))
               (if (and (<= (+ i block) n)
                        (== (substring a i (+ i block))
                            (substring b i (+ i block))))
                   (blocks (+ i block))
                   i))))
    (let loop ((i i0))
      (if (and (< i n) (== (string-ref a i) (string-ref b i)))
          (loop (+ i 1))
          ;; (back to the start of a character)
          (let back ((i i))
            (if (and (> i 0) (or (continuation? a i) (continuation? b i)))
                (back (- i 1))
                i))))))

(define (common-end a b start)
  ;; the length of the common end of @a and @b, after @start in both
  (let* ((na (string-length a)) (nb (string-length b))
         (n (- (min na nb) start))
         (k0 (let blocks ((k 0))
               (if (and (<= (+ k block) n)
                        (== (substring a (- na k block) (- na k))
                            (substring b (- nb k block) (- nb k))))
                   (blocks (+ k block))
                   k))))
    (let loop ((k k0))
      (if (and (< k n)
               (== (string-ref a (- na k 1)) (string-ref b (- nb k 1))))
          (loop (+ k 1))
          ;; (the end begins at the start of a character)
          (let fwd ((k k))
            (if (and (> k 0) (continuation? a (- na k)))
                (fwd (- k 1))
                k))))))

(define (line+start s i)
  ;; the line of the byte @i of the utf8 @s, and the start of that line
  ;; (from line to line: string-search-forwards is quick)
  (let loop ((line 0) (start 0))
    (with j (string-search-forwards "\n" start s)
      (if (or (< j 0) (>= j i)) (list line start)
          (loop (+ line 1) (+ j 1))))))

(define (offset->position s i)
  ;; the position (line, UTF-16 units) of the byte @i of the utf8 @s
  (with ls (line+start s i)
    (lsp-object "line" (lsp-number (car ls))
                "character" (lsp-number
                             (lsp-utf16-length (substring s (cadr ls) i))))))

(define (count-lines s)
  ;; the number of LF in @s
  (let loop ((n 0) (start 0))
    (with j (string-search-forwards "\n" start s)
      (if (< j 0) n (loop (+ n 1) (+ j 1))))))

(tm-define (lsp-text-change old new)
  (:synopsis "The incremental change (a TextDocumentContentChangeEvent)")
  (let* ((a (common-start old new))
         (k (common-end old new a)))
    (lsp-object "range"
                (lsp-object "start" (offset->position old a)
                            "end" (offset->position old
                                                    (- (string-length old) k)))
                "text" (substring new a (- (string-length new) k)))))

(tm-define (lsp-text-lines-change old new)
  (:synopsis "The lines changed from @old to @new: (first last added)")
  ;; the lines first to last of @old become added + 1 lines
  (let* ((a (common-start old new))
         (k (common-end old new a))
         (l1 (car (line+start old a)))
         (l2 (car (line+start old (- (string-length old) k)))))
    (list l1 l2 (count-lines (substring new a (- (string-length new) k))))))

(tm-define (lsp-document-lines-changed uri first last added)
  (:synopsis "The lines @first to @last of @uri became @added + 1 lines")
  ;; (the diagnostics which are shown follow them: see lsp-edit)
  (noop))

(tm-define (lsp-document-change c uri text)
  (:synopsis "The document @uri now has @text: sent if it has changed")
  ;; the change only, when the server takes them (kind 2), else the text
  (with d (ahash-ref (lsp-client-ref c 'documents) uri)
    (when (and d (!= (cadr d) text) (> (sync-kind c) 0))
      (with v (+ (car d) 1)
        (apply lsp-document-lines-changed uri
               (lsp-text-lines-change (cadr d) text))
        (ahash-set! (lsp-client-ref c 'documents) uri (list v text (caddr d)))
        (lsp-notify c "textDocument/didChange"
                    (lsp-object "textDocument"
                                (lsp-object "uri" uri
                                            "version" (lsp-number v))
                                "contentChanges"
                                (lsp-array
                                 (if (== (sync-kind c) 2)
                                     (lsp-text-change (cadr d) text)
                                     (lsp-object "text" text)))))))))

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
    (lsp-forget-diagnostics uri)
    (when (open-close? c)
      (lsp-notify c "textDocument/didClose"
                  (lsp-object "textDocument" (text-document uri))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Diagnostics
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; uri -> the list of the diagnostics last published (strees)
(define diagnostics (make-ahash-table))

(define (uri-key uri)
  ;; @uri written as we write it: a server may write the same file
  ;; otherwise (%c3%a9 for %C3%A9, %3A for :)
  (with p (lsp-uri->path uri)
    (if p (lsp-path->uri p) uri)))

(tm-define (lsp-diagnostics uri)
  (:synopsis "The diagnostics last published for @uri")
  (or (ahash-ref diagnostics (uri-key uri)) '()))

(tm-define (lsp-forget-diagnostics uri)
  (:synopsis "The diagnostics of @uri are forgotten (its server has ended)")
  (with key (uri-key uri)
    (when (ahash-ref diagnostics key)
      (ahash-remove! diagnostics key)
      (lsp-diagnostics-changed key))))

(tm-define (lsp-client-status-changed c)
  ;; the diagnostics of a server which has ended are forgotten (they
  ;; would stay underlined, also after a restart)
  (former c)
  (when (== (lsp-client-state c) 'dead)
    (for (uri (map car (ahash-table->list (lsp-client-ref c 'documents))))
      (lsp-forget-diagnostics uri))))

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
        (with key (uri-key uri)
          (ahash-set! diagnostics key
                      (lsp-items (lsp-ref params "diagnostics")))
          (lsp-diagnostics-changed key))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Buffers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define root-markers
  '(".git" "compile_commands.json" "compile_flags.txt" "pyproject.toml"
    "setup.py" "package.json" "Cargo.toml" "go.mod" "build.sbt" "pom.xml"))

(define project-roots (make-ahash-table))

(tm-define (lsp-project-root path)
  (:synopsis "The directory of the project of the file at the system @path")
  ;; (kept for each directory: it is asked for at each key, and looks for
  ;; ten files in each directory up to the root)
  (with dir (url->system (url-head (system->url path)))
    (or (ahash-ref project-roots dir)
        (with r (project-root path)
          (ahash-set! project-roots dir r)
          r))))

(tm-define (lsp-forget-project-roots)
  (:synopsis "The projects of the files are looked for again")
  (set! project-roots (make-ahash-table)))

(define (project-root path)
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

(tm-define (lsp-buffer-text . opt)
  (:synopsis "The text of the buffer, in utf8, for its server")
  ;; (built in C++: lists of the lines of a long file would fill the heap)
  (lsp-document-text (buffer-get-body (the-buffer opt))))

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
                         (apply lsp-buffer-text opt)))
    c))

(tm-define (lsp-buffer-sync . opt)
  (:synopsis "Gives the text of the buffer to its server (opened if needed)")
  (let ((c (apply lsp-buffer-client opt))
        (uri (apply lsp-buffer-uri opt)))
    (when (and c uri (lsp-client-alive? c))
      (if (lsp-document-open? c uri)
          (lsp-document-change c uri
                               (apply lsp-buffer-text opt))
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
    (and r (= (length r) 2) (lsp-buffer-position (car r) (cadr r)))))

;; (the line of a position only: all the lines of a long file, at each
;; request, would make a list as long as the file)

(tm-define (lsp-buffer-line i . opt)
  (:synopsis "The line @i of the buffer (current by default), in Cork")
  (let* ((body (buffer-get-body (the-buffer opt)))
         (t (cond ((and (tree-is? body 'document) (>= i 0)
                        (< i (tree-arity body)))
                   (tree-ref body i))
                  ((and (== i 0) (not (tree-is? body 'document))) body)
                  (else #f))))
    (if t (car (lsp-lines t)) "")))

(tm-define (lsp-buffer-position line offset)
  (:synopsis "The position at @offset of the line @line of the buffer")
  (lsp-object "line" (lsp-number line)
              "character" (lsp-number
                           (lsp-offset->character (lsp-buffer-line line)
                                                  offset))))

(tm-define (lsp-buffer-line+offset pos)
  (:synopsis "The line and the offset in the buffer of the position @pos")
  (let* ((line (or (lsp-number-value (lsp-ref pos "line")) 0))
         (ch (or (lsp-number-value (lsp-ref pos "character")) 0)))
    (list line (lsp-character->offset (lsp-buffer-line line) ch))))
