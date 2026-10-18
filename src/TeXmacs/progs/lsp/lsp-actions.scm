;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : lsp-actions.scm
;; DESCRIPTION : the edits of language servers: formatting, renames, fixes
;; COPYRIGHT   : (C) 2026  Massimiliano Gubinelli
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; A server answers some requests with edits of the text: TextEdit (a range
;; and its new text) for the formatting, WorkspaceEdit (TextEdits for one
;; or more files) for a rename and the quick fixes (code actions), and it
;; may ask for one itself (workspace/applyEdit, after executeCommand).
;;
;; The edits of a file are applied to the lines of its buffer (Cork), from
;; the last one to the first, so that the ranges of the others stay valid;
;; the buffer is then changed only where its lines differ (the common lines
;; at the start and at the end are kept), which keeps the markup of the
;; other lines and the undo history short; the cursor is moved with the
;; text (lsp-move-through-edits), since its line may be replaced. A file which is
;; not open is loaded in a buffer (it is then modified, to be saved).

(texmacs-module (lsp lsp-actions)
  (:use (lsp lsp-base)
        (lsp lsp-client)
        (lsp lsp-documents)
        (lsp lsp-edit)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Edits of lines
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (edit-range lines e)
  ;; the start and the end of the TextEdit @e in @lines: (l1 o1 l2 o2)
  (append (lsp-position->line+offset lines (lsp-ref e "range" "start"))
          (lsp-position->line+offset lines (lsp-ref e "range" "end"))))

(define (before? a b)
  ;; is the start of the range @a before the one of @b?
  (or (< (car a) (car b))
      (and (== (car a) (car b)) (< (cadr a) (cadr b)))))

(define (split-lines s)
  ;; the lines of the Cork string @s (CR LF as LF)
  (map (lambda (l)
         (if (string-ends? l "\r") (substring l 0 (- (string-length l) 1)) l))
       (string-decompose s "\n")))

(define (sorted-edits lines edits)
  ;; the ranges and Cork texts of @edits, the last ones first. Of edits
  ;; with the same start, the last one of the server comes first: applied
  ;; from the end, they then come in the order of the server (two
  ;; insertions A and B give AB, as the protocol says). (The order of the
  ;; server is its index: the sort of S7 is not stable.)
  (let* ((rs (map (lambda (e i)
                    (list (edit-range lines e)
                          (source->cork (or (lsp-ref e "newText") ""))
                          i))
                  edits (.. 0 (length edits)))))
    (sort rs (lambda (a b)
               (or (before? (car b) (car a))
                   (and (not (before? (car a) (car b)))
                        (> (caddr a) (caddr b))))))))

(tm-define (lsp-apply-to-lines lines edits)
  (:synopsis "The Cork @lines once the TextEdits @edits are applied")
  ;; in one pass over @lines, from the first edit: the text between the
  ;; edits is copied, then the new texts (linear in the size of the file,
  ;; also for thousands of edits, as from a formatter)
  (let* ((v (list->vector (if (null? lines) '("") lines)))
         (n (vector-length v))
         (last-pos (list (- n 1) (string-length (vector-ref v (- n 1)))))
         (rs (reverse (sorted-edits v edits)))
         (acc '()))
    (define (clamp p)
      ;; a position in the text (a server may give one beyond its end)
      (if (>= (car p) n) last-pos
          (with s (vector-ref v (car p))
            (list (car p) (min (cadr p) (string-length s))))))
    (define (emit s) (set! acc (cons s acc)))
    (define (copy from to)
      ;; the text from the position @from to the position @to
      (let ((l1 (car from)) (o1 (cadr from)) (l2 (car to)) (o2 (cadr to)))
        (if (== l1 l2)
            (when (< o1 o2) (emit (substring (vector-ref v l1) o1 o2)))
            (let ((s1 (vector-ref v l1)))
              (emit (substring s1 o1 (string-length s1)))
              (do ((i (+ l1 1) (+ i 1))) ((> i l2))
                (emit "\n")
                (emit (if (< i l2) (vector-ref v i)
                          (substring (vector-ref v l2) 0 o2))))))))
    (let loop ((at '(0 0)) (rs rs))
      (if (null? rs)
          (copy at last-pos)
          (let* ((r (car (car rs)))
                 (start (clamp (list (car r) (cadr r))))
                 (end (clamp (list (caddr r) (cadddr r))))
                 ;; (overlapping edits, which a server should not send:
                 ;; the next one starts where the previous one ended)
                 (start* (if (before? start at) at start))
                 (end* (if (before? end start*) start* end)))
            (copy at start*)
            (emit (cadr (car rs)))
            (loop end* (cdr rs)))))
    (split-lines (apply string-append (reverse acc)))))

(define (move-one lo r text)
  ;; the line and offset @lo once the range @r is replaced by @text: the
  ;; same before the range, inside it the same if @text is still there
  ;; (else its end), shifted after
  (let ((l (car lo)) (o (cadr lo))
        (l1 (car r)) (o1 (cadr r)) (l2 (caddr r)) (o2 (cadddr r)))
    (if (before? lo r) lo
        (let* ((pieces (split-lines text))
               (k (length pieces))
               (last (string-length (list-ref pieces (- k 1))))
               (end-l (+ l1 (- k 1)))
               (end-o (if (= k 1) (+ o1 last) last)))
          (cond ((before? (list l2 o2) lo)
                 ;; after the range: shifted
                 (if (== l l2) (list end-l (+ end-o (- o o2)))
                     (list (+ l (- end-l l2)) o)))
                ((and (== l l1) (== end-l l1) (<= o end-o)) lo)
                (else (list end-l end-o)))))))

(tm-define (lsp-move-through-edits lines edits lo)
  (:synopsis "The line and offset @lo of @lines once @edits are applied")
  ;; (the edits are applied from the last one: those after @lo leave it,
  ;; those before shift it, and their ranges stay those of @lines)
  (let loop ((lo lo) (rs (sorted-edits (list->vector lines) edits)))
    (if (null? rs) lo
        (loop (move-one lo (car (car rs)) (cadr (car rs))) (cdr rs)))))

(define (common-prefix a b)
  (let loop ((a a) (b b) (n 0))
    (if (and (pair? a) (pair? b) (== (car a) (car b)))
        (loop (cdr a) (cdr b) (+ n 1))
        n)))

(tm-define (lsp-set-lines body lines)
  (:synopsis "Changes the document tree @body so that its lines are @lines")
  ;; the number of lines which have changed
  ;; (only the lines which differ, see above)
  (let* ((old (lsp-lines body))
         (ov (list->vector old)) (nv (list->vector lines))
         (n (length old)) (m (length lines))
         (p (common-prefix old lines))
         (s (min (common-prefix (reverse old) (reverse lines))
                 (- (min n m) p)))
         (old-mid (- n p s))
         (new-mid (- m p s))
         (both (min old-mid new-mid)))
    ;; (a line is replaced in its document: tree-assign! of the string
    ;; of a line did not reach the document which holds it)
    (do ((i 0 (+ i 1))) ((>= i both))
      (when (!= (vector-ref ov (+ p i)) (vector-ref nv (+ p i)))
        (tree-remove! body (+ p i) 1)
        (tree-insert! body (+ p i) (list (vector-ref nv (+ p i))))))
    (cond ((> new-mid old-mid)
           (tree-insert! body (+ p old-mid)
                         (sublist lines (+ p old-mid) (+ p new-mid))))
          ((> old-mid new-mid)
           (tree-remove! body (+ p new-mid) (- old-mid new-mid))))
    ;; (the number of lines replaced, removed or inserted)
    (max old-mid new-mid)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Edits of buffers and of the workspace
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (in-buffer u thunk)
  ;; @thunk in the buffer @u (loaded if needed), then back in the current one
  (let ((old (current-buffer)))
    (cond ((== u old) (noop))
          ((buffer-exists? u) (switch-to-buffer u))
          (else (load-buffer u)))
    (with r (and (== (current-buffer) u) (thunk))
      (when (and (!= old u) (buffer-exists? old))
        (switch-to-buffer old))
      r)))

(tm-define (lsp-apply-buffer-edits u edits)
  (:synopsis "Applies the TextEdits @edits to the buffer @u")
  ;; the number of lines which have changed, or #f
  (and (nnull? edits)
       (in-buffer u
         (lambda ()
           ;; (the cursor follows the text: its line may be replaced)
           (let* ((body (buffer-tree))
                  (old (lsp-lines body))
                  (lines (lsp-apply-to-lines old edits))
                  (b (buffer-path))
                  (p (cursor-path))
                  (lo (and (list-starts? p b)
                           (with r (list-tail p (length b))
                             (and (= (length r) 2) r))))
                  (n (lsp-set-lines body lines)))
             (when lo
               (let* ((lo2 (lsp-move-through-edits old edits lo))
                      (l (max 0 (min (car lo2) (- (length lines) 1))))
                      (o (max 0 (min (cadr lo2)
                                     (string-length (lsp-line lines l))))))
                 (go-to (append b (list l o)))))
             n)))))

(define (uri->buffer uri)
  ;; the buffer of @uri: the open one whose uri is the same (its path may be
  ;; written otherwise: c: for C:), or the url of the file
  (with key (lsp-uri-key uri)
    (or (list-find (buffer-list) (lambda (u) (== (lsp-buffer-uri u) key)))
        (with p (lsp-uri->path uri)
          (and p (system->url p))))))

(tm-define (lsp-workspace-edit-files w)
  (:synopsis "The files of the WorkspaceEdit @w, with their edits")
  ;; as a list of (uri edits): changes (a map) or documentChanges
  ;; (TextDocumentEdits; the creations, renames and deletions of files
  ;; are left out)
  (with dc (lsp-ref w "documentChanges")
    (if (func? dc 'tuple)
        (list-filter
         (map (lambda (x)
                (and (lsp-ref x "textDocument" "uri")
                     (list (lsp-ref x "textDocument" "uri")
                           (lsp-items (lsp-ref x "edits")))))
              (cdr dc))
         identity)
        (with ch (lsp-ref w "changes")
          (if (func? ch 'attr)
              (let loop ((l (cdr ch)) (r '()))
                (if (or (null? l) (null? (cdr l))) (reverse r)
                    (loop (cddr l)
                          (cons (list (car l) (lsp-items (cadr l))) r))))
              '())))))

(tm-define (lsp-apply-workspace-edit w)
  (:synopsis "Applies the WorkspaceEdit @w; the number of files changed")
  (with files (lsp-workspace-edit-files w)
    (length (list-filter
             (map (lambda (f)
                    (with u (uri->buffer (car f))
                      ;; (#f: the file could not be opened)
                      (and u (or (null? (cadr f))
                                 (lsp-apply-buffer-edits u (cadr f))))))
                  files)
             identity))))

(tm-define (lsp-workspace-edit-stale? c w)
  (:synopsis "Whether the WorkspaceEdit @w of @c is for texts changed since")
  ;; (the TextDocumentEdits give the version of their file which they are
  ;; for: the edits come later than the answer to a request, after an
  ;; executeCommand, and the buffer may have changed meanwhile, also when
  ;; its server was not given its text yet)
  (with dc (lsp-ref w "documentChanges")
    (and (func? dc 'tuple)
         (list-or
          (map (lambda (x)
                 (let* ((uri (lsp-ref x "textDocument" "uri"))
                        (v (lsp-number-value
                            (lsp-ref x "textDocument" "version")))
                        (key (and uri (lsp-uri-key uri)))
                        (mine (and key (lsp-document-version c key)))
                        (u (and uri (uri->buffer uri))))
                   (and v mine
                        (or (!= v mine)
                            (and u (buffer-exists? u)
                                 (!= (lsp-buffer-text u)
                                     (lsp-document-sent-text c key)))))))
               (cdr dc))))))

;; the edits which servers asked for (workspace/applyEdit), since the start:
;; an answer which was waited for while one came is for a former text
(define server-edits 0)

;; the server asks for edits (after a command of a code action); it is told
;; whether all its files could be changed
(lsp-server-request-set! "workspace/applyEdit"
  (lambda (c params)
    (let* ((w (or (lsp-ref params "edit") (lsp-object)))
           (n (length (lsp-workspace-edit-files w))))
      (if (lsp-workspace-edit-stale? c w)
          (begin
            (set-message "The edit of the server is for an older text" "")
            (lsp-object "applied" (lsp-boolean #f)
                        "failureReason" "the file has changed"))
          (with done (begin (set! server-edits (+ server-edits 1))
                            (lsp-apply-workspace-edit w))
            (lsp-buffer-sync)
            (if (== done n)
                (lsp-object "applied" (lsp-boolean #t))
                (lsp-object "applied" (lsp-boolean #f)
                            "failureReason" "a file could not be opened")))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Formatting
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (formatting-options)
  ;; (as the buffer is indented: by tabs, at its tab stops, or by spaces,
  ;; prog-edit.scm, which the editing of programs loads: it is not loaded
  ;; with this module, whose tests are loaded in all the suites)
  (if (and (defined? 'program-indent-tabs?) (program-indent-tabs?))
      (lsp-object "tabSize" (lsp-number (program-tab-width))
                  "insertSpaces" (lsp-boolean #f))
      (with n (get-preference "editor:verbatim:tabstop")
        (lsp-object "tabSize" (lsp-number (or (and (string? n) (string->number n))
                                              (and (number? n) n) 4))
                    "insertSpaces" (lsp-boolean #t)))))

(define (selection-range)
  ;; the range of the selection in the current buffer, or #f
  (and (selection-active-any?)
       (let* ((b (buffer-path))
              (p1 (selection-get-start))
              (p2 (selection-get-end))
              (r1 (and (list-starts? p1 b) (list-tail p1 (length b))))
              (r2 (and (list-starts? p2 b) (list-tail p2 (length b)))))
         (and r1 r2 (= (length r1) 2) (= (length r2) 2)
              (lsp-object "start" (lsp-buffer-position (car r1) (cadr r1))
                          "end" (lsp-buffer-position (car r2) (cadr r2)))))))

(tm-define (lsp-format)
  (:synopsis "Formats the selection, or the buffer, with the language server")
  (let* ((c (lsp-buffer-client*))
         (uri (lsp-buffer-uri))
         (range (selection-range))
         (method (if range "textDocument/rangeFormatting"
                     "textDocument/formatting"))
         (cap (if range "documentRangeFormattingProvider"
                  "documentFormattingProvider")))
    (cond ((not (and c uri))
           (set-message "No language server for this buffer" ""))
          ((not (lsp-client-capable? c cap))
           (set-message "The language server cannot format" ""))
          (else
           (lsp-buffer-sync)
           (let* ((edits server-edits)
                  (a (lsp-request-wait
                    c method
                    (apply lsp-object
                           `("textDocument" ,(lsp-object "uri" uri)
                             ,@(if range (list "range" range) '())
                             "options" ,(formatting-options)))
                    5000)))
             (cond ((!= (car a) 'result)
                    (set-message "The formatting failed" ""))
                   ((!= server-edits edits)
                    (set-message "The file has changed meanwhile" ""))
                   ((null? (lsp-items (cadr a)))
                    (set-message "Already formatted" ""))
                   (else
                    (lsp-apply-buffer-edits (current-buffer)
                                            (lsp-items (cadr a)))
                    (lsp-buffer-sync)
                    (set-message "Formatted" ""))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Renames
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (lsp-rename new-name)
  (:synopsis "Renames the symbol at the cursor everywhere")
  (:argument new-name "New name")
  (let ((c (lsp-buffer-client*))
        (uri (lsp-buffer-uri))
        (pos (lsp-cursor-position)))
    (cond ((not (and c uri pos))
           (set-message "No language server for this buffer" ""))
          ((not (lsp-client-capable? c "renameProvider"))
           (set-message "The language server cannot rename" ""))
          ((== new-name "") (noop))
          (else
           ;; (all the open files first: the edits are made for the texts
           ;; which the server has)
           (lsp-sync-pending)
           (lsp-buffer-sync)
           (let* ((edits server-edits)
                  (a (lsp-request-wait
                    c "textDocument/rename"
                    (lsp-object "textDocument" (lsp-object "uri" uri)
                                "position" pos
                                "newName" (cork->sourcecode new-name))
                    10000)))
             (cond ((== (car a) 'error)
                    (set-message (utf8->cork (or (lsp-ref (cadr a) "message")
                                                 "The rename failed"))
                                 ""))
                   ((!= (car a) 'result)
                    (set-message "The language server does not answer" ""))
                   ((lsp-null? (cadr a))
                    (set-message "Nothing to rename here" ""))
                   ((!= server-edits edits)
                    (set-message "The files have changed meanwhile" ""))
                   (else
                    (with n (lsp-apply-workspace-edit (cadr a))
                      (lsp-buffer-sync)
                      (set-message (string-append "Renamed in "
                                                  (number->string n)
                                                  (if (== n 1) " file"
                                                      " files"))
                                   "")))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Quick fixes (code actions)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; the client and the actions last proposed, for the page of the fixes,
;; with the buffer and its text then (the ranges of the fixes are those
;; of that text)
(define fixes-client #f)
(define fixes '())
(define fixes-buffer #f)
(define fixes-text #f)

(tm-define (lsp-run-action c a)
  (:synopsis "Runs the code action (or command) @a of the server of @c")
  ;; an action has an edit, or a command, or both (the edit first); a
  ;; bare Command has a command and arguments
  (with e (lsp-ref a "edit")
    (when e (lsp-apply-workspace-edit e)))
  (with cmd (or (and (string? (lsp-ref a "command")) a)
                (lsp-ref a "command"))
    (when (and cmd (string? (lsp-ref cmd "command")))
      (lsp-request c "workspace/executeCommand"
                   (lsp-object "command" (lsp-ref cmd "command")
                               "arguments" (or (lsp-ref cmd "arguments")
                                               (lsp-array)))
                   ignore)))
  (lsp-buffer-sync))

(tm-define (lsp-run-fix i)
  (:synopsis "Runs the fix number @i of the page of the fixes")
  (:secure #t)
  (when (and fixes-client (>= i 0) (< i (length fixes)))
    (with a (list-ref fixes i)
      ;; (the page goes, the buffer of the fix comes back)
      (when (url-rooted-tmfs? (current-buffer))
        (with u (current-buffer)
          (buffer-close u)))
      (cond ((not (and fixes-buffer (buffer-exists? fixes-buffer)))
             (set-message "The file of the fixes has been closed" ""))
            ((!= (lsp-buffer-text fixes-buffer) fixes-text)
             ;; (its ranges would no longer be those of the text)
             (switch-to-buffer fixes-buffer)
             (set-message "The file has changed: ask for the fixes again"
                          ""))
            (else
             (when (!= (current-buffer) fixes-buffer)
               (switch-to-buffer fixes-buffer))
             (lsp-run-action fixes-client a))))))

(define (action-title a)
  (utf8->cork (or (lsp-ref a "title") "(untitled)")))

(tm-define (lsp-code-actions-at-cursor)
  ;; the code actions of the server at the cursor (the fixes of the
  ;; diagnostics there first), or #f
  (let* ((c (lsp-buffer-client*))
         (uri (lsp-buffer-uri))
         (pos (lsp-cursor-position)))
    (and c uri pos
         (lsp-client-capable? c "codeActionProvider")
         (begin
           (lsp-buffer-sync)
           (let* ((diags (list-filter
                          (lsp-diagnostics uri)
                          (lambda (d)
                            (let ((l (lsp-number-value (lsp-ref d "range"
                                                                "start" "line")))
                                  (l2 (lsp-number-value (lsp-ref d "range"
                                                                 "end" "line")))
                                  (cl (lsp-number-value (lsp-ref pos "line"))))
                              (and l l2 cl (<= l cl) (<= cl l2))))))
                  (a (lsp-request-wait
                      c "textDocument/codeAction"
                      (lsp-object "textDocument" (lsp-object "uri" uri)
                                  "range" (lsp-object "start" pos "end" pos)
                                  "context" (lsp-object
                                             "diagnostics"
                                             (lsp-list->array diags)))
                      5000)))
             (and (== (car a) 'result)
                  (list c (lsp-items (cadr a)))))))))

(tm-define (lsp-quick-fix)
  (:synopsis "Proposes the fixes of the language server at the cursor")
  ;; one is run at once; several are listed on a page, to be clicked
  (with r (lsp-code-actions-at-cursor)
    (cond ((not r) (set-message "No fixes here" ""))
          ((null? (cadr r)) (set-message "No fixes here" ""))
          ((null? (cdr (cadr r)))
           (lsp-run-action (car r) (car (cadr r)))
           (set-message (action-title (car (cadr r))) ""))
          (else
           (set! fixes-client (car r))
           (set! fixes (cadr r))
           (set! fixes-buffer (current-buffer))
           (set! fixes-text (lsp-buffer-text))
           (open-auxiliary
            "Quick fixes"
            `(document
              (style (tuple "generic"))
              (body
               (document
                (strong "Fixes proposed by the language server")
                ,@(let loop ((l fixes) (i 0) (acc '()))
                    (if (null? l) (reverse acc)
                        (loop (cdr l) (+ i 1)
                              (cons `(action ,(action-title (car l))
                                             ,(string-append
                                               "(lsp-run-fix "
                                               (number->string i) ")"))
                                    acc))))))))))))
