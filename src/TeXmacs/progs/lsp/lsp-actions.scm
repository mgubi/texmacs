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
;; at the start and at the end are kept), which keeps the cursor, the
;; markup of the other lines and the undo history short. A file which is
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

(define (line-at lines i)
  (if (and (>= i 0) (< i (length lines))) (list-ref lines i) ""))

(define (apply-one lines r text)
  ;; @lines with the range @r replaced by the Cork @text
  (let* ((l1 (car r)) (o1 (cadr r)) (l2 (caddr r)) (o2 (cadddr r))
         (n (length lines))
         (s1 (line-at lines l1))
         (s2 (line-at lines l2))
         (head (substring s1 0 (min o1 (string-length s1))))
         (tail (substring s2 (min o2 (string-length s2)) (string-length s2)))
         (mid (split-lines (string-append head text tail))))
    (append (sublist lines 0 (min l1 n))
            mid
            (if (< (+ l2 1) n) (list-tail lines (+ l2 1)) '()))))

(tm-define (lsp-apply-to-lines lines edits)
  (:synopsis "The Cork @lines once the TextEdits @edits are applied")
  (let* ((rs (map (lambda (e)
                    (list (edit-range lines e)
                          (utf8->cork (or (lsp-ref e "newText") ""))))
                  edits))
         ;; the last ones first (sort is stable: equal starts keep the
         ;; order of the server, which is then reversed by the insertion)
         (sorted (sort rs (lambda (a b) (before? (car b) (car a))))))
    (let loop ((l lines) (rs sorted))
      (if (null? rs) l
          (loop (apply-one l (car (car rs)) (cadr (car rs))) (cdr rs))))))

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
      (when (!= (list-ref old (+ p i)) (list-ref lines (+ p i)))
        (tree-remove! body (+ p i) 1)
        (tree-insert! body (+ p i) (list (list-ref lines (+ p i))))))
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
           (let* ((body (buffer-tree))
                  (lines (lsp-apply-to-lines (lsp-lines body) edits)))
             (lsp-set-lines body lines))))))

(define (uri->buffer uri)
  (with p (lsp-uri->path uri)
    (and p (system->url p))))

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
    (for (f files)
      (with u (uri->buffer (car f))
        (when u (lsp-apply-buffer-edits u (cadr f)))))
    (length files)))

;; the server asks for edits (after a command of a code action)
(lsp-server-request-set! "workspace/applyEdit"
  (lambda (c params)
    (lsp-apply-workspace-edit (or (lsp-ref params "edit") (lsp-object)))
    (lsp-object "applied" (lsp-boolean #t))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Formatting
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (formatting-options)
  (with n (get-preference "editor:verbatim:tabstop")
    (lsp-object "tabSize" (lsp-number (or (and (string? n) (string->number n))
                                          (and (number? n) n) 4))
                "insertSpaces" (lsp-boolean #t))))

(define (selection-range)
  ;; the range of the selection in the current buffer, or #f
  (and (selection-active-any?)
       (let* ((b (buffer-path))
              (p1 (selection-get-start))
              (p2 (selection-get-end))
              (r1 (and (list-starts? p1 b) (list-tail p1 (length b))))
              (r2 (and (list-starts? p2 b) (list-tail p2 (length b)))))
         (and r1 r2 (= (length r1) 2) (= (length r2) 2)
              (with lines (lsp-buffer-lines)
                (lsp-object "start" (lsp-position lines (car r1) (cadr r1))
                            "end" (lsp-position lines (car r2) (cadr r2))))))))

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
          ((lsp-null? (lsp-ref (lsp-client-capabilities c) cap))
           (set-message "The language server cannot format" ""))
          (else
           (lsp-buffer-sync)
           (with a (lsp-request-wait
                    c method
                    (apply lsp-object
                           `("textDocument" ,(lsp-object "uri" uri)
                             ,@(if range (list "range" range) '())
                             "options" ,(formatting-options)))
                    5000)
             (cond ((!= (car a) 'result)
                    (set-message "The formatting failed" ""))
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
          ((lsp-null? (lsp-ref (lsp-client-capabilities c) "renameProvider"))
           (set-message "The language server cannot rename" ""))
          ((== new-name "") (noop))
          (else
           (lsp-buffer-sync)
           (with a (lsp-request-wait
                    c "textDocument/rename"
                    (lsp-object "textDocument" (lsp-object "uri" uri)
                                "position" pos
                                "newName" (cork->utf8 new-name))
                    10000)
             (cond ((== (car a) 'error)
                    (set-message (utf8->cork (or (lsp-ref (cadr a) "message")
                                                 "The rename failed"))
                                 ""))
                   ((!= (car a) 'result)
                    (set-message "The language server does not answer" ""))
                   ((lsp-null? (cadr a))
                    (set-message "Nothing to rename here" ""))
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

;; the client and the actions last proposed, for the page of the fixes
(define fixes-client #f)
(define fixes '())

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
      (lsp-run-action fixes-client a))))

(define (action-title a)
  (utf8->cork (or (lsp-ref a "title") "(untitled)")))

(tm-define (lsp-code-actions-at-cursor)
  ;; the code actions of the server at the cursor (the fixes of the
  ;; diagnostics there first), or #f
  (let* ((c (lsp-buffer-client*))
         (uri (lsp-buffer-uri))
         (pos (lsp-cursor-position)))
    (and c uri pos
         (not (lsp-null? (lsp-ref (lsp-client-capabilities c)
                                  "codeActionProvider")))
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
