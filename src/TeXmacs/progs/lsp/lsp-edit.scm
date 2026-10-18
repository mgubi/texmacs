;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : lsp-edit.scm
;; DESCRIPTION : language servers in the editor
;; COPYRIGHT   : (C) 2026  Massimiliano Gubinelli
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; When the preference "lsp" is on, a source file whose language has a
;; server (lsp-server-set!) is followed by that server: the editor notifies
;; the changes of a buffer (notify-buffer-changed, after the typesetting),
;; and its text is given to the server once the user has paused (and when
;; it is loaded, the first notification). The diagnostics of the server are
;; underlined (the alt selections code-errors, code-warnings and code-infos,
;; drawn by the editor), the message of the one at the cursor is shown on
;; the status bar. Completion (tab after a word, or a . -> ::), information
;; at the cursor (hover), definitions and the diagnostics are reached by the
;; menu Tools > Language servers and the keys of lsp-kbd.
;;
;; The positions of the diagnostics are those of the text which the server
;; was given: they are shown as they are until the next ones come (the
;; editor leaves out those which do not exist any more).

(texmacs-module (lsp lsp-edit)
  (:use (lsp lsp-base)
        (lsp lsp-client)
        (lsp lsp-documents)
        (code code-navigate)
        (code code-diagnostics)
        (code code-complete)))

(tm-define (lsp-on?)
  (:synopsis "Do language servers follow the source files?")
  (!= (get-preference "lsp") "off"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Following the buffers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; buffer name -> #t when a server follows the buffer, no when none does
(define followed (make-ahash-table))
;; buffer name -> buffer, whose text the server does not have yet
(define pending (make-ahash-table))
(define scheduled? #f)

(define (name->buffer name)
  (list-find (buffer-list) (lambda (b) (== (url->string b) name))))

(define (follow? name u)
  ;; (a source file of a language with a server; a document which is a
  ;; file, for its code blocks, when they are followed: lsp-blocks.scm)
  (with r (ahash-ref followed name)
    (cond ((== r #t) #t)
          ((and (lsp-buffer-path u) (not (lsp-source-buffer? u)))
           (and (lsp-blocks-on?)
                (begin (ahash-set! followed name #t) #t)))
          (r #f)
          (else
           (let* ((lan (and (lsp-buffer-path u) (lsp-buffer-language u)))
                  (ok (and lan (lsp-server-command lan) #t)))
             (ahash-set! followed name (if ok #t 'no))
             ok)))))

(define (schedule-sync)
  (when (not scheduled?)
    (set! scheduled? #t)
    (delayed
      (:idle 300)
      (set! scheduled? #f)
      (lsp-sync-pending))))

(tm-define (lsp-sync-pending)
  (:synopsis "Gives the servers the texts of the buffers which have changed")
  (with l (map cdr (ahash-table->list pending))
    (set! pending (make-ahash-table))
    (for (u l)
      (when (buffer-exists? u)
        (lsp-buffer-sync u)))))

(tm-define (lsp-follow-buffer u)
  (:synopsis "Gives the text of the buffer @u to its server, after a pause")
  (with name (url->string u)
    (when (and (lsp-on?) (follow? name u))
      (ahash-set! pending name u)
      (schedule-sync))))

(tm-define (notify-buffer-changed name)
  (former name)
  (when (lsp-on?)
    (with u (name->buffer name)
      (when u (lsp-follow-buffer u))
      ;; (the underlines of the code blocks are drawn at their places: a
      ;; change outside of them moves them)
      (when (and u (== u (current-buffer)) (not (lsp-source-buffer? u))
                 (== (ahash-ref followed name) #t)
                 (nnull? (buffer-uris)))
        (draw-diagnostics)))))

(tm-define (notify-buffer-closed name)
  ;; the buffer is closed, or renamed (save as): its document is closed on
  ;; the server (the new name is followed when it is saved or changed)
  (former name)
  (when (== (ahash-ref followed name) #t)
    (ahash-remove! followed name)
    (ahash-remove! pending name)
    (lsp-buffer-close (string->url name))))

(tm-define (version-notify-saved name)
  ;; the server gets the text which has been saved, then didSave
  (former name)
  (when (lsp-on?)
    (with key (url->string name)
      (when (and (buffer-exists? name) (follow? key name))
        (ahash-remove! pending key)
        ;; (a document: all its code blocks are looked for again)
        (lsp-blocks-rescan name)
        (with c (lsp-buffer-sync name)
          (when (and c (lsp-client-alive? c))
            (lsp-document-save c (lsp-buffer-uri name))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Diagnostics
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; uri -> the diagnostics shown, as (severity line offset line offset
;; message), in the lines of the buffer when they were shown
(define shown (make-ahash-table))
;; uri -> #t when diagnostics have come which are not shown yet
(define stale (make-ahash-table))

(define (current-uri)
  (and (lsp-on?) (lsp-buffer-uri)))

(define (diagnostic-item d lines)
  ;; the diagnostic @d in @lines (a vector), or #f if it is beyond them;
  ;; an empty range covers a character
  (let* ((n (vector-length lines))
         (r (lsp-diagnostic-range d lines))
         (l1 (car r)) (o1 (cadr r))
         (l2 (min (caddr r) (- n 1)))
         (o2 (if (< (caddr r) n) (cadddr r)
                 (string-length (vector-ref lines (- n 1))))))
    (and (< l1 n) (>= l1 0)
         (with len (string-length (vector-ref lines l1))
           (when (and (== l1 l2) (<= o2 o1))
             (set! o2 (min len (+ o1 1)))
             (when (<= o2 o1) (set! o1 (max 0 (- o2 1)))))
           (list (with s (lsp-diagnostic-severity d)
                   (if (== s 'hint) 'info s))
                 l1 o1 l2 o2
                 (lsp-diagnostic-message d))))))

(define lsp-max-diagnostics 1000)

(define (most-severe-first ds)
  ;; @ds, the errors first, then the warnings, then the others, each in
  ;; their order (those which are kept when there are too many)
  (with sev (lambda (d) (or (lsp-number-value (lsp-ref d "severity")) 1))
    (append (list-filter ds (lambda (d) (<= (sev d) 1)))
            (list-filter ds (lambda (d) (== (sev d) 2)))
            (list-filter ds (lambda (d) (> (sev d) 2))))))

(define (lines-path root)
  ;; the place of an offset of a line of the document of lines @root
  (lambda (l o) (with b (tree->path root) (and b (append b (list l o))))))

(define (buffer-uris)
  ;; (uri root path-of) of the code of the current buffer which servers
  ;; see, its lines and the place in the buffer of an offset of a line: the
  ;; buffer of a source file, or the code blocks of a document (each one,
  ;; or those of a language together, a unit)
  (if (lsp-source-buffer?)
      (with uri (lsp-buffer-uri (current-buffer))
        (if uri (list (list uri (buffer-get-body (current-buffer))
                            (lines-path (buffer-get-body (current-buffer)))))
            '()))
      (with doc (lsp-buffer-path (current-buffer))
        (if (not doc) '()
            (append
             (map (lambda (un)
                    ;; (the diagnostics: placed as the server saw the blocks)
                    (list (lsp-path->uri (lsp-unit-path un)) (lsp-unit-lines un)
                          (lambda (l o) (lsp-unit-path-of un l o 'synced))))
                  (lsp-units-of doc))
             (map (lambda (e) (list (lsp-path->uri (caddr e)) (car e)
                                    (lines-path (car e))))
                  (list-filter (lsp-block-entries doc)
                               (lambda (e) (not (lsp-unit-of-path (caddr e)))))))))))

(define (in-current-buffer? uri)
  (and (lsp-on?) uri (assoc uri (buffer-uris)) #t))

(define (show-uri uri root)
  ;; the diagnostics of @uri, in the lines @root, are kept as shown
  (let* ((lines (list->vector (lsp-lines root)))
         ;; (at most lsp-max-diagnostics: a file with thousands of
         ;; warnings would make each redraw slow)
         (all (lsp-diagnostics uri))
         (ds (if (> (length all) lsp-max-diagnostics)
                 (sublist (most-severe-first all) 0 lsp-max-diagnostics)
                 all)))
    (ahash-remove! stale uri)
    (ahash-set! shown uri
                (list-filter (map (lambda (d) (diagnostic-item d lines)) ds)
                             identity))))

(tm-define (lsp-show-diagnostics)
  (:synopsis "Shows the diagnostics of the current buffer")
  ;; (of its code blocks, in a document)
  (for (x (buffer-uris)) (show-uri (car x) (cadr x)))
  (draw-diagnostics))

(define (draw-diagnostics)
  ;; (as the diagnostics of the source server of the core, with those of
  ;; the other sources: progs/code/code-diagnostics.scm; each code at the
  ;; place of its lines)
  (code-set-diagnostics
   'server
   (append-map
    (lambda (x)
      (let ((path-of (caddr x))
            (l (or (ahash-ref shown (car x)) '())))
        (list-filter
         (map (lambda (x)
                (let ((p1 (path-of (list-ref x 1) (list-ref x 2)))
                      (p2 (path-of (list-ref x 3) (list-ref x 4))))
                  ;; (in a unit, a diagnostic over two blocks at its start)
                  (and p1 (list (car x) p1 (or p2 p1) (list-ref x 5)))))
              l)
         identity)))
    (buffer-uris))))

(tm-define (lsp-document-lines-changed uri first last added)
  ;; the diagnostics shown follow the lines, until the server sends new
  ;; ones: those after the change are moved, those in it are dropped (an
  ;; edit removes the underlines; F8 and the messages at the cursor use
  ;; these positions)
  (former uri first last added)
  (with l (ahash-ref shown uri)
    (when l
      (let* ((delta (- added (- last first)))
             (move (lambda (x)
                     (cond ((< (list-ref x 3) first) x)
                           ((> (list-ref x 1) last)
                            (list (car x)
                                  (+ (list-ref x 1) delta) (list-ref x 2)
                                  (+ (list-ref x 3) delta) (list-ref x 4)
                                  (list-ref x 5)))
                           (else #f))))
             (l2 (list-filter (map move l) identity)))
        (ahash-set! shown uri l2)
        (when (in-current-buffer? uri) (draw-diagnostics))))))

(tm-define (lsp-hide-diagnostics)
  (code-clear-diagnostics 'server))

(tm-define (lsp-diagnostics-changed uri)
  (former uri)
  (ahash-set! stale uri #t)
  (when (in-current-buffer? uri)
    (lsp-show-diagnostics)
    (lsp-status-refresh)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The status, for the focus bar (lsp-kbd)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; The focus bar is made again at each key: the language and the project of
;; a buffer (the project needs the files around it) are kept, buffer name
;; -> (language root)
(define buffer-servers (make-ahash-table))
(define refresh-scheduled? #f)

(define (buffer-server u)
  ;; (in a document, the server of the code block at the cursor)
  (with name (url->string u)
    (or (and (lsp-source-buffer? u) (ahash-ref buffer-servers name))
        (and (not (lsp-source-buffer? u))
             (let* ((lan (lsp-buffer-language))
                    (p (lsp-buffer-path)))
               (list lan (and lan p (lsp-project-root p)))))
        (let* ((lan (lsp-buffer-language u))
               (p (lsp-buffer-path u))
               (r (list lan (and lan p (lsp-project-root p)))))
          (ahash-set! buffer-servers name r)
          r))))

(tm-define (lsp-status-refresh)
  (:synopsis "The focus bar shows the new status, soon")
  ;; (once for many changes: a server reports its progress often)
  (when (not refresh-scheduled?)
    (set! refresh-scheduled? #t)
    (delayed
      (:idle 100)
      (set! refresh-scheduled? #f)
      (update-menus))))

(tm-define (lsp-client-status-changed c)
  (former c)
  (lsp-status-refresh))

(tm-define (lsp-focus-status)
  (:synopsis "The status of the server of the current buffer, or #f")
  ;; (name state progress errors warnings): the state is starting,
  ;; ready, stopped or none (not started yet), progress a string or #f
  (and (lsp-on?) (followed-buffer?)
       (let* ((u (current-buffer))
              (ls (buffer-server u))
              (c (and (car ls) (lsp-client-entry (car ls) (cadr ls))))
              (st (and c (lsp-client-state c)))
              (uri (lsp-buffer-uri))
              (l (or (and uri (ahash-ref shown uri)) '())))
         (list (if c (command-name (lsp-client-ref c 'name))
                   (command-name (or (lsp-server-command* (car ls)) "")))
               (cond ((not c) 'none)
                     ((in? st '(new initializing)) 'starting)
                     ((== st 'running) 'ready)
                     (else 'stopped))
               (and c (lsp-client-progress c))
               (length (list-filter l (lambda (x) (== (car x) 'error))))
               (length (list-filter l (lambda (x) (== (car x) 'warning))))))))

(define (command-name cmd)
  ;; the program of a command, without its directory
  (with p (car (append (string-tokenize-by-char cmd #\space) (list "")))
    (with i (string-search-backwards "/" (string-length p) p)
      (if (>= i 0) (substring p (+ i 1) (string-length p)) p))))

(define server-commands (make-ahash-table))

(define (lsp-server-command* lan)
  ;; the command of the server of @lan, kept (it is looked for in the PATH)
  (or (ahash-ref server-commands lan)
      (with cmd (and lan (lsp-server-command lan))
        (when cmd (ahash-set! server-commands lan cmd))
        cmd)))

(define (cursor-line+offset)
  ;; the line and offset of the cursor in a source file, or #f
  (lsp-target-lo (cursor-path)))

(define (before-or-at? l1 o1 l2 o2)
  (or (< l1 l2) (and (== l1 l2) (<= o1 o2))))

(define (diagnostic-at uri lo)
  ;; the diagnostic shown at the line and offset @lo (errors first)
  (let* ((l (or (ahash-ref shown uri) '()))
         (in (list-filter
              l (lambda (x)
                  (and (before-or-at? (list-ref x 1) (list-ref x 2)
                                      (car lo) (cadr lo))
                       (before-or-at? (car lo) (cadr lo)
                                      (list-ref x 3) (list-ref x 4)))))))
    (or (list-find in (lambda (x) (== (car x) 'error)))
        (and (nnull? in) (car in)))))

(define message-shown #f)

(define (first-line s)
  (with i (string-search-forwards "\n" 0 s)
    (if (>= i 0) (substring s 0 i) s)))

(define (severity-name s)
  (cond ((== s 'error) "Error")
        ((== s 'warning) "Warning")
        (else "Note")))

(tm-define (lsp-cursor-moved)
  (with uri (current-uri)
    (when uri
      (when (ahash-ref stale uri) (lsp-show-diagnostics))
      (let* ((lo (cursor-line+offset))
             (x (and lo (diagnostic-at uri lo))))
        (cond (x
               (with msg (string-append (severity-name (car x)) ": "
                                        (first-line (list-ref x 5)))
                 (when (!= msg message-shown)
                   (set! message-shown msg)
                   (set-message msg ""))))
              (message-shown
               (set! message-shown #f)
               (set-message "" "")))))))

(tm-define (notify-cursor-moved status)
  ;; NOTE: no mode predicates here (in-prog?...): the editor calls this
  ;; while it applies its changes, and the modes would be computed for an
  ;; environment which is not up to date (in-math? was then wrong after
  ;; (make 'math)); the buffers which a server follows are known instead
  (former status)
  (when (lsp-on?)
    ;; (in a document, the diagnostics of the code blocks which came while
    ;; it was not the current buffer, wherever the cursor is)
    (when (and (== (ahash-ref followed (url->string (current-buffer))) #t)
               (not (lsp-source-buffer?))
               (list-or (map (lambda (x) (ahash-ref stale (car x)))
                             (buffer-uris))))
      (lsp-show-diagnostics))
    (when (followed-buffer?)
      (lsp-cursor-moved))))

(tm-define (lsp-next-diagnostic forward?)
  (:synopsis "Goes to the next diagnostic (or the previous one)")
  (let* ((uri (current-uri))
         (lo (cursor-line+offset))
         (l (or (and uri (ahash-ref shown uri)) '()))
         (after? (lambda (x)
                   (and lo (if forward?
                               (not (before-or-at? (list-ref x 1)
                                                   (list-ref x 2)
                                                   (car lo) (cadr lo)))
                               (not (before-or-at? (car lo) (cadr lo)
                                                   (list-ref x 1)
                                                   (list-ref x 2)))))))
         (sorted (sort l (lambda (x y)
                           (before-or-at? (list-ref x 1) (list-ref x 2)
                                          (list-ref y 1) (list-ref y 2)))))
         (cands (list-filter (if forward? sorted (reverse sorted)) after?))
         (x (if (nnull? cands) (car cands)
                (and (nnull? sorted)
                     (if forward? (car sorted) (cAr sorted))))))
    (if (not x) (set-message "No diagnostics" "")
        ;; (the lines may have changed since: the place is in the buffer)
        (let* ((b (lsp-target-root))
               (n (if (tree-is? b 'document) (tree-arity b) 1))
               (l (max 0 (min (list-ref x 1) (- n 1))))
               (o (max 0 (min (list-ref x 2)
                              (string-length (lsp-buffer-line l))))))
          (with p (lsp-target-path l o) (when p (go-to-path p)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Requests at the cursor
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (cursor-params)
  ;; the document and the position of the cursor, once the server has the
  ;; text of the buffer
  (let ((uri (lsp-buffer-uri))
        (pos (lsp-cursor-position)))
    (and uri pos
         (begin
           (lsp-buffer-sync)
           (lsp-object "textDocument" (lsp-object "uri" uri)
                       "position" pos)))))

(define (capable? c capability)
  (lsp-client-capable? c capability))

(tm-define (lsp-request-at-cursor-with method capability extend)
  (:synopsis "As request-at-cursor, with the parameters given to @extend")
  (request-at-cursor method capability extend))

(define (request-at-cursor method capability . opt)
  ;; the result of the request @method at the cursor, or #f (with a
  ;; message): the server of the buffer must have @capability; @opt may
  ;; give a function which adds to the parameters
  ;; (the client is found first: synchronizing the text would start one)
  (let* ((c (lsp-buffer-client*))
         (ok? (and c (capable? c capability)))
         (params0 (and ok? (cursor-params)))
         (params (if (and params0 (pair? opt)) ((car opt) params0) params0)))
    (cond ((not c)
           (set-message "No language server for this buffer" "") #f)
          ((not ok?)
           (set-message "The language server cannot do this" "") #f)
          ((not params) #f)
          (else
           (with a (lsp-request-wait c method params 5000)
             (cond ((== (car a) 'result) (cadr a))
                   ((== (car a) 'error)
                    (set-message (utf8->cork
                                  (or (lsp-ref (cadr a) "message")
                                      "The request failed"))
                                 "")
                    #f)
                   (else
                    (set-message "The language server does not answer" "")
                    #f)))))))

;; Completion

(define (word-char? c)
  (or (char-alphabetic? c) (char-numeric? c) (== c #\_) (== c #\$)
      (>= (char->integer c) 128)))

(define (word-before line off)
  ;; the identifier which ends at @off in @line
  (let loop ((i off))
    (if (and (> i 0) (word-char? (string-ref line (- i 1))))
        (loop (- i 1))
        (substring line i off))))

(define (cursor-line)
  (with lo (cursor-line+offset)
    (and lo (list (lsp-buffer-line (car lo)) (cadr lo)))))

(define (completion-text item)
  ;; (the edit of an item comes before its insertText, as in the protocol;
  ;; a text of several lines is cut at its first one: a line of a source
  ;; file is a string)
  (with t (or (lsp-ref item "textEdit" "newText")
              (lsp-ref item "insertText")
              (lsp-ref item "label"))
    (if (string? t)
        (with i (string-search-forwards "\n" 0 t)
          (string-trim-both (if (>= i 0) (substring t 0 i) t)))
        "")))

(define (completion-items r)
  (let* ((items (if (func? r 'tuple) (cdr r) (lsp-items (lsp-ref r "items"))))
         (key (lambda (x) (or (lsp-ref x "sortText") (completion-text x)))))
    (sort items (lambda (x y) (string<? (key x) (key y))))))

(tm-define (lsp-completions prefix r)
  (:synopsis "The suffixes of the completions @r of @prefix (Cork)")
  (let loop ((l (completion-items r)) (acc '()) (n 0))
    (if (or (null? l) (>= n 100)) (reverse acc)
        (with t (source->cork (completion-text (car l)))
          (if (and (string-starts? t prefix)
                   (> (string-length t) (string-length prefix)))
              (with s (substring t (string-length prefix) (string-length t))
                (if (in? s acc) (loop (cdr l) acc n)
                    (loop (cdr l) (cons s acc) (+ n 1))))
              (loop (cdr l) acc n))))))

(tm-define (lsp-completion-tuple word line off l)
  (:synopsis "The argument of custom-complete for the suffixes @l of @word")
  ;; custom-complete inserts a suffix after the prefix which ends at the
  ;; cursor: after a . -> or :: (@word is empty), the character before the
  ;; cursor is that prefix, and the suffixes are the whole names
  (cond ((null? l) #f)
        ((!= word "") `(tuple ,word ,@l))
        ((> off 0) `(tuple ,(substring line (- off 1) off) ,@l))
        (else #f)))

;; the completion which is awaited (client . id), at most one
(define completion-request #f)

(define (completion-answer r word line off fallback?)
  (with t (lsp-completion-tuple word line off (lsp-completions word r))
    (cond (t (custom-complete (stree->tree t)))
          ((and fallback? (complete-try?)) (noop))
          (else (set-message "No completions" "")))))

(tm-define (lsp-complete . opt)
  (:synopsis "Completes the word at the cursor with the language server")
  ;; the answer is awaited apart, and applied if the cursor has not moved
  ;; meanwhile; without completions, those of the editor (complete-try?)
  ;; are tried if @opt is (#t)
  (let ((lo (cursor-line))
        (c (lsp-buffer-client*))
        (fallback? (and (pair? opt) (car opt))))
    (cond ((not lo) (noop))
          ((not (and c (capable? c "completionProvider")))
           (when (not (and fallback? (complete-try?)))
             (set-message "No completions" "")))
          (else
           (let* ((line (car lo)) (off (cadr lo))
                  (word (word-before line off))
                  (u (current-buffer))
                  (p (cursor-path))
                  (params (cursor-params)))
             (when completion-request
               (lsp-cancel (car completion-request) (cdr completion-request)))
             (set! completion-request
               (cons c (lsp-request
                        c "textDocument/completion" params
                        (lambda (r e)
                          (set! completion-request #f)
                          (when (and (not e) (== (current-buffer) u)
                                     (== (cursor-path) p))
                            (completion-answer r word line off
                                               fallback?)))))))))))

(define (followed-buffer?)
  ;; (in a document, the cursor in one of its code blocks)
  (and (== (ahash-ref followed (url->string (current-buffer))) #t)
       (or (lsp-source-buffer?) (lsp-cursor-block) #f)
       #t))

(tm-define (lsp-buffer-followed?)
  (:synopsis "Does a language server follow the current buffer?")
  (followed-buffer?))

(tm-define (lsp-completion-here?)
  ;; tab completes after a word, a . a -> or a ::, in a source file which a
  ;; server follows (NOTE: no mode predicates, see notify-cursor-moved)
  (and (lsp-on?) (followed-buffer?)
       (with lo (cursor-line+offset)
         (and lo (> (cadr lo) 0)
              (with t (tree->stree (path->tree (cDr (cursor-path))))
                (and (string? t) (<= (cadr lo) (string-length t))
                     (with c (string-ref t (- (cadr lo) 1))
                       (or (word-char? c) (in? c '(#\. #\> #\:))))))))
       (with c (lsp-buffer-client*)
         (and c (capable? c "completionProvider")))
       ;; (not in a string or a comment, as the syntax tree says)
       (not (code-in-text?))
       #t))

(tm-define (kbd-variant t forwards?)
  (:require (and forwards? (lsp-completion-here?)))
  (lsp-complete #t))

;; Information at the cursor (hover)

(define (marked-string x)
  ;; a MarkedString as markdown: a string, or a language and a value (code)
  (if (string? x) x
      (string-append "```" (or (lsp-ref x "language") "") "\n"
                     (or (lsp-ref x "value") "") "\n```")))

(define (hover-text r)
  ;; the text of the contents of a hover and whether it is markdown:
  ;; MarkupContent (its kind says), MarkedString (markdown, or code) or an
  ;; array of them
  (with c (lsp-ref r "contents")
    (cond ((string? c) (list c #t))
          ((func? c 'tuple)
           (list (string-recompose (map marked-string (cdr c)) "\n\n") #t))
          ((lsp-ref c "kind")
           (list (or (lsp-ref c "value") "")
                 (!= (lsp-ref c "kind") "plaintext")))
          ((lsp-ref c "value") (list (marked-string c) #t))
          (else (list "" #f)))))

(define (useful-lines s)
  ;; the lines of @s which say something (no fences or rules of markdown)
  (list-filter (map string-trim-both (string-decompose s "\n"))
               (lambda (l) (and (!= l "") (not (string-starts? l "```"))
                                (!= l "---")))))

(tm-define (lsp-hover-document text . opt)
  (:synopsis "The document of the text @text (markdown, utf8) of a hover")
  ;; markdown, which servers use for the hovers, or else lines as they are
  ;; (also for a text which is not markdown, when (car opt) is #f)
  (with t (and (or (null? opt) (car opt))
               (catch #t
                 (lambda () (convert text "markdown-snippet" "texmacs-stree"))
                 (lambda args #f)))
    (if (and t (!= t "") (!= t '(document)) (!= t '(document "")))
        t
        `(document ,@(map (lambda (l) `(verbatim ,(utf8->cork l)))
                          (useful-lines text))))))

(define hover-id #f)

(tm-define (lsp-hover-close)
  (when hover-id
    (close-tooltip hover-id)
    (set! hover-id #f)))

(define (show-hover r p anchor type)
  ;; the hover @r in a balloon at the tree @anchor (of the path @p): type
  ;; "keyboard" (closed when the cursor moves) or "mouse" (when the mouse
  ;; leaves it); #f if it says nothing
  (let* ((h (if (and r (not (lsp-null? r))) (hover-text r) (list "" #f)))
         (text (car h))
         (l (useful-lines text)))
    (cond ((null? l) #f)
          ((headless?)
           (set-message (utf8->cork (car l))
                        (if (null? (cdr l)) "" (utf8->cork (cadr l))))
           #t)
          (else
           (let ((id (string-append "lsp-hover:" (object->string p)))
                 (doc (lsp-hover-document text (cadr h))))
             (set! hover-id id)
             (show-tooltip id anchor `(preview-balloon ,doc)
                           "auto" "auto" type 0.8)
             #t)))))

(tm-define (lsp-hover)
  (:synopsis "Shows what the language server says about the cursor")
  ;; in a balloon at the cursor, as the previews of references (show-tooltip,
  ;; preview-balloon); it is closed when the cursor moves
  (lsp-hover-close)
  (with r (request-at-cursor "textDocument/hover" "hoverProvider")
    (when (and r (not (show-hover r (cursor-path) (cursor-tree) "keyboard")))
      (set-message "No information" ""))))

;; Information at the mouse: when the mouse rests on a word of a source file
;; which a server follows (preference "lsp:mouse hover"), the hover of the
;; server is asked, and shown if the mouse is still there

(define mouse-last #f)        ; (x y time) of the last move
(define mouse-scheduled? #f)
(define mouse-at #f)          ; the path asked last
(define mouse-request #f)     ; (client . id) of the hover awaited
(define mouse-rest 600)       ; ms

(tm-define (lsp-mouse-hover-on?)
  (!= (get-preference "lsp:mouse hover") "off"))

(define (mouse-word? p)
  ;; is the position @p (in a line) on a word?
  (with t (path->tree (cDr p))
    (and (tree-atomic? t)
         (let ((s (tree->string t)) (o (cAr p)))
           (and (< o (string-length s))
                (word-char? (string-ref s o)))))))

(define (mouse-hover x y)
  (let* ((p (xy->path x y))
         (r (lsp-target-lo p))
         (c (lsp-buffer-client*))
         (uri (lsp-buffer-uri))
         (u (current-buffer)))
    ;; (a place which the mouse has left is asked again when it comes back)
    (when (!= p mouse-at) (set! mouse-at #f))
    (when (and r (not mouse-at) c uri
               (capable? c "hoverProvider") (mouse-word? p))
      (set! mouse-at p)
      (when mouse-request
        (lsp-cancel (car mouse-request) (cdr mouse-request)))
      (lsp-buffer-sync)
      (set! mouse-request
        (cons c (lsp-request
                 c "textDocument/hover"
                 (lsp-object "textDocument" (lsp-object "uri" uri)
                             "position" (lsp-buffer-position (car r) (cadr r)))
                 (lambda (res e)
                   (set! mouse-request #f)
                   ;; (only if the mouse is still on the same word)
                   (when (and (not e) mouse-last (== (current-buffer) u)
                              (== (xy->path (car mouse-last) (cadr mouse-last))
                                  p))
                     (lsp-hover-close)
                     (show-hover res p (path->tree (cDr p)) "mouse")))))))))

(define (mouse-check)
  (set! mouse-scheduled? #f)
  (when mouse-last
    (with dt (- (texmacs-time) (caddr mouse-last))
      (if (< dt mouse-rest)
          (begin
            (set! mouse-scheduled? #t)
            (delayed (:pause (+ (- mouse-rest dt) 20)) (mouse-check)))
          (mouse-hover (car mouse-last) (cadr mouse-last))))))

(tm-define (mouse-event key x y mods time data)
  ;; (no mode predicates: see notify-cursor-moved)
  (former key x y mods time data)
  (when (and (== key "move") (lsp-on?) (followed-buffer?)
             (lsp-mouse-hover-on?))
    (set! mouse-last (list x y (texmacs-time)))
    (when (not mouse-scheduled?)
      (set! mouse-scheduled? #t)
      (delayed (:pause mouse-rest) (mouse-check)))))

;; Signatures: after a ( or a , (the trigger characters of the server), the
;; signature of the function whose arguments are typed is shown on the
;; status bar, with the current argument between guillemets (the status
;; bar shows text, not markup: a bold argument was shown as the others)

(define signature-shown? #f)

(define (signature-triggers c)
  (with l (lsp-items (lsp-ref (lsp-client-capabilities c)
                              "signatureHelpProvider" "triggerCharacters"))
    (if (null? l) '("(" ",") l)))

(define (utf16->byte s k)
  ;; the byte of the utf8 string @s at the UTF-16 unit @k, or #f (past it)
  (let ((n (string-length s)))
    (let loop ((i 0) (u 0))
      ;; (at a character only: the rest of the bytes of one are passed)
      (cond ((and (>= u k)
                  (or (>= i n)
                      (with c (char->integer (string-ref s i))
                        (or (< c 128) (>= c 192)))))
             i)
            ((>= i n) #f)
            (else
             (with c (char->integer (string-ref s i))
               (loop (+ i 1)
                     (cond ((< c 128) (+ u 1))
                           ((< c 192) u)
                           ((< c 240) (+ u 1))
                           (else (+ u 2))))))))))

(define (parameter-parts label param)
  ;; the Cork strings before, of and after the part @param (a string, or
  ;; offsets in UTF-16 units) of @label (utf8); all of it before when it is
  ;; not there. A string is looked for after the name of the function (from
  ;; its first parenthesis: "x" in "max(int x)" is not the x of max)
  (let* ((range (cond ((string? param)
                       (let* ((p (string-search-forwards "(" 0 label))
                              (i (string-search-forwards param (max p 0) label)))
                         (and (>= i 0) (list i (+ i (string-length param))))))
                      ((func? param 'tuple 2)
                       (let* ((a (lsp-number-value (cadr param)))
                              (b (lsp-number-value (caddr param)))
                              (a* (and a (>= a 0) (utf16->byte label a)))
                              (b* (and b (utf16->byte label b))))
                         (and a* b* (<= a* b*) (list a* b*))))
                      (else #f))))
    (if (not range) (list (utf8->cork label) "" "")
        (list (utf8->cork (substring label 0 (car range)))
              (utf8->cork (substring label (car range) (cadr range)))
              (utf8->cork (substring label (cadr range)
                                     (string-length label)))))))

(define (mark-parts l)
  ;; the parts @l as one Cork string, the parameter between guillemets
  ;; (19 and 20 in Cork)
  (if (== (cadr l) "") (car l)
      (string-append (car l) (string (integer->char 19)) (cadr l)
                     (string (integer->char 20)) (caddr l))))

(tm-define (lsp-signature-parts r)
  (:synopsis "The parts (Cork) of the SignatureHelp @r, or #f")
  ;; before the active parameter, the parameter, after it
  (let* ((sigs (lsp-items (lsp-ref r "signatures")))
         (i (or (lsp-number-value (lsp-ref r "activeSignature")) 0))
         (s (and (nnull? sigs)
                 (if (< i (length sigs)) (list-ref sigs i) (car sigs))))
         (label (and s (lsp-ref s "label")))
         (ap (and s (or (lsp-number-value (lsp-ref r "activeParameter"))
                        (lsp-number-value (lsp-ref s "activeParameter")))))
         (param (and ap (lsp-ref s "parameters" ap "label"))))
    (and (string? label) (parameter-parts label param))))

(tm-define (lsp-signature-markup r)
  (:synopsis "The text (Cork) of the SignatureHelp @r, or #f")
  (with l (lsp-signature-parts r)
    (and l (mark-parts l))))

;; the parts of the signature shown, for the focus bar (lsp-kbd)
(define signature-parts #f)

(tm-define (lsp-focus-signature)
  (:synopsis "The parts of the signature which is shown, or #f")
  signature-parts)

(define (hide-signature)
  (set! signature-shown? #f)
  (set! signature-parts #f)
  (set-message "" "")
  (lsp-status-refresh))

;; the request of a signature which is awaited (client . id), at most one
(define signature-request #f)

(tm-define (lsp-signature-help)
  (:synopsis "Shows the signature of the function at the cursor")
  (let ((c (lsp-buffer-client*))
        (uri (lsp-buffer-uri))
        (pos (lsp-cursor-position))
        (u (current-buffer))
        (lo (cursor-line+offset)))
    (when (and c uri pos (capable? c "signatureHelpProvider"))
      (lsp-buffer-sync)
      (when signature-request
        (lsp-cancel (car signature-request) (cdr signature-request)))
      (set! signature-request
        (cons c
          (lsp-request
           c "textDocument/signatureHelp"
           (lsp-object "textDocument" (lsp-object "uri" uri)
                       "position" pos)
           (lambda (r e)
             (set! signature-request #f)
             ;; (shown only in the same buffer, with the cursor on the same
             ;; line: an answer may come late)
             (let* ((lo2 (cursor-line+offset))
                    (here? (and (== (current-buffer) u) lo lo2
                                (== (car lo) (car lo2))))
                    (l (and here? (not e) r (not (lsp-null? r))
                            (lsp-signature-parts r))))
               (cond (l (set! signature-shown? #t)
                        (set! signature-parts l)
                        (set-message (mark-parts l) "")
                        (lsp-status-refresh))
                     (signature-shown? (hide-signature)))))))))))

(tm-define (keyboard-press key time)
  ;; (no mode predicates: see notify-cursor-moved)
  (former key time)
  (when (and (lsp-on?) (followed-buffer?))
    (with c (lsp-buffer-client*)
      (when c
        (cond ((in? key (signature-triggers c)) (lsp-signature-help))
              ((and signature-shown? (== key ")"))
               (hide-signature)))))))

;; Definitions

(define (location-of r)
  ;; the first Location or LocationLink of @r, as (uri range)
  (with x (if (func? r 'tuple) (and (nnull? (cdr r)) (cadr r)) r)
    (and x (not (lsp-null? x))
         (let ((uri (or (lsp-ref x "uri") (lsp-ref x "targetUri")))
               (range (or (lsp-ref x "range")
                          (lsp-ref x "targetSelectionRange"))))
           (and (string? uri) range (list uri range))))))

(define (goto-block b range)
  ;; b: (doc root lan), a code block of the document doc (lsp-blocks.scm)
  (with u (system->url (car b))
    (when (!= u (current-buffer)) (switch-to-buffer u))
    (when (and (== (current-buffer) u) (tree->path (cadr b)))
      (go-to-path (append (tree->path (cadr b))
                          (lsp-buffer-line+offset (lsp-ref range "start")
                                                  (cadr b)))))))

(define (goto-unit un range)
  ;; a place of the unit @un, the blocks of a language of a document
  (with u (system->url (lsp-unit-doc un))
    (when (!= u (current-buffer)) (switch-to-buffer u))
    (when (== (current-buffer) u)
      (let* ((lo (lsp-buffer-line+offset (lsp-ref range "start")
                                          (lsp-unit-lines un)))
             (p (lsp-unit-path-of un (car lo) (cadr lo))))
        (when p (go-to-path p))))))

(tm-define (lsp-goto-location uri range)
  (:synopsis "Opens the file of @uri at the start of @range")
  ;; (or the document of the code block whose virtual file it is)
  (let* ((path (lsp-uri->path uri))
         (un (and path (lsp-unit-of-path path)))
         (b (and path (not un) (lsp-block-of-path path))))
    (cond ((and un (buffer-exists? (system->url (lsp-unit-doc un))))
           (goto-unit un range))
          ((and b (buffer-exists? (system->url (car b))))
           (goto-block b range))
          ((or (not path) (lsp-virtual-block-path? path))
           ;; (a code block of a document which is closed)
           (set-message "Cannot open this location" ""))
          (else
           (with u (system->url path)
             ;; (switching to the current buffer would make a second view)
             (cond ((== u (current-buffer)) (noop))
                   ((buffer-exists? u) (switch-to-buffer u))
                   (else (load-buffer u)))
             (when (== (current-buffer) u)
               (with lo (lsp-buffer-line+offset (lsp-ref range "start"))
                 (with p (lsp-target-path (car lo) (cadr lo))
                   (when p (go-to-path p))))))))))

(tm-define (lsp-definition-at-cursor)
  (:synopsis "Goes to the definition of the symbol at the cursor, #t if it did")
  ;; (for code-navigate.scm: #f lets the next provider look for it)
  (with r (request-at-cursor "textDocument/definition" "definitionProvider")
    (with loc (and r (location-of r))
      (and loc (begin (apply lsp-goto-location loc) #t)))))

(tm-define (lsp-can-find-definitions?)
  (:synopsis "Does a server which finds definitions follow the buffer?")
  (and (lsp-on?) (lsp-buffer-followed?)
       (with c (lsp-buffer-client*)
         (and c (lsp-client-running? c)
              (lsp-client-capable? c "definitionProvider")))))

(tm-define (lsp-goto-definition)
  (:synopsis "Goes to the definition of the symbol at the cursor")
  ;; (the server first, else the syntax tree: code-navigate.scm)
  (code-go-to-definition))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Servers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (buffer-key)
  ;; the language and the project of the current buffer, or #f
  (let ((lan (lsp-buffer-language))
        (p (lsp-buffer-path)))
    (and lan p (list lan (lsp-project-root p)))))

(define (buffer-client-entry)
  ;; the client of the current buffer, also if its server has ended
  (and-with k (buffer-key)
    (lsp-client-entry (car k) (cadr k))))

(tm-define (lsp-client-ended c)
  (former c)
  (set-message (string-append "The language server has ended "
                              "(Tools, Language servers, Server log)")
               (utf8->cork (lsp-client-ref c 'name))))

(define (forget-caches)
  ;; (the buffers, their servers and their projects are looked for again:
  ;; a project may have been made since, or a server added)
  (set! followed (make-ahash-table))
  (set! buffer-servers (make-ahash-table))
  (set! server-commands (make-ahash-table))
  (lsp-forget-project-roots))

(tm-define (lsp-restart)
  (:synopsis "Restarts the language server of the current buffer")
  ;; (also one which has ended or which has been stopped)
  (with k (buffer-key)
    (when k
      (and-with c (lsp-client-find (car k) (cadr k))
        (lsp-client-stop c))
      (lsp-client-forget (car k) (cadr k)))
    (forget-caches)
    (with c (lsp-buffer-sync)
      ;; (the other open buffers are followed again, by the new server)
      (follow-all)
      (if (and c (lsp-client-alive? c))
          (set-message "Language server started" "")
          (set-message "No language server for this buffer" "")))))

(define (follow-all)
  ;; the buffers which are open are followed (their texts go to their
  ;; servers, after a pause)
  (for (u (buffer-list)) (lsp-follow-buffer u)))

(tm-define (lsp-stop-servers)
  (:synopsis "Ends all the language servers, until they are restarted")
  (lsp-stop-all)
  (set! pending (make-ahash-table))
  (set-message "Language servers stopped" "Restart server starts one again"))

(define (log-body c)
  ;; the body of the page of the log of @c
  `(document
    (with "font-family" "tt" "par-first" "0cm"
      (document
       ,(utf8->cork (string-append (lsp-client-ref c 'name) " ("
                                   (symbol->string (lsp-client-state c))
                                   ")"))
       ,@(append-map
          (lambda (x)
            (map (lambda (l) (utf8->cork (string-append (car x) ": " l)))
                 (list-filter (string-decompose (cadr x) "\n")
                              (lambda (l) (!= l "")))))
          (lsp-client-log c))))))

;; the client whose log is on the page of the logs, and whether a refresh
;; of the page is due
(define log-client #f)
(define log-refresh? #f)

(define (log-page)
  (aux-name "Language server"))

(tm-define (lsp-show-log)
  (:synopsis "Shows what the language server of the buffer has written")
  ;; the page follows the log as long as it is open (lsp-log-refresh)
  (with c (buffer-client-entry)
    (if (not c) (set-message "No language server for this buffer" "")
        (begin
          (set! log-client c)
          ;; (a whole document, with its style and its body)
          (open-auxiliary "Language server"
                          `(document (style (tuple "generic"))
                                     (body ,(log-body c))))))))

(tm-define (lsp-log-refresh)
  (:synopsis "The page of the log shows the log as it is now")
  (set! log-refresh? #f)
  (when log-client
    (with u (log-page)
      (if (buffer-exists? u)
          (buffer-set-body u (stree->tree (log-body log-client)))
          (set! log-client #f)))))

(tm-define (lsp-client-log-changed c)
  (former c)
  (when (and (eq? c log-client) (not log-refresh?))
    (set! log-refresh? #t)
    (delayed
      (:pause 300)
      (lsp-log-refresh))))

(define diags-uri #f)          ; the file whose diagnostics are on the page
(define diags-refresh? #f)

(define (diags-page)
  (aux-name "Diagnostics"))

(define (severity-label s)
  (cond ((== s 'error) `(with "color" "#c02020" "error"))
        ((== s 'warning) `(with "color" "#b07000" "warning"))
        ((== s 'hint) "hint")
        (else "note")))

(define (diags-body uri)
  ;; the body of the page of the diagnostics of @uri: one line by
  ;; diagnostic, which goes to it when clicked
  (let* ((ds (lsp-diagnostics uri))
         (p (or (lsp-uri->path uri) uri)))
    `(document
      (strong ,(utf8->cork p))
      ,@(if (null? ds) '("No diagnostics")
            (map (lambda (d)
                   (let ((l (or (lsp-number-value
                                 (lsp-ref d "range" "start" "line")) 0))
                         (ch (or (lsp-number-value
                                  (lsp-ref d "range" "start" "character")) 0)))
                     `(action (concat ,(number->string (+ l 1)) ":"
                                      ,(number->string (+ ch 1)) " "
                                      ,(severity-label
                                        (lsp-diagnostic-severity d))
                                      ": "
                                      ,(first-line (lsp-diagnostic-message d)))
                              ,(string-append "(lsp-goto-position "
                                              (object->string uri) " "
                                              (number->string l) " "
                                              (number->string ch) ")"))))
                 ds)))))

(tm-define (lsp-goto-position uri line ch)
  (:synopsis "Goes to the line @line and the character @ch of @uri")
  (:secure #t)
  (lsp-goto-location uri (lsp-object "start"
                                     (lsp-object "line" (lsp-number line)
                                                 "character"
                                                 (lsp-number ch)))))

(tm-define (lsp-show-diagnostics-list)
  (:synopsis "Shows the diagnostics of the buffer on a page")
  ;; the page follows the diagnostics as long as it is open
  (with uri (lsp-buffer-uri)
    (if (not uri) (set-message "No language server for this buffer" "")
        (begin
          (set! diags-uri uri)
          (open-auxiliary "Diagnostics"
                          `(document (style (tuple "generic"))
                                     (body ,(diags-body uri))))))))

(tm-define (lsp-diagnostics-list-refresh)
  (set! diags-refresh? #f)
  (when diags-uri
    (with u (diags-page)
      (if (buffer-exists? u)
          (buffer-set-body u (stree->tree (diags-body diags-uri)))
          (set! diags-uri #f)))))

(tm-define (lsp-diagnostics-changed uri)
  (former uri)
  (when (and (== uri diags-uri) (not diags-refresh?))
    (set! diags-refresh? #t)
    (delayed
      (:pause 300)
      (lsp-diagnostics-list-refresh))))

(tm-define (lsp-turn-off)
  ;; (they are started again when the preference is on again)
  (lsp-stop-all)
  (lsp-forget-all)
  (forget-caches)
  (set! pending (make-ahash-table))
  ;; (the underlines are removed in each buffer which has some)
  (for (u (buffer-list))
    (when (or (ahash-ref shown (lsp-buffer-uri u))
              (and (lsp-buffer-path u)
                   (nnull? (lsp-block-entries (lsp-buffer-path u)))))
      (with-buffer u (lsp-hide-diagnostics))))
  (set! shown (make-ahash-table)))

(define (unfollow-blocks)
  ;; the code blocks of the documents are no longer followed
  (for (u (buffer-list))
    (with name (url->string u)
      (when (and (== (ahash-ref followed name) #t)
                 (not (lsp-source-buffer? u)))
        (ahash-remove! followed name)
        (ahash-remove! pending name)
        (lsp-buffer-close u)
        (with-buffer u (lsp-hide-diagnostics))))))

(define-preferences
  ("lsp" "on" (lambda (pref val)
                (if (== val "off") (lsp-turn-off) (follow-all))))
  ("lsp:code blocks" "off" (lambda (pref val)
                             (if (== val "on") (follow-all) (unfollow-blocks))))
  ;; (the blocks of a language in a document as one file, lsp-blocks.scm)
  ("lsp:join code blocks" "on" (lambda (pref val)
                                 (when (lsp-blocks-on?) (follow-all))))
  ("lsp:mouse hover" "on" (lambda (pref val) (noop))))

;; the buffers which were loaded before this module
(delayed
  (:idle 500)
  (when (lsp-on?) (follow-all)))
