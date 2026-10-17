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
;; underlined (the alt selections lsp-errors, lsp-warnings and lsp-infos,
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
        (lsp lsp-documents)))

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
  (with r (ahash-ref followed name)
    (if r (== r #t)
        (let* ((lan (and (lsp-buffer-path u) (lsp-buffer-language u)))
               (ok (and lan (lsp-server-command lan) #t)))
          (ahash-set! followed name (if ok #t 'no))
          ok))))

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
      (when u (lsp-follow-buffer u)))))

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
  ;; the diagnostic @d in @lines, or #f if it is beyond them; an empty
  ;; range covers a character
  (let* ((n (length lines))
         (r (lsp-diagnostic-range d lines))
         (l1 (car r)) (o1 (cadr r))
         (l2 (min (caddr r) (- n 1)))
         (o2 (if (< (caddr r) n) (cadddr r)
                 (string-length (list-ref lines (- n 1))))))
    (and (< l1 n) (>= l1 0)
         (with len (string-length (list-ref lines l1))
           (when (and (== l1 l2) (<= o2 o1))
             (set! o2 (min len (+ o1 1)))
             (when (<= o2 o1) (set! o1 (max 0 (- o2 1)))))
           (list (with s (lsp-diagnostic-severity d)
                   (if (== s 'hint) 'info s))
                 l1 o1 l2 o2
                 (lsp-diagnostic-message d))))))

(define (severity-paths l severity)
  (with b (buffer-path)
    (append-map (lambda (x)
                  (if (!= (car x) severity) '()
                      (list (append b (list (list-ref x 1) (list-ref x 2)))
                            (append b (list (list-ref x 3) (list-ref x 4))))))
                l)))

(tm-define (lsp-show-diagnostics)
  (:synopsis "Shows the diagnostics of the current buffer")
  (let* ((uri (lsp-buffer-uri))
         (lines (lsp-buffer-lines))
         (l (if uri
                (list-filter (map (lambda (d) (diagnostic-item d lines))
                                  (lsp-diagnostics uri))
                             identity)
                '())))
    (when uri
      (ahash-remove! stale uri)
      (ahash-set! shown uri l))
    (set-alt-selection "lsp-errors" (severity-paths l 'error))
    (set-alt-selection "lsp-warnings" (severity-paths l 'warning))
    (set-alt-selection "lsp-infos" (severity-paths l 'info))))

(tm-define (lsp-hide-diagnostics)
  (cancel-alt-selection "lsp-errors")
  (cancel-alt-selection "lsp-warnings")
  (cancel-alt-selection "lsp-infos"))

(tm-define (lsp-diagnostics-changed uri)
  (former uri)
  (ahash-set! stale uri #t)
  (when (== (current-uri) uri)
    (lsp-show-diagnostics)))

(define (cursor-line+offset)
  ;; the line and offset of the cursor in a source file, or #f
  (let* ((p (cursor-path))
         (b (buffer-path)))
    (and (list-starts? p b)
         (with r (list-tail p (length b))
           (and (= (length r) 2) r)))))

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
  (when (and (lsp-on?) (followed-buffer?))
    (lsp-cursor-moved)))

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
        (go-to-path (append (buffer-path)
                            (list (list-ref x 1) (list-ref x 2)))))))

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

(define (request-at-cursor method capability)
  ;; the result of the request @method at the cursor, or #f (with a
  ;; message): the server of the buffer must have @capability
  (let ((c (lsp-buffer-client*))
        (params (cursor-params)))
    (cond ((not c)
           (set-message "No language server for this buffer" "") #f)
          ((lsp-null? (lsp-ref (lsp-client-capabilities c) capability))
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
    (and lo
         (with lines (lsp-buffer-lines)
           (and (< (car lo) (length lines))
                (list (list-ref lines (car lo)) (cadr lo)))))))

(define (completion-text item)
  (with t (or (lsp-ref item "insertText")
              (lsp-ref item "textEdit" "newText")
              (lsp-ref item "label"))
    (if (string? t) (string-trim-both t) "")))

(define (string-trim-both s)
  (let* ((l (string->list s))
         (sp? (lambda (c) (in? c '(#\space #\tab))))
         (l1 (list-drop-while l sp?))
         (l2 (reverse (list-drop-while (reverse l1) sp?))))
    (list->string l2)))

(define (list-drop-while l pred?)
  (if (and (pair? l) (pred? (car l))) (list-drop-while (cdr l) pred?) l))

(define (completion-items r)
  (let* ((items (if (func? r 'tuple) (cdr r) (lsp-items (lsp-ref r "items"))))
         (key (lambda (x) (or (lsp-ref x "sortText") (completion-text x)))))
    (sort items (lambda (x y) (string<? (key x) (key y))))))

(tm-define (lsp-completions prefix r)
  (:synopsis "The suffixes of the completions @r of @prefix (Cork)")
  (let loop ((l (completion-items r)) (acc '()) (n 0))
    (if (or (null? l) (>= n 100)) (reverse acc)
        (with t (utf8->cork (completion-text (car l)))
          (if (and (string-starts? t prefix)
                   (> (string-length t) (string-length prefix)))
              (with s (substring t (string-length prefix) (string-length t))
                (if (in? s acc) (loop (cdr l) acc n)
                    (loop (cdr l) (cons s acc) (+ n 1))))
              (loop (cdr l) acc n))))))

(tm-define (lsp-complete)
  (:synopsis "Completes the word at the cursor with the language server")
  (with lo (cursor-line)
    (when lo
      (let* ((line (car lo)) (off (cadr lo))
             (word (word-before line off))
             (r (request-at-cursor "textDocument/completion"
                                   "completionProvider")))
        (when r
          (with l (lsp-completions word r)
            (cond ((null? l) (set-message "No completions" ""))
                  ((!= word "")
                   (custom-complete (stree->tree `(tuple ,word ,@l))))
                  ((> off 0)
                   ;; after a . -> or ::, the character before the cursor
                   ;; is the prefix of the completions
                   (with c (substring line (- off 1) off)
                     (custom-complete
                      (stree->tree `(tuple ,c ,@(map (lambda (s)
                                                      (string-append c s))
                                                    l))))))
                  (else (set-message "No completions" "")))))))))

(define (followed-buffer?)
  (== (ahash-ref followed (url->string (current-buffer))) #t))

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
       (lsp-buffer-client*)
       #t))

(tm-define (kbd-variant t forwards?)
  (:require (and forwards? (lsp-completion-here?)))
  (lsp-complete))

;; Information at the cursor (hover)

(define (hover-text r)
  ;; the text of the contents of a hover: MarkupContent, MarkedString (a
  ;; string, or language and value) or an array of them
  (with c (lsp-ref r "contents")
    (cond ((string? c) c)
          ((func? c 'tuple)
           (string-recompose (map (lambda (x)
                                    (if (string? x) x
                                        (or (lsp-ref x "value") "")))
                                  (cdr c))
                             "\n"))
          ((lsp-ref c "value") (lsp-ref c "value"))
          (else ""))))

(define (useful-lines s)
  ;; the lines of @s which say something (no fences or rules of markdown)
  (list-filter (map string-trim-both (string-decompose s "\n"))
               (lambda (l) (and (!= l "") (not (string-starts? l "```"))
                                (!= l "---")))))

(tm-define (lsp-hover)
  (:synopsis "Shows what the language server says about the cursor")
  (with r (request-at-cursor "textDocument/hover" "hoverProvider")
    (with l (if (and r (not (lsp-null? r)))
                (useful-lines (hover-text r))
                '())
      (cond ((null? l) (when r (set-message "No information" "")))
            (else
             (set-message (utf8->cork (car l))
                          (if (null? (cdr l)) ""
                              (utf8->cork (cadr l)))))))))

;; Definitions

(define (location-of r)
  ;; the first Location or LocationLink of @r, as (uri range)
  (with x (if (func? r 'tuple) (and (nnull? (cdr r)) (cadr r)) r)
    (and x (not (lsp-null? x))
         (let ((uri (or (lsp-ref x "uri") (lsp-ref x "targetUri")))
               (range (or (lsp-ref x "range")
                          (lsp-ref x "targetSelectionRange"))))
           (and (string? uri) range (list uri range))))))

(tm-define (lsp-goto-location uri range)
  (:synopsis "Opens the file of @uri at the start of @range")
  (with path (lsp-uri->path uri)
    (if (not path) (set-message "Cannot open this location" "")
        (with u (system->url path)
          ;; (switching to the current buffer would make a second view)
          (cond ((== u (current-buffer)) (noop))
                ((buffer-exists? u) (switch-to-buffer u))
                (else (load-buffer u)))
          (when (== (current-buffer) u)
            (with lo (lsp-position->line+offset (lsp-buffer-lines)
                                                (lsp-ref range "start"))
              (go-to-path (append (buffer-path) lo))))))))

(tm-define (lsp-goto-definition)
  (:synopsis "Goes to the definition of the symbol at the cursor")
  (with r (request-at-cursor "textDocument/definition" "definitionProvider")
    (with loc (and r (location-of r))
      (if loc (apply lsp-goto-location loc)
          (when r (set-message "No definition found" ""))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Servers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (lsp-restart)
  (:synopsis "Restarts the language server of the current buffer")
  (with c (lsp-buffer-client*)
    (when c (lsp-client-stop c))
    (set! followed (make-ahash-table))
    (if (lsp-buffer-sync) (set-message "Language server started" "")
        (set-message "No language server for this buffer" ""))))

(tm-define (lsp-show-log)
  (:synopsis "Shows what the language server of the buffer has written")
  (with c (lsp-buffer-client*)
    (if (not c) (set-message "No language server for this buffer" "")
        (open-auxiliary
         "Language server"
         `(document
           (with "font-family" "tt" "par-first" "0cm"
             (document
              ,(utf8->cork (string-append (lsp-client-ref c 'name) " ("
                                          (symbol->string
                                           (lsp-client-state c))
                                          ")"))
              ,@(append-map
                 (lambda (x)
                   (map (lambda (l) (utf8->cork (string-append (car x) ": "
                                                               l)))
                        (list-filter (string-decompose (cadr x) "\n")
                                     (lambda (l) (!= l "")))))
                 (lsp-client-log c)))))))))

(tm-define (lsp-turn-off)
  (lsp-stop-all)
  (set! followed (make-ahash-table))
  (set! pending (make-ahash-table))
  (set! shown (make-ahash-table))
  (lsp-hide-diagnostics))

(define-preferences
  ("lsp" "on" (lambda (pref val) (when (== val "off") (lsp-turn-off)))))

;; the buffers which were loaded before this module
(delayed
  (:idle 500)
  (when (lsp-on?)
    (for (u (buffer-list)) (lsp-follow-buffer u))))
