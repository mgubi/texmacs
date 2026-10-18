;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : lsp-decorate.scm
;; DESCRIPTION : colours (semantic tokens) and inlay hints of language servers
;; COPYRIGHT   : (C) 2026  Massimiliano Gubinelli
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Once a server has seen a change of the current buffer (it publishes its
;; diagnostics then), it is asked for the kinds of its names (semantic
;; tokens: functions, types, macros...) and for its inlay hints (the names
;; of the parameters before the arguments, the types which are deduced).
;; They are given to the typesetter line by line (lsp-decorate-line, C++,
;; lsp_decorations.cpp): the names get the colours of their kinds, the hints
;; are shown in grey before their characters, and are not part of the text.
;; The decorations of a line are kept with its text: a line which is edited
;; loses them until the server has answered again. Only the lines whose
;; decorations change are typeset again. Keywords, comments, strings and
;; numbers keep the colours of the editor (those of the preferences of the
;; syntax); the kinds which the editor does not see get theirs.

(texmacs-module (lsp lsp-decorate)
  (:use (lsp lsp-base)
        (lsp lsp-client)
        (lsp lsp-documents)
        (lsp lsp-edit)))

(define (colors-on?) (!= (get-preference "lsp:semantic colors") "off"))
(define (hints-on?) (!= (get-preference "lsp:inlay hints") "off"))

;; (a long file is not decorated: the answers of the server, as strees,
;; would be large, and the editor's colours are there)
(define lsp-max-decorated-lines 20000)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Semantic tokens
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (syntax-color lan group)
  ;; the colour of the syntax @group of @lan, in the preferences, or #f
  (with c (get-preference (string-append "syntax:" lan ":" group))
    (and (string? c) (!= c "") (!= c "default") c)))

(tm-define (lsp-token-color lan type)
  (:synopsis "The colour of a semantic token of the kind @type, or #f")
  (cond ((in? type '("type" "class" "enum" "interface" "struct"
                     "typeParameter" "namespace" "concept"))
         (or (syntax-color lan "constant_type") "#4040c0"))
        ((in? type '("function" "method")) "#1f5fa8")
        ((== type "macro") (or (syntax-color lan "preprocessor") "dark brown"))
        ((== type "enumMember")
         (or (syntax-color lan "constant_number") "#4040c0"))
        (else #f)))

(tm-define (lsp-decode-tokens data types)
  (:synopsis "The semantic tokens @data (numbers) as (line char length type)")
  ;; the protocol gives each token relative to the previous one: the line
  ;; after it, and the start after its start on the same line
  (let loop ((l data) (line 0) (ch 0) (acc '()))
    (if (or (null? l) (null? (cdr l)) (null? (cddr l))
            (null? (cdddr l)) (null? (cddddr l)))
        (reverse acc)
        (let* ((dl (car l)) (ds (cadr l)) (len (caddr l)) (ty (cadddr l))
               (line* (+ line dl))
               (ch* (if (== dl 0) (+ ch ds) ds))
               (name (and (>= ty 0) (< ty (vector-length types))
                          (vector-ref types ty))))
          (loop (cdr (cddddr l)) line* ch*
                (if name (cons (list line* ch* len name) acc) acc))))))

(define (numbers t)
  ;; the numbers of the array @t
  (map (lambda (x) (or (lsp-number-value x) 0)) (lsp-items t)))

(define (token-legend c)
  (list->vector
   (lsp-items (lsp-ref (lsp-client-capabilities c)
                       "semanticTokensProvider" "legend" "tokenTypes"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Inlay hints
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (lsp-hint-text h)
  (:synopsis "The text of the InlayHint @h, with its paddings")
  (let* ((l (lsp-ref h "label"))
         (s (if (string? l) l
                (apply string-append
                       (map (lambda (p) (or (lsp-ref p "value") ""))
                            (lsp-items l))))))
    (string-append (if (lsp-true? (lsp-ref h "paddingLeft")) " " "")
                   s
                   (if (lsp-true? (lsp-ref h "paddingRight")) " " ""))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The decorations of the lines
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (lsp-line-decorations lines lan tokens hints)
  (:synopsis "The decorations of @lines: line -> (colors hints), in Cork")
  ;; @tokens as from lsp-decode-tokens, @hints InlayHints; the offsets are
  ;; those of the Cork lines (UTF-16 units in the protocol)
  (let ((table (make-ahash-table))
        (n (vector-length lines)))
    (define (add! line which item)
      (with old (or (ahash-ref table line) (list '() '()))
        (ahash-set! table line
                    (if (== which 0)
                        (list (cons item (car old)) (cadr old))
                        (list (car old) (cons item (cadr old)))))))
    (for (tok tokens)
      (let* ((line (car tok))
             (col (lsp-token-color lan (cadddr tok))))
        (when (and col (< line n))
          (let* ((s (vector-ref lines line))
                 (a (lsp-character->offset s (cadr tok)))
                 (b (lsp-character->offset s (+ (cadr tok) (caddr tok)))))
            (when (< a b) (add! line 0 (list a b col)))))))
    (for (h hints)
      (let* ((pos (lsp-ref h "position"))
             (line (or (lsp-number-value (lsp-ref pos "line")) -1))
             (ch (or (lsp-number-value (lsp-ref pos "character")) 0)))
        (when (and (>= line 0) (< line n))
          (add! line 1 (list (lsp-character->offset (vector-ref lines line) ch)
                             (utf8->cork (lsp-hint-text h)))))))
    ;; (in the order of the line)
    (with by-start (lambda (x y) (< (car x) (car y)))
      (map (lambda (x)
             (list (car x)
                   (list (sort (car (cdr x)) by-start)
                         (sort (cadr (cdr x)) by-start))))
           (ahash-table->list table)))))

(define (decoration-tree items)
  `(tuple ,@(map (lambda (x)
                   `(tuple ,@(map (lambda (y)
                                    (if (number? y) (number->string y) y))
                                  x)))
                 items)))

;; uri -> line -> (colors hints): what the lines of the buffer have now,
;; and uri -> its buffer
(define decorated (make-ahash-table))
(define decorated-buffers (make-ahash-table))

(tm-define (lsp-decorations uri)
  (:synopsis "The decorations of the lines of @uri: (line colors hints)")
  (with t (ahash-ref decorated uri)
    (if (not t) '()
        (sort (map (lambda (x) (cons (car x) (cdr x))) (ahash-table->list t))
              (lambda (x y) (< (car x) (car y)))))))

(tm-define (lsp-apply-decorations u uri decos)
  (:synopsis "Gives the decorations @decos (line, colors, hints) to @u")
  ;; (the lines whose decorations change are typeset again)
  (let* ((body (buffer-get-body u))
         (n (if (tree-is? body 'document) (tree-arity body) 0))
         (old (or (ahash-ref decorated uri) (make-ahash-table)))
         (new (make-ahash-table))
         (changed '()))
    (for (d decos)
      (when (< (car d) n)
        (ahash-set! new (car d) (cadr d))))
    (for (x (ahash-table->list old))
      (when (and (< (car x) n) (not (ahash-ref new (car x))))
        (lsp-undecorate-line (tree-ref body (car x)))
        (set! changed (cons (car x) changed))))
    (for (x (ahash-table->list new))
      (when (!= (ahash-ref old (car x)) (cdr x))
        (lsp-decorate-line (tree-ref body (car x))
                           (stree->tree (decoration-tree (car (cdr x))))
                           (stree->tree (decoration-tree (cadr (cdr x)))))
        (set! changed (cons (car x) changed))))
    (ahash-set! decorated uri new)
    (ahash-set! decorated-buffers uri u)
    (when (and (nnull? changed) (== (current-buffer) u))
      (if (> (length changed) 2000) (update-current-buffer)
          (with b (buffer-path)
            (for (l changed) (update-path (append b (list l)))))))
    (length changed)))

(define (request-decorations c u uri)
  ;; asks @c for the tokens and the hints of @u, then decorates it
  (let* ((lines (list->vector (lsp-buffer-lines u)))
         (n (vector-length lines))
         (lan (or (lsp-buffer-language u) ""))
         (text (lsp-buffer-text u))
         (tokens #f) (hints #f) (left 2))
    (define (done)
      (set! left (- left 1))
      (when (and (== left 0) (buffer-exists? u)
                 ;; (only for the text which was asked about)
                 (== (lsp-buffer-text u) text))
        (lsp-apply-decorations u uri (lsp-line-decorations
                                      lines lan (or tokens '())
                                      (or hints '())))))
    (if (and (colors-on?)
             (not (lsp-null? (lsp-ref (lsp-client-capabilities c)
                                      "semanticTokensProvider"))))
        (lsp-request c "textDocument/semanticTokens/full"
                     (lsp-object "textDocument" (lsp-object "uri" uri))
                     (lambda (r e)
                       (when (and (not e) r (not (lsp-null? r)))
                         (set! tokens (lsp-decode-tokens
                                       (numbers (lsp-ref r "data"))
                                       (token-legend c))))
                       (done)))
        (done))
    (if (and (hints-on?)
             (not (lsp-null? (lsp-ref (lsp-client-capabilities c)
                                      "inlayHintProvider"))))
        (lsp-request c "textDocument/inlayHint"
                     (lsp-object "textDocument" (lsp-object "uri" uri)
                                 "range" (lsp-object
                                          "start" (lsp-object
                                                   "line" (lsp-number 0)
                                                   "character" (lsp-number 0))
                                          "end" (lsp-object
                                                 "line" (lsp-number n)
                                                 "character" (lsp-number 0))))
                     (lambda (r e)
                       (when (and (not e) r (not (lsp-null? r)))
                         (set! hints (lsp-items r)))
                       (done)))
        (done))))

(define scheduled? #f)

(tm-define (lsp-decorate-current)
  (:synopsis "Asks the server of the current buffer for its decorations")
  (set! scheduled? #f)
  (let ((c (lsp-buffer-client*))
        (u (current-buffer))
        (uri (lsp-buffer-uri)))
    (when (and c uri (lsp-client-running? c) (lsp-buffer-followed?)
               (<= (tree-arity (buffer-get-body u)) lsp-max-decorated-lines))
      (if (or (colors-on?) (hints-on?))
          (request-decorations c u uri)
          (lsp-apply-decorations u uri '())))))

(tm-define (lsp-decorations-changed)
  (:synopsis "The decorations are made again (a preference has changed)")
  (when (and (lsp-on?) (lsp-buffer-uri))
    (lsp-decorate-current)))

(tm-define (lsp-diagnostics-changed uri)
  ;; (the server has seen the text: its tokens and hints are those of it)
  (former uri)
  (when (and (== uri (lsp-buffer-uri)) (not scheduled?))
    (set! scheduled? #t)
    (delayed
      (:pause 200)
      (lsp-decorate-current))))

(tm-define (lsp-forget-decorations uri)
  (:synopsis "The lines of @uri lose their decorations")
  (let ((u (ahash-ref decorated-buffers uri))
        (old (ahash-ref decorated uri)))
    (when (and u old (buffer-exists? u))
      (with body (buffer-get-body u)
        (for (x (ahash-table->list old))
          (when (and (tree-is? body 'document) (< (car x) (tree-arity body)))
            (lsp-undecorate-line (tree-ref body (car x)))))))
    (ahash-remove! decorated uri)
    (ahash-remove! decorated-buffers uri)))

(tm-define (notify-buffer-closed name)
  ;; (before the buffer goes: its lines lose their decorations)
  (with u (string->url name)
    (and-with uri (lsp-buffer-uri u)
      (lsp-forget-decorations uri)))
  (former name))

(tm-define (lsp-turn-off)
  (former)
  (for (uri (map car (ahash-table->list decorated)))
    (with u (ahash-ref decorated-buffers uri)
      (lsp-forget-decorations uri)
      (when (and u (== u (current-buffer))) (update-current-buffer)))))

;; (at the end: the preferences call lsp-decorations-changed when they are
;; defined)
(define-preferences
  ("lsp:semantic colors" "on" (lambda (pref val) (lsp-decorations-changed)))
  ("lsp:inlay hints" "on" (lambda (pref val) (lsp-decorations-changed))))
