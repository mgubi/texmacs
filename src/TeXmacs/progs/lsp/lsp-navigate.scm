;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : lsp-navigate.scm
;; DESCRIPTION : the references of a symbol and the outline of a file
;; COPYRIGHT   : (C) 2026  Massimiliano Gubinelli
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Two pages, as the one of the diagnostics: std U lists the references of
;; the symbol at the cursor (textDocument/references), by file, each with
;; its line; std O shows the outline of the file (textDocument/documentSymbol):
;; its functions, classes, variables..., nested as in the file. A click on
;; a line goes to its place. The outline follows the file while its page
;; is open (it is asked for again when the server has seen a change).
;; std P lists the symbols of the project whose names match (workspace/
;; symbol), std H the callers and the callees of the function at the
;; cursor (the call hierarchy); and the uses of the symbol at the cursor
;; are highlighted when it rests (textDocument/documentHighlight).

(texmacs-module (lsp lsp-navigate)
  (:use (lsp lsp-base)
        (lsp lsp-client)
        (lsp lsp-documents)
        (lsp lsp-edit)))

(define (goto-command uri line ch)
  (string-append "(lsp-goto-position " (object->string uri) " "
                 (number->string line) " " (number->string ch) ")"))

(define (position-of pos)
  ;; the line and the character of the Position @pos
  (list (or (lsp-number-value (lsp-ref pos "line")) 0)
        (or (lsp-number-value (lsp-ref pos "character")) 0)))

(define (trim-line s)
  ;; a line of code, without its indentation
  (string-trim-both s))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; References
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define lsp-max-references 2000)

(define (uri-lines uri)
  ;; the lines of the file of @uri, in Cork, as a vector: those of its
  ;; buffer if it is open, else of the file on disk
  (let* ((p (lsp-uri->path uri))
         (u (and p (system->url p))))
    (list->vector
     (cond ((not u) '())
           ((buffer-exists? u) (lsp-buffer-lines u))
           ((url-exists? u) (lsp-text->lines (string-load u)))
           (else '())))))

(define (file-label uri root)
  ;; the file of @uri, relative to the project @root when it is inside
  (let* ((p (or (lsp-uri->path uri) uri))
         (r (and root (string-append root "/"))))
    (utf8->cork (if (and r (string-starts? p r))
                    (substring p (string-length r) (string-length p))
                    p))))

(define (group-by-file locs)
  ;; the Locations @locs as ((uri loc ...) ...), the files in the order of
  ;; their first reference, and the references of a file by position
  (let loop ((l locs) (files '()) (table (make-ahash-table)))
    (if (null? l)
        (map (lambda (uri)
               (cons uri
                     (sort (reverse (ahash-ref table uri))
                           (lambda (a b)
                             (let ((pa (position-of (lsp-ref a "range" "start")))
                                   (pb (position-of (lsp-ref b "range" "start"))))
                               (or (< (car pa) (car pb))
                                   (and (== (car pa) (car pb))
                                        (< (cadr pa) (cadr pb)))))))))
             (reverse files))
        (let ((uri (lsp-ref (car l) "uri")))
          (if (not (string? uri)) (loop (cdr l) files table)
              (let ((known? (ahash-ref table uri)))
                (ahash-set! table uri (cons (car l) (or known? '())))
                (loop (cdr l) (if known? files (cons uri files)) table)))))))

(tm-define (lsp-references-body name root locs)
  (:synopsis "The page of the references @locs of @name, by file")
  (let* ((files (group-by-file locs))
         (n (length locs)))
    `(document
      (strong (concat "References of " (verbatim ,name) ": "
                      ,(string-append (number->string n)
                                      (if (== n 1) " place" " places"))))
      ,@(if (null? files) '("No references")
            (append-map
             (lambda (f)
               (let* ((uri (car f))
                      (lines (uri-lines uri)))
                 (cons `(concat (em ,(file-label uri root)))
                       (map (lambda (loc)
                              (let* ((p (position-of (lsp-ref loc "range" "start")))
                                     (s (if (< (car p) (vector-length lines))
                                            (vector-ref lines (car p)) "")))
                                `(action (concat ,(number->string (+ (car p) 1))
                                                 ":"
                                                 ,(number->string (+ (cadr p) 1))
                                                 "  "
                                                 (verbatim ,(trim-line s)))
                                         ,(goto-command uri (car p) (cadr p)))))
                            (cdr f)))))
             files)))))

(define (word-at-cursor)
  ;; the word at the cursor, for the title of the page
  (let* ((p (cursor-path))
         (t (path->tree (cDr p))))
    (if (not (tree-atomic? t)) "symbol"
        (let* ((s (tree->string t))
               (o (min (cAr p) (string-length s)))
               (w? (lambda (c) (or (char-alphabetic? c) (char-numeric? c)
                                   (== c #\_))))
               (b (let loop ((i o))
                    (if (and (> i 0) (w? (string-ref s (- i 1))))
                        (loop (- i 1)) i)))
               (e (let loop ((i o))
                    (if (and (< i (string-length s)) (w? (string-ref s i)))
                        (loop (+ i 1)) i))))
          (if (< b e) (substring s b e) "symbol")))))

(tm-define (lsp-references)
  (:synopsis "Lists the references of the symbol at the cursor on a page")
  (let* ((name (word-at-cursor))
         (root (with k (lsp-buffer-client*)
                 (and k (lsp-client-ref k 'root))))
         (r (lsp-request-at-cursor-with
             "textDocument/references" "referencesProvider"
             (lambda (params)
               (append params
                       (list "context"
                             (lsp-object "includeDeclaration"
                                         (lsp-boolean #t))))))))
    (when r
      (let* ((all (lsp-items r))
             (locs (if (> (length all) lsp-max-references)
                       (sublist all 0 lsp-max-references)
                       all)))
        (if (null? locs) (set-message "No references found" "")
            (open-auxiliary "References"
                            `(document (style (tuple "generic"))
                                       (body ,(lsp-references-body
                                               name root locs)))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The outline of a file
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define symbol-kinds
  ;; the SymbolKinds of the protocol, 1 to 26
  #("file" "module" "namespace" "package" "class" "method" "property"
    "field" "constructor" "enum" "interface" "function" "variable"
    "constant" "string" "number" "boolean" "array" "object" "key" "null"
    "enum member" "struct" "event" "operator" "type parameter"))

(tm-define (lsp-symbol-kind-name k)
  (:synopsis "The name of the SymbolKind @k (a number)")
  (if (and (integer? k) (>= k 1) (<= k (vector-length symbol-kinds)))
      (vector-ref symbol-kinds (- k 1))
      "symbol"))

(define (symbol-entries s depth)
  ;; the DocumentSymbol or SymbolInformation @s and its children, as
  ;; (depth kind name detail line character)
  (let* ((kind (lsp-symbol-kind-name (lsp-number-value (lsp-ref s "kind"))))
         (name (or (lsp-ref s "name") ""))
         (detail (or (lsp-ref s "detail") ""))
         (range (or (lsp-ref s "selectionRange") (lsp-ref s "range")
                    (lsp-ref s "location" "range")))
         (p (position-of (and range (lsp-ref range "start")))))
    (cons (list depth kind name (if (string? detail) detail "")
                (car p) (cadr p))
          (append-map (lambda (c) (symbol-entries c (+ depth 1)))
                      (lsp-items (lsp-ref s "children"))))))

(tm-define (lsp-outline-entries r)
  (:synopsis "The entries of the outline for the answer @r of documentSymbol")
  ;; hierarchical DocumentSymbols, or flat SymbolInformations (whose
  ;; containers are not nested here); in the order of the file
  (with l (append-map (lambda (s) (symbol-entries s 0)) (lsp-items r))
    (if (null? (lsp-items r)) '()
        (if (lsp-ref (car (lsp-items r)) "location")
            (sort l (lambda (a b)
                      (or (< (list-ref a 4) (list-ref b 4))
                          (and (== (list-ref a 4) (list-ref b 4))
                               (< (list-ref a 5) (list-ref b 5))))))
            l))))

(define (outline-body c uri entries)
  (with root (and c (lsp-client-ref c 'root))
    `(document
      (strong (concat "Outline of " (verbatim ,(file-label uri root))))
      ,@(if (null? entries) '("No symbols")
            (map (lambda (e)
                   `(action (concat
                             ,(make-string (* 4 (car e)) #\space)
                             (with "color" "#606060" ,(cadr e))
                             " "
                             (strong (verbatim ,(utf8->cork (caddr e))))
                             ,@(if (== (cadddr e) "") '()
                                   (list " "
                                         `(with "color" "#606060"
                                            (verbatim
                                             ,(utf8->cork
                                               (first-line* (cadddr e))))))))
                            ,(goto-command uri (list-ref e 4) (list-ref e 5))))
                 entries)))))

(define (first-line* s)
  (with i (string-search-forwards "\n" 0 s)
    (if (>= i 0) (substring s 0 i) s)))

(define outline-uri #f)        ; the file whose outline is on the page
(define outline-client #f)
(define outline-refresh? #f)

(define (outline-page)
  (aux-name "Outline"))

(define (request-outline c uri handler)
  ;; asks @c for the symbols of @uri; @handler gets the entries, or #f
  (lsp-request c "textDocument/documentSymbol"
               (lsp-object "textDocument" (lsp-object "uri" uri))
               (lambda (r e)
                 (handler (and (not e) r (lsp-outline-entries r))))))

(tm-define (lsp-outline)
  (:synopsis "Shows the outline of the buffer on a page")
  (let ((c (lsp-buffer-client*))
        (uri (lsp-buffer-uri)))
    (cond ((not (and c uri))
           (set-message "No language server for this buffer" ""))
          ((lsp-null? (lsp-ref (lsp-client-capabilities c)
                               "documentSymbolProvider"))
           (set-message "The language server cannot do this" ""))
          (else
           (lsp-buffer-sync)
           (with a (lsp-request-wait
                    c "textDocument/documentSymbol"
                    (lsp-object "textDocument" (lsp-object "uri" uri))
                    5000)
             (if (!= (car a) 'result)
                 (set-message "The language server does not answer" "")
                 (begin
                   (set! outline-uri uri)
                   (set! outline-client c)
                   (open-auxiliary
                    "Outline"
                    `(document (style (tuple "generic"))
                               (body ,(outline-body
                                       c uri (lsp-outline-entries
                                            (cadr a)))))))))))))

(tm-define (lsp-outline-refresh)
  (:synopsis "The page of the outline shows the symbols of the file now")
  (set! outline-refresh? #f)
  (when (and outline-uri outline-client)
    (if (not (and (buffer-exists? (outline-page))
                  (lsp-client-alive? outline-client)))
        (begin (set! outline-uri #f) (set! outline-client #f))
        (with uri outline-uri
          (request-outline
           outline-client uri
           (lambda (entries)
             (when (and entries (== uri outline-uri)
                        (buffer-exists? (outline-page)))
               (buffer-set-body (outline-page)
                                (stree->tree (outline-body outline-client
                                                           uri entries))))))))))

(tm-define (lsp-diagnostics-changed uri)
  ;; (the server publishes its diagnostics once it has seen a change: the
  ;; outline is asked for again then)
  (former uri)
  (when (and (== uri outline-uri) (not outline-refresh?))
    (set! outline-refresh? #t)
    (delayed
      (:pause 300)
      (lsp-outline-refresh))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The uses of the symbol at the cursor, highlighted
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; When the cursor rests in a name, the server is asked where the same
;; symbol is in the file (textDocument/documentHighlight), and those places
;; are highlighted as the matches of a search (the alternative selection
;; lsp-highlights). The highlights go when the cursor leaves them and when
;; the text changes. NOTE: no mode predicates in notify-cursor-moved (see
;; lsp-edit).

(define (highlight-on?) (!= (get-preference "lsp:highlight") "off"))

(define hl-ranges '())         ; (line offset line offset) highlighted
(define hl-buffer #f)
(define hl-scheduled? #f)
(define hl-request #f)         ; (client . id) awaited

(define (cursor-lo)
  ;; the line and offset of the cursor in a source file, or #f
  (let* ((p (cursor-path))
         (b (buffer-path)))
    (and (list-starts? p b)
         (with r (list-tail p (length b))
           (and (= (length r) 2) r)))))

(define (in-range? lo r)
  (let ((l (car lo)) (o (cadr lo)))
    (and (or (> l (car r)) (and (== l (car r)) (>= o (cadr r))))
         (or (< l (caddr r)) (and (== l (caddr r)) (<= o (cadddr r)))))))

(define (in-highlights? lo)
  (and lo (== hl-buffer (current-buffer))
       (list-or (map (lambda (r) (in-range? lo r)) hl-ranges))))

(define (clear-highlights)
  (when (and hl-buffer (== hl-buffer (current-buffer)))
    (cancel-alt-selection "lsp-highlights"))
  (set! hl-ranges '())
  (set! hl-buffer #f))

(tm-define (lsp-highlight-ranges r)
  (:synopsis "The DocumentHighlights @r as (line offset line offset)")
  (map (lambda (h)
         (append (lsp-buffer-line+offset (lsp-ref h "range" "start"))
                 (lsp-buffer-line+offset (lsp-ref h "range" "end"))))
       (lsp-items r)))

(tm-define (lsp-highlights)
  (:synopsis "The ranges highlighted, as (line offset line offset)")
  hl-ranges)

(define (show-highlights ranges)
  (with b (buffer-path)
    (set! hl-ranges ranges)
    (set! hl-buffer (current-buffer))
    (if (null? ranges) (cancel-alt-selection "lsp-highlights")
        (set-alt-selection
         "lsp-highlights"
         (append-map (lambda (r)
                       (list (append b (list (car r) (cadr r)))
                             (append b (list (caddr r) (cadddr r)))))
                     ranges)))))

(tm-define (lsp-highlight-at-cursor)
  (:synopsis "Highlights the uses of the symbol at the cursor")
  (set! hl-scheduled? #f)
  (let* ((c (lsp-buffer-client*))
         (uri (lsp-buffer-uri))
         (lo (cursor-lo))
         (u (current-buffer)))
    (when (and c uri lo (lsp-client-running? c) (lsp-buffer-followed?)
               (not (in-highlights? lo))
               (not (lsp-null? (lsp-ref (lsp-client-capabilities c)
                                        "documentHighlightProvider"))))
      ;; (the server must have the text: the changes which wait are sent)
      (lsp-sync-pending)
      (when hl-request
        (lsp-cancel (car hl-request) (cdr hl-request)))
      (set! hl-request
        (cons c
          (lsp-request
           c "textDocument/documentHighlight"
           (lsp-object "textDocument" (lsp-object "uri" uri)
                       "position" (lsp-buffer-position (car lo) (cadr lo)))
           (lambda (r e)
             (set! hl-request #f)
             ;; (only if the cursor is still there)
             (when (and (== (current-buffer) u) (== (cursor-lo) lo))
               (show-highlights
                (if (or e (not r) (lsp-null? r)) '()
                    (lsp-highlight-ranges r)))))))))))

(tm-define (notify-cursor-moved status)
  (former status)
  (when (and (lsp-on?) (highlight-on?) (lsp-buffer-followed?))
    (when (and (nnull? hl-ranges) (not (in-highlights? (cursor-lo))))
      (clear-highlights))
    (when (not hl-scheduled?)
      (set! hl-scheduled? #t)
      (delayed
        (:idle 300)
        (lsp-highlight-at-cursor)))))

(tm-define (notify-buffer-changed name)
  ;; (an edit removes the highlights: they are asked for again)
  (former name)
  (when (nnull? hl-ranges)
    (set! hl-ranges '())
    (set! hl-buffer #f)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The symbols of the project
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define lsp-max-symbols 500)

(tm-define (lsp-symbols-body query root syms)
  (:synopsis "The page of the symbols @syms of the project which match @query")
  ;; SymbolInformations or WorkspaceSymbols (whose location may have no
  ;; range: the start of the file then)
  (let* ((n (length syms)))
    `(document
      (strong (concat "Symbols matching " (verbatim ,query) ": "
                      ,(string-append (number->string n)
                                      (if (== n 1) " symbol" " symbols"))))
      ,@(if (null? syms) '("No symbols")
            (map (lambda (s)
                   (let* ((uri (or (lsp-ref s "location" "uri") ""))
                          (p (position-of
                              (lsp-ref s "location" "range" "start")))
                          (kind (lsp-symbol-kind-name
                                 (lsp-number-value (lsp-ref s "kind"))))
                          (cont (lsp-ref s "containerName")))
                     `(action (concat
                               (with "color" "#606060" ,kind) " "
                               (strong (verbatim ,(utf8->cork
                                                   (or (lsp-ref s "name") ""))))
                               ,@(if (and (string? cont) (!= cont ""))
                                     (list " " `(with "color" "#606060"
                                                  (verbatim ,(utf8->cork cont))))
                                     '())
                               "  "
                               (em ,(file-label uri root)) ":"
                               ,(number->string (+ (car p) 1)))
                              ,(goto-command uri (car p) (cadr p)))))
                 syms)))))

(tm-define (lsp-find-symbol query)
  (:synopsis "Lists the symbols of the project whose names match @query")
  (:argument query "Symbol")
  (let ((c (lsp-buffer-client*)))
    (cond ((not c) (set-message "No language server for this buffer" ""))
          ((lsp-null? (lsp-ref (lsp-client-capabilities c)
                               "workspaceSymbolProvider"))
           (set-message "The language server cannot do this" ""))
          (else
           (with a (lsp-request-wait c "workspace/symbol"
                                     (lsp-object "query" query) 10000)
             (if (!= (car a) 'result)
                 (set-message "The language server does not answer" "")
                 (let* ((all (lsp-items (cadr a)))
                        (syms (if (> (length all) lsp-max-symbols)
                                  (sublist all 0 lsp-max-symbols)
                                  all)))
                   (open-auxiliary
                    "Symbols"
                    `(document (style (tuple "generic"))
                               (body ,(lsp-symbols-body
                                       query (lsp-client-ref c 'root)
                                       syms)))))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Calls
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (call-line item where uri root)
  ;; a line of the page of the calls: the function @item, at @where (a
  ;; Position) in @uri
  (let ((p (position-of where))
        (kind (lsp-symbol-kind-name (lsp-number-value (lsp-ref item "kind")))))
    `(action (concat (with "color" "#606060" ,kind) " "
                     (strong (verbatim ,(utf8->cork
                                         (or (lsp-ref item "name") ""))))
                     "  " (em ,(file-label uri root)) ":"
                     ,(number->string (+ (car p) 1)))
             ,(goto-command uri (car p) (cadr p)))))

(tm-define (lsp-calls-body item incoming outgoing root)
  (:synopsis "The page of the calls of @item")
  ;; a caller goes to its first call of @item; a callee to its definition
  `(document
    (strong (concat "Calls of " (verbatim ,(utf8->cork
                                            (or (lsp-ref item "name") "")))))
    (em "Called by")
    ,@(if (null? incoming) '("No callers")
          (map (lambda (x)
                 (let ((from (lsp-ref x "from"))
                       (r (and (nnull? (lsp-items (lsp-ref x "fromRanges")))
                               (car (lsp-items (lsp-ref x "fromRanges"))))))
                   (call-line from
                              (if r (lsp-ref r "start")
                                  (lsp-ref from "selectionRange" "start"))
                              (lsp-ref from "uri") root)))
               incoming))
    (em "Calls")
    ,@(if (null? outgoing) '("No calls")
          (map (lambda (x)
                 (with to (lsp-ref x "to")
                   (call-line to (lsp-ref to "selectionRange" "start")
                              (lsp-ref to "uri") root)))
               outgoing))))

(tm-define (lsp-call-hierarchy)
  (:synopsis "Shows the callers and the callees of the function at the cursor")
  (let* ((c (lsp-buffer-client*))
         (r (lsp-request-at-cursor-with
             "textDocument/prepareCallHierarchy" "callHierarchyProvider"
             (lambda (params) params)))
         (item (and r (nnull? (lsp-items r)) (car (lsp-items r)))))
    (cond ((not r) (noop))
          ((not item) (set-message "No function here" ""))
          (else
           (let* ((in (lsp-request-wait c "callHierarchy/incomingCalls"
                                        (lsp-object "item" item) 10000))
                  (out (lsp-request-wait c "callHierarchy/outgoingCalls"
                                         (lsp-object "item" item) 10000))
                  (items (lambda (a)
                           (if (== (car a) 'result) (lsp-items (cadr a)) '()))))
             (open-auxiliary
              "Calls"
              `(document (style (tuple "generic"))
                         (body ,(lsp-calls-body item (items in) (items out)
                                                (lsp-client-ref c 'root))))))))))

(define-preferences
  ("lsp:highlight" "on" (lambda (pref val)
                          (when (== val "off") (clear-highlights)))))
