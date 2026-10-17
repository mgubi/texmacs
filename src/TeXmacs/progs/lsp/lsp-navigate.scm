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
