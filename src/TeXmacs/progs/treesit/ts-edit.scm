;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : ts-edit.scm
;; DESCRIPTION : editing programs with their syntax trees
;; COPYRIGHT   : (C) 2026  Massimiliano Gubinelli
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; The editing functions of prog-edit.scm ask here first: when the code
;; around the cursor has a syntax tree (its program language is highlighted
;; by tree-sitter), the selection grows by syntax nodes and brackets match
;; by the tree, so that those in strings and comments are left alone.
;;
;; The code is a region (Plugins/TreeSitter/syntax_region.cpp): a document
;; of lines or a single string. Positions in it are points (row column), the
;; column being the position in the TeXmacs string of the line. This module
;; only uses the glue, so that loading it does not load the generator.

(texmacs-module (treesit ts-edit)
  ;; (the master of code-syntax-menu, which this module refines)
  (:use (code code-menu)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Points and nodes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (point<? a b)
  (or (< (car a) (car b))
      (and (= (car a) (car b)) (< (cadr a) (cadr b)))))

(define (point<=? a b) (not (point<? b a)))

;; a node is (type named? start end field)
(define (node-type n) (car n))
(define (node-named? n) (cadr n))
(define (node-start n) (caddr n))
(define (node-end n) (cadddr n))

(define-public (syntax-enlarge-range root lan start end)
  ;; the range (start end) of the smallest node which contains start..end
  ;; and is larger, or #f
  (let loop ((l (syntax-region-node-range root lan (car start) (cadr start)
                                          (car end) (cadr end))))
    (and (pair? l)
         (let ((s (node-start (car l))) (e (node-end (car l))))
           (if (and (point<=? s start) (point<=? end e)
                    (or (point<? s start) (point<? end e)))
               (list s e)
               (loop (cdr l)))))))

(define brackets '(("(" . ")") ("[" . "]") ("{" . "}")))

(define (closing-of type) (assoc-ref brackets type))

(define (opening-of type)
  (let ((p (list-find brackets (lambda (b) (== (cdr b) type)))))
    (and p (car p))))

(define (bracket-at root lan p)
  ;; the bracket token which starts at p, with its parent: (node . parent)
  (let ((l (syntax-region-node-at root lan (car p) (cadr p))))
    (and (pair? l) (pair? (cdr l))
         (let ((n (car l)))
           (and (not (node-named? n))
                (== (node-start n) p)
                (or (closing-of (node-type n)) (opening-of (node-type n)))
                (cons n (cadr l)))))))

(define-public (syntax-matching-bracket root lan p)
  ;; when a bracket token of the tree starts at the point p, the point of
  ;; the bracket which matches it, else #f
  (let ((b (bracket-at root lan p)))
    (and b
         (let* ((type (node-type (car b)))
                (parent (cdr b))
                (close (closing-of type))
                (q (if close
                       (let ((e (node-end parent)))
                         (list (car e) (- (cadr e) 1)))
                       (node-start parent)))
                (b* (and (>= (cadr q) 0) (bracket-at root lan q))))
           (and b* (== (node-type (car b*)) (or close (opening-of type)))
                q)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; At the cursor
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (path-context p lan)
  ;; (root-path root row column) for the position p in a string of code
  (and (pair? p) (pair? (cdr p))
       (let* ((leaf (cDr p))
              (t (path->tree leaf)))
         (and (tree-atomic? t)
              (let ((parent (and (pair? leaf) (path->tree (cDr leaf)))))
                (if (and parent (tree-is? parent 'document))
                    (list (cDr leaf) parent (cAr leaf) (cAr p))
                    (list leaf t 0 (cAr p))))))))

(define (context-path ctx point)
  ;; the path of a point of the region of ctx (a point after the last line,
  ;; such as the end of the root node, is the end of that line)
  (let ((root (cadr ctx)))
    (cond ((not (tree-is? root 'document))
           (append (car ctx) (list (min (cadr point) (string-length (tree->string root))))))
          ((< (car point) (tree-arity root))
           (append (car ctx) (list (car point) (cadr point))))
          (else
           (let ((last (- (tree-arity root) 1)))
             (append (car ctx)
                     (list last (string-length (tree->string (tree-ref root last))))))))))

(define-public (syntax-cursor-language)
  ;; the language of tree-sitter of the code at the cursor, or #f
  (and (in-prog?)
       (let ((id (tree-sitter-program-language (get-env "prog-language"))))
         (and (>= id 0) id))))

(define-public (syntax-select-enlarge)
  ;; select the smallest syntax node around the selection (or the cursor);
  ;; #f when the code has no syntax tree
  (let* ((lan (syntax-cursor-language))
         (sel? (selection-active-normal?))
         (a (and lan (path-context (if sel? (selection-get-start) (cursor-path)) lan)))
         (b (and a (path-context (if sel? (selection-get-end) (cursor-path)) lan))))
    (and a b (== (car a) (car b))
         (let ((r (syntax-enlarge-range (cadr a) lan (cddr a) (cddr b))))
           (when r
             (selection-set (context-path a (car r)) (context-path a (cadr r))))
           #t))))

(define (path++ p) (rcons (cDr p) (+ (cAr p) 1)))

(define-public (syntax-select-brackets)
  ;; highlight the bracket at the cursor, or before it, and its match; #f
  ;; when the code has no syntax tree
  (let* ((lan (syntax-cursor-language))
         (ctx (and lan (path-context (cursor-path) lan))))
    (and ctx
         (let* ((root (cadr ctx))
                (p (cddr ctx))
                (p* (list (car p) (- (cadr p) 1)))
                (here (syntax-matching-bracket root lan p))
                (before (and (not here) (>= (cadr p*) 0)
                             (syntax-matching-bracket root lan p*)))
                (from (if here p p*))
                (to (or here before)))
           (if (not to)
               (when (nnull? (get-alt-selection "brackets"))
                 (cancel-alt-selection "brackets"))
               (let ((x (context-path ctx from)) (y (context-path ctx to)))
                 (set-alt-selection "brackets"
                                    (list x (path++ x) y (path++ y)))))
           #t))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Balanced brackets
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; The brackets and the quotes of code stay balanced as they are typed and
;; deleted (preference "prog:balanced brackets", on by default; see
;; bracket-open... in prog/prog-edit.scm): an opening bracket comes with its
;; closing one, a closing bracket is typed over the one which is there, and
;; deleting one of an empty pair deletes both, while deleting a bracket of a
;; pair with content selects the pair (a second deletion removes it). The
;; tree tells the brackets of the code from the characters of strings and
;; comments, which stay plain characters. Text which is pasted or written
;; otherwise is left as it is.

(define (literal-type? type)
  ;; a node of a string or of a comment (by the names of the grammars)
  (or (string-contains? type "string") (string-contains? type "comment")
      (string-contains? type "char") (string-contains? type "heredoc")
      (string-contains? type "regex")))

(define (literal-around root lan p)
  ;; the node of a string or comment which strictly contains the point p
  (list-find (syntax-region-node-range root lan (car p) (cadr p) (car p) (cadr p))
             (lambda (n) (and (literal-type? (node-type n))
                              (point<? (node-start n) p)
                              (point<? p (node-end n))))))

(define (char-at root p)
  ;; the character at the point p, as a string, or ""
  (let* ((s (line-string root (car p))) (c (cadr p)))
    (if (and (>= c 0) (< c (string-length s))) (substring s c (+ c 1)) "")))

(define (word-char? c)
  (and (== (string-length c) 1)
       (let ((ch (string-ref c 0)))
         (or (char-alphabetic? ch) (char-numeric? ch) (== ch #\_)))))

(define (cursor-code)
  ;; (ctx lan) for the code at the cursor, or #f
  (let* ((lan (syntax-cursor-language))
         (ctx (and lan (path-context (cursor-path) lan))))
    (and ctx (list ctx lan))))

(define (move-to ctx p) (go-to (context-path ctx p)))

(define-public (syntax-bracket-open lb rb)
  ;; type the opening bracket or quote lb, whose closing one is rb; #f when
  ;; the code has no syntax tree
  (let ((c (cursor-code)))
    (and c
         (let* ((ctx (car c)) (lan (cadr c)) (root (cadr ctx)) (p (cddr ctx))
                (next (char-at root p)))
           (cond ((selection-active-normal?)
                  (clipboard-cut "temp")
                  (insert-go-to (string-append lb rb) '(1))
                  (clipboard-paste "temp"))
                 ((literal-around root lan p)
                  ;; in a string: its closing quote is typed over
                  (if (and (== lb rb) (== next lb))
                      (move-to ctx (list (car p) (+ (cadr p) 1)))
                      (insert lb)))
                 ((word-char? next) (insert lb))
                 (else (insert-go-to (string-append lb rb) '(1))))
           #t))))

(define-public (syntax-bracket-close lb rb)
  ;; type the closing bracket rb; #f when the code has no syntax tree
  (let ((c (cursor-code)))
    (and c
         (let* ((ctx (car c)) (lan (cadr c)) (root (cadr ctx)) (p (cddr ctx)))
           (if (and (== (char-at root p) rb)
                    (not (literal-around root lan p))
                    (syntax-matching-bracket root lan p))
               (move-to ctx (list (car p) (+ (cadr p) 1)))
               (insert rb))
           #t))))

(define (quote-pair root lan p)
  ;; when a string starts or ends with the quote at p: (start end) of it
  (let ((n (list-find (syntax-region-node-range root lan (car p) (cadr p)
                                                (car p) (+ (cadr p) 1))
                      (lambda (n) (and (string-contains? (node-type n) "string")
                                       (or (== (node-start n) p)
                                           (== (node-end n)
                                               (list (car p) (+ (cadr p) 1)))))))))
    (and n (list (node-start n) (node-end n)))))

(define (pair-at root lan p)
  ;; the pair of brackets or quotes of which the character at p is one:
  ;; (start end) from the opening one to after the closing one, or #f
  (let ((q (syntax-matching-bracket root lan p)))
    (if q
        (let ((a (if (point<? p q) p q)) (b (if (point<? p q) q p)))
          (list a (list (car b) (+ (cadr b) 1))))
        (quote-pair root lan p))))

(define (delete-pair ctx root lan p)
  ;; delete or select the pair of the character at p; whether there was one
  (let ((pair (pair-at root lan p)))
    (and pair
         (let ((a (car pair)) (b (cadr pair)))
           (if (and (== (car a) (car b)) (== (+ (cadr a) 2) (cadr b)))
               ;; empty: both go
               (begin
                 (tree-remove! (tree-ref root (car a)) (cadr a) 2)
                 (move-to ctx a))
               (selection-set (context-path ctx a) (context-path ctx b)))
           #t))))

(define-public (syntax-delete-bracket forward?)
  ;; Backspace (or Delete when forward?) on a bracket or quote of the code:
  ;; #f when there is none there (the usual deletion then applies)
  (let ((c (cursor-code)))
    (and c (not (selection-active-any?))
         (let* ((ctx (car c)) (lan (cadr c)) (root (cadr ctx)) (p (cddr ctx))
                (q (if forward? p (list (car p) (- (cadr p) 1)))))
           (and (>= (cadr q) 0)
                (tree-is? root 'document)
                (delete-pair ctx root lan q))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Keywords and tags which close blocks
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; After Return, a block which the line before opens and nothing closes gets
;; its closing keyword on the next line: the keywords of the grammar which
;; close blocks (misc/tree-sitter/<grammar>/closers.scm, made by
;; gen-closers.py: end, fi, done...) and, in LaTeX, the \end of the last
;; \begin of the line are tried in turn on a copy of the tree, and the first
;; one which leaves fewer errors in the code is inserted. In HTML, the >
;; which ends a start tag inserts its end tag.

(define grammar-closers '())   ; ((grammar . keywords) ...)

(define (closers-of lan)
  (let* ((g (or (program-language-grammar lan) lan))
         (known (assoc g grammar-closers)))
    (if known (cdr known)
        (let* ((u (url-concretize
                   (string-append "$TEXMACS_PATH/misc/tree-sitter/" g "/closers.scm")))
               (l (if (url-exists? u) (string->object (string-load u)) '())))
          (set! grammar-closers (cons (cons g l) grammar-closers))
          l))))

(define (latex-closer line)
  ;; \end{name} for the last \begin{name} of the line, or #f
  (let ((i (string-search-backwards "\\begin{" (string-length line) line)))
    (and (>= i 0)
         (let ((j (string-index line #\} (+ i 7))))
           (and j (string-append "\\end{" (substring line (+ i 7) j) "}"))))))

(define-public (syntax-close-block)
  ;; insert the line which closes the block opened by the line before the
  ;; cursor, when there is a closing keyword which makes fewer errors; #f
  ;; when nothing was inserted
  (let ((c (cursor-code)))
    (and c (tree-is? (cadr (car c)) 'document)
         (let* ((ctx (car c)) (id (cadr c)) (root (cadr ctx)) (p (cddr ctx))
                (lan (get-env "prog-language"))
                (row (car p)) (line (line-string root row))
                (prev (and (> row 0) (line-string root (- row 1))))
                (latex (and prev (== (or (program-language-grammar lan) lan) "latex")
                            (latex-closer prev)))
                (candidates (append (if latex (list latex) '()) (closers-of lan))))
           (and prev (pair? candidates)
                ;; at the end of the line
                (not (first-non-space (string-drop line (min (cadr p) (string-length line)))))
                (let* ((spaces (make-string (line-indent root (- row 1)) #\space))
                       (e0 (syntax-region-errors-with root id row (cadr p) "")))
                  (and (> e0 0)
                       (let loop ((l candidates))
                         (and (pair? l)
                              (let ((n (syntax-region-errors-with
                                        root id row (cadr p)
                                        (string-append "\n" spaces (car l)))))
                                (if (and (>= n 0) (< n e0))
                                    (begin
                                      (tree-insert! root (+ row 1)
                                                    (list (string-append spaces (car l))))
                                      #t)
                                    (loop (cdr l)))))))))))))

(define-public (syntax-close-tag)
  ;; after the > of a start tag of HTML, insert its end tag; #f when there
  ;; is none to insert
  (let ((c (cursor-code)))
    (and c
         (let* ((ctx (car c)) (id (cadr c)) (root (cadr ctx)) (p (cddr ctx))
                (q (list (car p) (- (cadr p) 1)))
                (nodes (and (>= (cadr q) 0)
                            (syntax-region-node-range root id (car q) (cadr q)
                                                      (car q) (cadr q))))
                (tag (and nodes (list-find nodes (lambda (n) (== (node-type n) "start_tag")))))
                (elem (and tag (list-find nodes (lambda (n) (== (node-type n) "element"))))))
           (and tag (== (node-end tag) p)
                (let* ((nm (tag-name (region-text root (node-start tag) (node-end tag))))
                       ;; does the element end with an end tag of its own?
                       (e (and elem (node-end elem)))
                       (closed? (and e (> (cadr e) 0)
                                     (let* ((l (syntax-region-node-range
                                                root id (car e) (- (cadr e) 1)
                                                (car e) (- (cadr e) 1)))
                                            (et (memq (list-find l (lambda (n) (== (node-type n) "end_tag")))
                                                      l)))
                                       (and et (pair? (cdr et))
                                            (== (node-start (cadr et)) (node-start elem))
                                            (== (node-end (cadr et)) e))))))
                  (and nm (not closed?)
                       (not (in? (locase-all nm) html-void-elements))
                       (begin (insert-go-to (string-append "<less>/" nm "<gtr>") '(0))
                              #t))))))))

(define (tag-name text)
  ;; the name of a start tag (in a TeXmacs string: <less>name ...<gtr>)
  (and (string-starts? text "<less>")
       (let* ((s (string-drop text 6))
              (n (string-length s))
              (end (let loop ((i 0))
                     (if (or (= i n) (in? (string-ref s i) '(#\space #\< #\/ #\tab)))
                         i (loop (+ i 1))))))
         (and (> end 0) (substring s 0 end)))))

(define html-void-elements
  '("area" "base" "br" "col" "embed" "hr" "img" "input" "link" "meta"
    "param" "source" "track" "wbr"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Structural edits
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Edits of whole syntax nodes, which keep the brackets balanced: delete a
;; node, swap it with its neighbour, replace the node around it by it
;; (raise), remove the brackets around the cursor (splice), wrap a node in
;; brackets. The ranges come from syntax-region-node-info.

(define (node-info ctx lan)
  ;; ((start end) of: the node, its next and previous siblings, its parent,
  ;; the pair of brackets around the point, the node which ends at the
  ;; point), each #f when there is none
  (let ((v (syntax-region-node-info (cadr ctx) lan (caddr ctx) (cadddr ctx))))
    (and (pair? v)
         (let loop ((v v) (acc '()))
           (if (null? v) (reverse acc)
               (let ((r (list (list (car v) (cadr v)) (list (caddr v) (cadddr v)))))
                 (loop (cddddr v) (cons (and (>= (car v) 0) r) acc))))))))

(define (range-text root a b)
  ;; the text from the point a to the point b of a document of lines
  (if (== (car a) (car b))
      (substring (line-string root (car a)) (cadr a) (cadr b))
      (let loop ((row (+ (car a) 1))
                 (acc (list (string-drop (line-string root (car a)) (cadr a)))))
        (if (= row (car b))
            (apply string-append
                   (list-intersperse (reverse (cons (substring (line-string root row) 0 (cadr b))
                                                    acc))
                                     "\n"))
            (loop (+ row 1) (cons (line-string root row) acc))))))

(define (point-after p text)
  ;; the point after text written from the point p
  (let loop ((l (string->list text)) (row (car p)) (col (cadr p)))
    (cond ((null? l) (list row col))
          ((== (car l) #\newline) (loop (cdr l) (+ row 1) 0))
          (else (loop (cdr l) row (+ col 1))))))

(define (replace-range! root a b text)
  ;; replace the text from a to b of a document of lines by text
  (let* ((pre (substring (line-string root (car a)) 0 (cadr a)))
         (post (string-drop (line-string root (car b)) (cadr b)))
         (new (let ((l (string-decompose (string-append pre text post) "\n")))
                (if (null? l) (list "") l))))
    (tree-set (tree-ref root (car a)) (car new))
    (when (> (car b) (car a))
      (tree-remove! root (+ (car a) 1) (- (car b) (car a))))
    (when (pair? (cdr new))
      (tree-insert! root (+ (car a) 1) (cdr new)))))

(define (with-structure proc)
  ;; call (proc ctx root info) for the code at the cursor, info being the
  ;; ranges of node-info; #f without a tree, or when proc gives #f
  (let ((c (cursor-code)))
    (and c (tree-is? (cadr (car c)) 'document)
         (let ((info (node-info (car c) (cadr c))))
           (and info (proc (car c) (cadr (car c)) info))))))

(define-public (syntax-delete-node forward?)
  ;; delete the node at the cursor (or, unless forward?, the one which ends
  ;; at the cursor)
  (with-structure
    (lambda (ctx root info)
      (let ((n (if forward? (list-ref info 0) (list-ref info 5))))
        (and n
             (begin
               (replace-range! root (car n) (cadr n) "")
               (move-to ctx (car n))
               #t))))))

(define-public (syntax-swap-node forward?)
  ;; swap the node at the cursor with the next (or previous) one; the cursor
  ;; stays at the start of the node, which has moved
  (with-structure
    (lambda (ctx root info)
      (let* ((n (list-ref info 0))
             (m (if forward? (list-ref info 1) (list-ref info 2))))
        (and n m
             (let* ((first (if forward? n m)) (second (if forward? m n))
                    (a (range-text root (car first) (cadr first)))
                    (between (range-text root (cadr first) (car second)))
                    (b (range-text root (car second) (cadr second))))
               (replace-range! root (car first) (cadr second)
                               (string-append b between a))
               (move-to ctx (if forward?
                                (point-after (car first) (string-append b between))
                                (car first)))
               #t))))))

(define-public (syntax-raise-node)
  ;; replace the node around the node at the cursor by it
  (with-structure
    (lambda (ctx root info)
      (let ((n (list-ref info 0)) (parent (list-ref info 3)))
        (and n parent
             (let ((text (range-text root (car n) (cadr n))))
               (replace-range! root (car parent) (cadr parent) text)
               (move-to ctx (car parent))
               #t))))))

(define-public (syntax-splice-brackets)
  ;; remove the innermost pair of brackets around the cursor, keeping what
  ;; is between them
  (with-structure
    (lambda (ctx root info)
      (let ((pair (list-ref info 4)) (p (cddr ctx)))
        (and pair
             (let* ((a (car pair)) (b (cadr pair))
                    (close (list (car b) (- (cadr b) 1))))
               ;; the closing bracket first, the points before it do not move
               (replace-range! root close b "")
               (replace-range! root a (list (car a) (+ (cadr a) 1)) "")
               (move-to ctx (if (== (car p) (car a)) (list (car p) (- (cadr p) 1)) p))
               #t))))))

(define (comment-rows! root rows prefix)
  ;; toggle the line comments of the rows
  (let* ((lines (map (lambda (r) (line-string root r)) rows))
         (new (toggle-line-comments lines prefix)))
    (for-each (lambda (r s) (when (!= s (line-string root r))
                              (tree-set (tree-ref root r) s)))
              rows new)))

(define (whole-lines? root a b)
  ;; does the range a..b cover whole lines (but their indentation)?
  (and (== (first-non-space (line-string root (car a))) (cadr a))
       (not (first-non-space (string-drop (line-string root (car b))
                                          (min (cadr b)
                                               (string-length
                                                (line-string root (car b)))))))))

(define-public (syntax-toggle-comment-node)
  ;; comment the node at the cursor: with line comments when it has whole
  ;; lines, else with a block comment; or uncomment the comment around the
  ;; cursor
  (with-structure
    (lambda (ctx root info)
      (let* ((lan (get-env "prog-language")) (id (syntax-cursor-language))
             (p (cddr ctx))
             (prefix (syntax-comment-prefix lan))
             (block (program-language-block-comment lan))
             (around (list-find (syntax-region-node-range root id (car p) (cadr p)
                                                          (car p) (cadr p))
                                (lambda (n) (string-contains? (node-type n) "comment"))))
             (rows (lambda (a b) (map (lambda (i) (+ (car a) i))
                                      (iota (+ (- (car b) (car a)) 1))))))
        (cond
         (around
          (let* ((a (node-start around)) (b (node-end around))
                 (text (range-text root a b)))
            (cond ((and block (string-starts? text (car block))
                        (string-ends? text (cdr block)))
                   (let* ((inner (substring text (string-length (car block))
                                            (- (string-length text)
                                               (string-length (cdr block)))))
                          (inner (if (string-starts? inner " ") (string-drop inner 1) inner))
                          (inner (if (string-ends? inner " ") (string-drop-right inner 1) inner)))
                     (replace-range! root a b inner)
                     (move-to ctx a)
                     #t))
                  (prefix
                   ;; the lines commented around it
                   (let* ((commented? (lambda (r)
                                        (and (>= r 0) (< r (tree-arity root))
                                             (string-starts?
                                              (string-drop (line-string root r)
                                                           (or (first-non-space
                                                                (line-string root r)) 0))
                                              prefix))))
                          (first (let up ((r (car a)))
                                   (if (commented? (- r 1)) (up (- r 1)) r)))
                          (last (let down ((r (car b)))
                                  (if (commented? (+ r 1)) (down (+ r 1)) r))))
                     (comment-rows! root (rows (list first 0) (list last 0)) prefix)
                     #t))
                  (else #f))))
         ((list-ref info 0)
          => (lambda (n)
               (let ((a (car n)) (b (cadr n)))
                 (cond ((and prefix (or (whole-lines? root a b) (not block)))
                        (comment-rows! root (rows a b) prefix) #t)
                       (block
                        (replace-range! root a b
                                        (string-append (car block) " "
                                                       (range-text root a b)
                                                       " " (cdr block)))
                        (move-to ctx a)
                        #t)
                       (else #f)))))
         (else #f))))))

(define-public (syntax-wrap-node lb rb)
  ;; put the node at the cursor between the brackets lb and rb
  (with-structure
    (lambda (ctx root info)
      (let ((n (list-ref info 0)))
        (and n
             (begin
               (replace-range! root (cadr n) (cadr n) rb)
               (replace-range! root (car n) (car n) lb)
               (move-to ctx (list (car (car n)) (+ (cadr (car n)) 1)))
               #t))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Folds
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; A fold hides the lines of a syntax node after its first one, which gets
;; a mark; the last line stays when it is only the closing token of the
;; node (}, end...). Folds are a state of the view, kept on the hidden lines
;; by the core (progs/code/code-fold.scm, System/Language/code_lines.cpp),
;; not markup: the document, its export and the editing of programs do not
;; see them, nor what is printed. The state of a line is the number of the
;; folds which hide it: folds nest, and unfolding one keeps those inside it.
;; The line before hidden lines gets the mark (Typeset/Bridge/bridge.cpp).
;; Modifying a hidden line forgets its state, and the cursor arriving in a
;; hidden line unfolds the folds around it (Edit/Interface/edit_cursor.cpp).

;; (the folds of the core, progs/code/code-fold.scm, which the language
;; servers fold too: the number of the folds which hide a line, a fold
;; starting at a line when the next one is deeper)
(define (depth root row) (code-fold-depth root row))
(define (hidden? root row) (code-hidden? root row))
(define (fold-starts? root row)
  (> (depth root (+ row 1)) (depth root row)))

(define (fold-rows root lan n)
  ;; the rows which a fold of the node n hides
  (let* ((s (car (node-start n))) (e (car (node-end n)))
         (c (first-non-space (line-string root e)))
         (leaf (and c (syntax-region-node-at root lan e c)))
         ;; the line is only the token which ends the node (}, end...)
         (closing? (and (pair? leaf)
                        (not (node-named? (car leaf)))
                        (== (node-end (car leaf)) (node-end n))))
         (last (if closing? (- e 1) e)))
    (if (< last (+ s 1)) '() (map (lambda (i) (+ s 1 i)) (iota (- last s))))))

(define (fold-node! root lan n)
  ;; fold the node n (not when a fold starts already at its first line);
  ;; whether there was something to hide
  (let ((rows (fold-rows root lan n)) (s (car (node-start n))))
    (and (pair? rows) (code-fold-lines root s (cAr rows)))))

(define (unfold-at! root row)
  ;; unfold the fold which starts at row; whether there was one
  (code-unfold-lines root row))

(define (rows->range s rows)
  (and (pair? rows) (list s (cAr rows))))

(define-public (syntax-fold-ranges)
  ;; the blocks which can be folded around the cursor, as (first last):
  ;; the nodes of several lines, without the line which closes them (for
  ;; the folds of the core, progs/code/code-fold.scm); #f without a tree
  (let ((c (cursor-code)))
    (and c (tree-is? (cadr (car c)) 'document)
         (let* ((ctx (car c)) (lan (cadr c)) (root (cadr ctx)) (p (cddr ctx)))
           (list-filter
            (map (lambda (n)
                   (and (< (car (node-start n)) (car (node-end n)))
                        (rows->range (car (node-start n))
                                     (fold-rows root lan n))))
                 (syntax-region-node-range root lan (car p) (cadr p)
                                           (car p) (cadr p)))
            identity)))))

(define (node-range root lan n)
  ;; the block (first last) of the node n of several lines, or #f
  (and (< (car (node-start n)) (car (node-end n)))
       (rows->range (car (node-start n)) (fold-rows root lan n))))

(define (top-level-nodes ctx root lan)
  ;; the nodes of several lines which start a line, at the top level
  (list-filter
   (map (lambda (row)
          (let* ((c (first-non-space (line-string root row)))
                 (info (and c (node-info (list (car ctx) root row c) lan))))
            (and info (list-ref info 0) (not (list-ref info 3))
                 (< (car (car (list-ref info 0))) (car (cadr (list-ref info 0))))
                 (let ((n (list-ref info 0)))
                   (list "" #t (car n) (cadr n))))))
        (iota (tree-arity root)))
   identity))

(define-public (syntax-fold-all-ranges)
  ;; the definitions of several lines of the code at the cursor, as blocks
  ;; (first last), or, in a language without definitions (tags.scm), its
  ;; blocks at the top level; #f without a tree
  (let ((o (cursor-outline)))
    (and o
         (let* ((ctx (car o)) (lan (cadr o)) (root (cadr ctx))
                (nodes (if (pair? (caddr o))
                           (map (lambda (d) (list "" #t (list-ref d 4) (cadddr d)))
                                (caddr o))
                           (top-level-nodes ctx root lan))))
           (list-filter (map (lambda (n) (node-range root lan n)) nodes)
                        identity)))))

(define-public (syntax-fold-block)
  ;; fold the innermost node of several lines around the cursor; #f when
  ;; there is none
  (let ((c (cursor-code)))
    (and c (tree-is? (cadr (car c)) 'document)
         (let* ((ctx (car c)) (lan (cadr c)) (root (cadr ctx)) (p (cddr ctx))
                (n (list-find (syntax-region-node-range root lan (car p) (cadr p)
                                                        (car p) (cadr p))
                              (lambda (n) (and (< (car (node-start n))
                                                  (car (node-end n)))
                                               (pair? (fold-rows root lan n)))))))
           (and n (fold-node! root lan n)
                (begin (move-to ctx (node-start n)) #t))))))

(define-public (syntax-unfold-block)
  ;; unfold the fold which starts at the line of the cursor; #f when there
  ;; is none
  (let ((c (cursor-code)))
    (and c (tree-is? (cadr (car c)) 'document)
         (unfold-at! (cadr (car c)) (car (cddr (car c)))))))

(define-public (syntax-toggle-fold)
  ;; unfold the fold which starts at the line of the cursor, or fold the
  ;; innermost node of several lines around the cursor
  (or (syntax-unfold-block) (syntax-fold-block)))

(define-public (syntax-fold-definitions)
  ;; fold all the definitions of several lines of the code at the cursor,
  ;; or, in a language without definitions (tags.scm), its blocks at the
  ;; top level
  (let ((o (cursor-outline)))
    (and o
         (let* ((ctx (car o)) (lan (cadr o)) (root (cadr ctx)))
           (if (pair? (caddr o))
               (for-each (lambda (d)
                           (let ((n (list "" #t (list-ref d 4) (cadddr d))))
                             (when (< (car (node-start n)) (car (node-end n)))
                               (fold-node! root lan n))))
                         (caddr o))
               (for-each
                (lambda (row)
                  (let* ((c (first-non-space (line-string root row)))
                         (info (and c (not (hidden? root row))
                                    (node-info (list (car ctx) root row c) lan))))
                    ;; the node which starts the line, whose parent is the root
                    (when (and info (list-ref info 0) (not (list-ref info 3))
                               (< (car (car (list-ref info 0)))
                                  (car (cadr (list-ref info 0)))))
                      (let ((n (list-ref info 0)))
                        (fold-node! root lan (list "" #t (car n) (cadr n)))))))
                (iota (tree-arity root))))
           #t))))

(define-public (syntax-unfold-all)
  ;; unfold all the folds of the code at the cursor
  (let ((c (cursor-code)))
    (and c (tree-is? (cadr (car c)) 'document)
         (let ((root (cadr (car c))))
           (let loop ()
             (with fs (code-folds root)
               (when (nnull? fs)
                 (for-each (lambda (f) (unfold-at! root (car f))) fs)
                 (loop))))
           #t))))

;; (the cursor which comes into a hidden line: code-cursor-in-fold of the
;; core, called by Edit/Interface/edit_cursor.cpp)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Inputs of sessions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (text-end body)
  ;; the point after the last character which is not a space
  (let loop ((row (- (tree-arity body) 1)))
    (if (< row 0) '(0 0)
        (let* ((line (tree-ref body row))
               (s (if (tree-atomic? line) (tree->string line) ""))
               (n (let trim ((n (string-length s)))
                    (if (and (> n 0) (char-whitespace? (string-ref s (- n 1))))
                        (trim (- n 1))
                        n))))
          (if (> n 0) (list row n) (loop (- row 1)))))))

(define-public (syntax-incomplete? body lan)
  ;; whether the program of body (a document of lines) has an error which
  ;; reaches its end (an open bracket), or ends with an empty node (the
  ;; block after a line which starts one, in Python): then only more lines
  ;; can complete it
  (and (tree-is? body 'document)
       (let ((end (text-end body)))
         (and (not (== end '(0 0)))
              (or (list-or (map (lambda (e) (point<=? end (caddr e)))
                                (syntax-region-errors body lan)))
                  (let ((l (syntax-region-last-nodes body lan)))
                    (and (pair? l)
                         (list-or (map (lambda (n)
                                         (and (node-named? n)
                                              (== (node-start n) (node-end n))))
                                       (cdr l))))))))))

(define-public (syntax-input-incomplete? body)
  ;; the same for the input of a session, in the language at the cursor
  (let ((id (tree-sitter-program-language (get-env "prog-language"))))
    (and (>= id 0) (syntax-incomplete? body id))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Indentation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; The indentation of a line comes from the query indents.scm of the grammar
;; (those of nvim-treesitter, see syntax_region_indent in
;; Plugins/TreeSitter/syntax_region.cpp), or else from rules: a line is
;; indented one step more than the line where the innermost node of an
;; indenting type which is still open around it starts; as much as that line
;; when it starts with the token which closes that node (}, ), end) or with a
;; branch (else...). The rules of a program language: the indenting types,
;; the branches, and for an offside language (whose blocks end where the
;; indentation goes back) the types of its brackets: only those are closed
;; before a line; optionally, whether a blank line after a named token stays
;; at the indentation of the indenting node around it (YAML: after key:
;; value, but not after key: or [1,).

(define syntax-indent-rules
  ;; the languages whose grammars have no query indents.scm (or a weaker
  ;; one: that of Neovim for YAML only indents after a block); for Haskell,
  ;; a rule of layout (see layout-indentation)
  `(("yaml" ("block_mapping_pair" "block_sequence_item" "flow_mapping" "flow_sequence")
     () ("flow_mapping" "flow_sequence") #t)
    ("latex"
     ("generic_environment" "math_environment" "curly_group" "brack_group")
     ("end") #f)
    ("haskell" layout
     ("where" "do" "of" "let" "=" "->" "then" "else" "(" "[" "{" "," "|"))
    ("lean"
     ("def" "theorem" "instance" "abbrev" "example" "structure" "inductive"
      "by" "do_block" "match" "match_alt" "fun" "have" "let" "where_decls"
      "if_then_else" "tactic_case" "paren" "anon_ctor" "list_lit" "array_lit"
      "struct_lit")
     ()
     ("paren" "anon_ctor" "list_lit" "array_lit" "struct_lit"))))

(define (line-string doc row)
  (let ((t (tree-ref doc row)))
    (if (and t (tree-atomic? t)) (tree->string t) "")))

(define (line-indent doc row)
  (let* ((s (line-string doc row)) (n (string-length s)))
    (let loop ((i 0))
      (if (and (< i n) (== (string-ref s i) #\space)) (loop (+ i 1)) i))))

(define (first-non-space s)
  (let ((n (string-length s)))
    (let loop ((i 0))
      (cond ((= i n) #f)
            ((char-whitespace? (string-ref s i)) (loop (+ i 1)))
            (else i)))))

(define (previous-text-end doc row)
  ;; the point after the last character which is not a space, before row
  (let loop ((r (- row 1)))
    (if (< r 0) #f
        (let* ((s (line-string doc r))
               (n (let trim ((n (string-length s)))
                    (if (and (> n 0) (char-whitespace? (string-ref s (- n 1))))
                        (trim (- n 1))
                        n))))
          (if (> n 0) (list r n) (loop (- r 1)))))))

;; The queries of the grammars (misc/tree-sitter/<grammar>/<file>), compiled
;; once for each language of tree-sitter

(define grammar-queries '())  ; (((id . file) . query or #f) ...)

(define (grammar-query lan id file)
  ;; the query file of the grammar of the program language lan, or #f
  (let ((known (assoc (cons id file) grammar-queries)))
    (if known (cdr known)
        (let* ((g (or (program-language-grammar lan) lan))
               (u (url-concretize
                   (string-append "$TEXMACS_PATH/misc/tree-sitter/" g "/" file)))
               (q (and (url-exists? u)
                       (tree-sitter-query-new id (string-load u))))
               (r (and q (>= q 0) q)))
          (set! grammar-queries (cons (cons (cons id file) r) grammar-queries))
          r))))

(define-public (syntax-indentation doc row lan rules tab)
  ;; the indentation of the line row of doc (a document of lines), by the
  ;; rules of its language (an entry of syntax-indent-rules)
  (let* ((indenting (cadr rules))
         (branches (caddr rules))
         (brackets (cadddr rules))
         (complete-stays? (let ((l (list-tail rules 4))) (and (pair? l) (car l))))
         (s (line-string doc row))
         (c0 (first-non-space s))
         (prev (and (not c0) (previous-text-end doc row)))
         (p (cond (c0 (list row c0))
                  (prev (list (car prev) (- (cadr prev) 1)))
                  (else #f))))
    (if (not p) 0
        (let* ((nodes (syntax-region-node-range doc lan (car p) (cadr p)
                                                (car p) (cadr p)))
               (errors (if prev (syntax-region-errors doc lan) '()))
               (closed? (lambda (n)
                          ;; complete before the (blank) line
                          (and prev
                               (point<=? (node-end n) prev)
                               (or (not brackets) (in? (node-type n) brackets))
                               (not (list-or
                                     (map (lambda (e)
                                            (and (point<=? (node-start n) (cadr e))
                                                 (point<=? (caddr e) (node-end n))))
                                          errors))))))
               ;; after an unfinished construct (an unclosed bracket),
               ;; error recovery leaves an ERROR node instead of its node
               (a (list-find nodes
                             (lambda (n)
                               (and (or (in? (node-type n) indenting)
                                        (and prev (== (node-type n) "ERROR")))
                                    (< (car (node-start n)) row)
                                    (not (closed? n)))))))
          (if (not a) 0
              (let* ((n0 (car nodes))
                     (parent (and (pair? (cdr nodes)) (cadr nodes)))
                     (closing? (and c0 (not (node-named? n0))
                                    (== (node-start n0) p)
                                    (== parent a)
                                    (== (node-end n0) (node-end a))))
                     (first (let loop ((l nodes) (best #f))
                              (if (and (pair? l) (== (node-start (car l)) p))
                                  (loop (cdr l) (car l))
                                  best)))
                     (branch? (and c0 first (in? (node-type first) branches)))
                     ;; a blank line after a which has its last part
                     (complete? (and prev complete-stays? (node-named? n0))))
                (+ (line-indent doc (car (node-start a)))
                   (if (or closing? branch? complete?) 0 tab))))))))

(define-public (syntax-language-indentation doc row lan id tab)
  ;; the same for the program language lan, whose language of tree-sitter
  ;; is id: by the query indents.scm of its grammar (those of Neovim, see
  ;; syntax_region_indent in Plugins/TreeSitter/syntax_region.cpp) or by its
  ;; rules; #f without either
  (and (tree-is? doc 'document)
       (let ((q (grammar-query lan id "indents.scm")))
         (if q
             (let ((r (syntax-region-indent doc id q row tab)))
               (cond ((>= r 0) r)
                     ((= r -1)   ; @indent.auto: as the previous line
                      (let ((p (previous-text-end doc row)))
                        (if p (line-indent doc (car p)) 0)))
                     (else #f)))
             (let ((rules (assoc lan syntax-indent-rules)))
               (cond ((not rules) #f)
                     ((== (cadr rules) 'layout)
                      (layout-indentation doc row id (caddr rules) tab))
                     (else (syntax-indentation doc row id rules tab))))))))

(define (layout-indentation doc row lan openers tab)
  ;; for a language of layout (Haskell): one step more than the line before
  ;; when that line ends with an opener (where, do, =, an open bracket...),
  ;; else as much as it (a comment which ends the line does not count)
  (let ((p (previous-text-end doc row)))
    (if (not p) 0
        (let* ((r (car p))
               (s (line-string doc r))
               (end (let ((l (syntax-region-node-at doc lan r (- (cadr p) 1))))
                      ;; before a comment at the end of the line
                      (if (and (pair? l) (string-contains? (node-type (car l)) "comment")
                               (== (car (node-start (car l))) r)
                               (> (cadr (node-start (car l))) 0))
                          (cadr (node-start (car l)))
                          (cadr p))))
               (text (let trim ((n end))
                       (if (and (> n 0) (char-whitespace? (string-ref s (- n 1))))
                           (trim (- n 1))
                           (substring s 0 n))))
               (opens? (list-or
                        (map (lambda (o)
                               (and (string-ends? text o)
                                    ;; a word ends there, not a part of one
                                    (let ((k (- (string-length text) (string-length o))))
                                      (or (= k 0)
                                          (not (char-alphabetic? (string-ref o 0)))
                                          (not (char-alphabetic?
                                                (string-ref text (- k 1))))))))
                             openers))))
          (+ (line-indent doc r) (if opens? tab 0))))))

(define-public (syntax-compute-indentation doc row tab)
  ;; the indentation of a line of the program at the cursor, or #f when
  ;; its language has no syntax tree or no rules
  (let* ((lan (get-env "prog-language"))
         (id (tree-sitter-program-language lan)))
    (and (>= id 0) (syntax-language-indentation doc row lan id tab))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Outline: the definitions of a program
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; The definitions are the captures @definition.<kind> of the query
;; misc/tree-sitter/<language>/tags.scm, as in tree-sitter-tags, each with
;; the first capture @name inside it.

(define (tags-query lan id)
  ;; the query of the definitions of the program language lan, or #f
  (grammar-query lan id "tags.scm"))

(define (region-text root start end)
  ;; the text between two points of the same line
  (let ((line (if (tree-is? root 'document) (tree-ref root (car start)) root)))
    (if (and line (tree-atomic? line) (= (car start) (car end)))
        (let ((s (tree->string line)))
          (substring s (min (cadr start) (string-length s))
                     (min (cadr end) (string-length s))))
        "")))

(define-public (syntax-outline root lan id)
  ;; the definitions of a region of the program language lan (language id
  ;; of tree-sitter): (kind name start end def-start) in the order of the
  ;; text, kind being "function", "class"..., start the position of the name
  ;; and def-start..end the range of the definition
  (let ((q (tags-query lan id)))
    (if (not q) '()
        (let* ((names (tree-sitter-query-capture-names q))
               (caps (syntax-region-captures root id q))
               (cap-name (lambda (c) (list-ref names (car c))))
               (defs (list-filter caps (lambda (c) (string-starts? (cap-name c)
                                                                   "definition."))))
               (nms (list-filter caps (lambda (c) (== (cap-name c) "name")))))
          (let loop ((l defs) (acc '()))
            (if (null? l) (reverse acc)
                (let* ((d (car l))
                       (n (list-find nms (lambda (n) (and (point<=? (cadr d) (cadr n))
                                                          (point<=? (caddr n) (caddr d))))))
                       (item (and n (list (string-drop (cap-name d) 11)
                                          (region-text root (cadr n) (caddr n))
                                          (cadr n) (caddr d) (cadr d)))))
                  ;; (not twice the same name, nor a definition of the same
                  ;; kind and name just after itself: the equations of a
                  ;; function of Haskell are definitions of their own)
                  (loop (cdr l)
                        (if (and item
                                 (not (list-find acc (lambda (x)
                                                       (== (caddr x) (caddr item)))))
                                 (not (and (pair? acc)
                                           (== (car (car acc)) (car item))
                                           (== (cadr (car acc)) (cadr item)))))
                            (cons item acc)
                            acc)))))))))

(define (cursor-outline)
  ;; (ctx id definitions) for the code at the cursor, or #f
  (let* ((lan (get-env "prog-language"))
         (id (syntax-cursor-language))
         (ctx (and id (path-context (cursor-path) id))))
    (and ctx (list ctx id (syntax-outline (cadr ctx) lan id)))))

(define-public (syntax-errors-at-cursor)
  ;; the errors and the missing nodes of the code at the cursor, as items
  ;; (error start end message) of progs/code/code-diagnostics.scm (an
  ;; empty node covers a character); #f without a syntax tree
  (let ((c (cursor-code)))
    (and c (tree-is? (cadr (car c)) 'document)
         (let* ((ctx (car c)) (lan (cadr c)) (root (cadr ctx)))
           (map (lambda (e)
                  (let* ((s (cadr e)) (en (caddr e))
                         (len (string-length (line-string root (car en))))
                         (en* (if (and (== s en) (< (cadr en) len))
                                  (list (car en) (+ (cadr en) 1))
                                  en)))
                    (list 'error (context-path ctx s) (context-path ctx en*)
                          (if (== (car e) "missing") "Missing in the syntax"
                              "Syntax error"))))
                (syntax-region-errors root lan))))))

(define-public (syntax-context-at-cursor)
  ;; where the cursor is in the code: string, comment or code (the node of
  ;; the character before it); #f without a syntax tree
  (let ((c (cursor-code)))
    (and c
         (let* ((ctx (car c)) (lan (cadr c)) (root (cadr ctx)) (p (cddr ctx))
                (col (max 0 (- (cadr p) 1)))
                (l (syntax-region-node-at root lan (car p) col))
                (types (if (pair? l) (map node-type l) '()))
                (has? (lambda (w) (list-find types (lambda (t) (string-contains? t w))))))
           (cond ((has? "comment") 'comment)
                 ((has? "string") 'string)
                 (else 'code))))))

(define-public (syntax-local-names)
  ;; the names of the definitions of the code at the cursor; #f without a
  ;; syntax tree
  (let ((o (cursor-outline)))
    (and o (map cadr (caddr o)))))

(define-public (syntax-outline-at-cursor)
  ;; the definitions of the code at the cursor as entries of an outline
  ;; (level kind name detail path), for progs/code/code-navigate.scm; #f
  ;; without a syntax tree
  (let ((o (cursor-outline)))
    (and o
         (map (lambda (d) (list 0 (car d) (cadr d) ""
                                (context-path (car o) (caddr d))))
              (caddr o)))))

(define-public (syntax-go-to-point ctx point)
  (go-to (context-path ctx point)))

(define-public (syntax-definitions-at-cursor)
  ;; the definitions of the code at the cursor: (kind name path)
  (let ((o (cursor-outline)))
    (if (not o) '()
        (map (lambda (d) (list (car d) (cadr d) (context-path (car o) (caddr d))))
             (caddr o)))))

(define-public (syntax-go-to-definition)
  ;; go to the definition of the name at the cursor in the same code; #f
  ;; when there is none
  (let ((o (cursor-outline)))
    (and o
         (let* ((ctx (car o))
                (l (syntax-region-node-at (cadr ctx) (cadr o) (caddr ctx) (cadddr ctx)))
                (n (and (pair? l) (node-named? (car l)) (car l)))
                (name (and n (region-text (cadr ctx) (node-start n) (node-end n))))
                (d (and name (list-find (caddr o) (lambda (d) (== (cadr d) name))))))
           (and d (begin (go-to (context-path ctx (caddr d))) #t))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Structured moves
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; The structured moves of TeXmacs (traverse-left... in prog-edit.scm) go
;; through the syntax tree: to the next or previous node, to the node around,
;; into it, and from definition to definition.

(define-public (syntax-move-node dir)
  ;; go to the next node (dir 1), the previous one (-1), the node around
  ;; (2) or the first one inside (-2), see syntax_region_node_move; #f when
  ;; the code has no syntax tree or there is no such node
  (let* ((lan (syntax-cursor-language))
         (ctx (and lan (path-context (cursor-path) lan)))
         (r (and ctx (syntax-region-node-move (cadr ctx) lan (caddr ctx) (cadddr ctx) dir))))
    (and (pair? r)
         (begin (go-to (context-path ctx r)) #t))))

(define-public (syntax-move-definition dir)
  ;; go to the name of the next (dir 1) or previous (-1) definition; #f
  ;; when there is none
  (let* ((o (cursor-outline))
         (ctx (and o (car o)))
         (p (and ctx (cddr ctx)))
         (defs (if o (map caddr (caddr o)) '()))
         (d (if (> dir 0)
                (list-find defs (lambda (q) (point<? p q)))
                (let ((before (list-filter defs (lambda (q) (point<? q p)))))
                  (and (pair? before) (cAr before))))))
    (and d (begin (go-to (context-path ctx d)) #t))))

(define-public (syntax-select-definition)
  ;; select the innermost definition around the cursor; #f when there is
  ;; none
  (let* ((o (cursor-outline))
         (ctx (and o (car o)))
         (p (and ctx (cddr ctx)))
         (around (if o (list-filter (caddr o)
                                    (lambda (d) (and (point<=? (list-ref d 4) p)
                                                     (point<=? p (cadddr d)))))
                     '())))
    (and (pair? around)
         (let ((d (cAr around)))   ; the innermost starts last
           (selection-set (context-path ctx (list-ref d 4))
                          (context-path ctx (cadddr d)))
           #t))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Comments
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-public (syntax-comment-prefix lan)
  ;; the start of a line comment (tm-program-languages.scm)
  (program-language-comment lan))

(define-public (toggle-line-comments lines prefix)
  ;; comment the strings lines with prefix, or uncomment them when they all
  ;; are (blank lines aside); the new lines
  (let* ((full (list-filter lines (lambda (s) (first-non-space s))))
         (commented? (lambda (s) (let ((i (first-non-space s)))
                                   (string-starts? (substring s i (string-length s))
                                                   prefix))))
         (all? (and (nnull? full) (list-and (map commented? full))))
         (indent (if (null? full) 0 (apply min (map first-non-space full))))
         (k (string-length prefix)))
    (map (lambda (s)
           (let ((i (first-non-space s)))
             (cond ((not i) s)
                   (all?
                    (let* ((j (+ i k))
                           (j* (if (and (< j (string-length s))
                                        (== (string-ref s j) #\space))
                                   (+ j 1) j)))
                      (string-append (substring s 0 i)
                                     (substring s j* (string-length s)))))
                   (else
                    (string-append (substring s 0 indent) prefix " "
                                   (substring s indent (string-length s)))))))
         lines)))

(define-public (toggle-block-comments lines block)
  ;; the same with block comments, one for each line, for the languages
  ;; without line comments: block is (start . end)
  (let* ((b (car block)) (e (cdr block))
         (full (list-filter lines (lambda (s) (first-non-space s))))
         (body (lambda (s) (string-drop s (first-non-space s))))
         (commented? (lambda (s) (let ((t (body s)))
                                   (and (string-starts? t b) (string-ends? t e)
                                        (>= (string-length t)
                                            (+ (string-length b) (string-length e)))))))
         (all? (and (nnull? full) (list-and (map commented? full)))))
    (map (lambda (s)
           (let ((i (first-non-space s)))
             (cond ((not i) s)
                   (all?
                    (let* ((t (body s))
                           (in (substring t (string-length b)
                                          (- (string-length t) (string-length e))))
                           (in (if (string-starts? in " ") (string-drop in 1) in))
                           (in (if (string-ends? in " ") (string-drop-right in 1) in)))
                      (string-append (substring s 0 i) in)))
                   (else
                    (string-append (substring s 0 i) b " " (body s) " " e)))))
         lines)))

(define-public (syntax-comment-any lan)
  ;; whether the lines of lan can be commented (line or block comments)
  (or (syntax-comment-prefix lan) (program-language-block-comment lan)))

(define-public (syntax-toggle-comment)
  ;; comment or uncomment the lines of the selection, or the line of the
  ;; cursor, in program code, with line comments or else block comments; #f
  ;; when its language has neither
  (let* ((lan (get-env "prog-language"))
         (prefix (syntax-comment-prefix lan))
         (block (program-language-block-comment lan))
         (sel? (selection-active-normal?))
         (a (and (or prefix block)
                 (path-context (if sel? (selection-get-start) (cursor-path)) 0)))
         (b (and a (path-context (if sel? (selection-get-end) (cursor-path)) 0))))
    (and a b (== (car a) (car b)) (tree-is? (cadr a) 'document)
         (let* ((root (cadr a))
                (rows (map (lambda (i) (+ i (caddr a)))
                           (iota (+ (- (caddr b) (caddr a)) 1))))
                (lines (map (lambda (r) (tree->string (tree-ref root r))) rows))
                (new (if prefix (toggle-line-comments lines prefix)
                         (toggle-block-comments lines block))))
           (for-each (lambda (r s) (when (!= s (tree->string (tree-ref root r)))
                                     (tree-set (tree-ref root r) s)))
                     rows new)
           #t))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Menu
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-menu (syntax-definitions-menu)
  (for (d (syntax-definitions-at-cursor))
    ((eval (string-append (cadr d) " (" (car d) ")"))
     (go-to (caddr d)))))

(tm-menu (code-syntax-menu)
  ;; (after the entries of the features of program code, which come from
  ;; tree-sitter or from a server: progs/code/code-menu.scm)
  (:require (and (in-prog?) (syntax-cursor-language)))
  ("Previous definition" (syntax-move-definition -1))
  ("Next definition" (syntax-move-definition 1))
  ("Select definition" (syntax-select-definition))
  (-> "Structure"
      ("Delete node" (syntax-delete-node #t))
      ("Swap with next" (syntax-swap-node #t))
      ("Swap with previous" (syntax-swap-node #f))
      ("Raise node" (syntax-raise-node))
      ("Remove brackets" (syntax-splice-brackets))
      ("Comment or uncomment node" (syntax-toggle-comment-node))
      ("Wrap in parentheses" (syntax-wrap-node "(" ")"))
      ("Wrap in brackets" (syntax-wrap-node "[" "]"))
      ("Wrap in braces" (syntax-wrap-node "{" "}")))
  (assuming (syntax-comment-any (get-env "prog-language"))
    ("Toggle comment" (syntax-toggle-comment))))
