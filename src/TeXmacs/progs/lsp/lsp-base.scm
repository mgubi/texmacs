;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : lsp-base.scm
;; DESCRIPTION : messages, texts and positions of the Language Server Protocol
;; COPYRIGHT   : (C) 2026  Massimiliano Gubinelli
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; The messages are the strees of the JSON converter (json->typed-tree):
;; objects (attr key value ...), arrays (tuple ...), numbers (json-number
;; "1"), booleans (json-boolean "true"), null (json-null), and strings, in
;; utf8. The typed variant keeps the numbers apart from the strings: the
;; answer to a request of a server has the id of the request, number or
;; string.
;;
;; A text is a list of lines, in Cork as in the documents of TeXmacs (a
;; source file is a document with a line in each paragraph); the server
;; gets them in utf8, and counts the characters of a position in UTF-16
;; units (two for the characters beyond U+FFFF), the default encoding of
;; the protocol.

(texmacs-module (lsp lsp-base))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Messages
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (lsp-read s)
  (:synopsis "The message @s (JSON), as an stree, or #f")
  (and (string? s) (!= s "")
       (catch #t
         (lambda () (tree->stree (json->typed-tree s)))
         (lambda args #f))))

(tm-define (lsp-write t)
  (:synopsis "The stree @t as a message (JSON)")
  (typed-tree->json (stree->tree t)))

(tm-define (lsp-object . l)
  (:synopsis "The object with the keys and values @l")
  (cons 'attr l))

(tm-define (lsp-array . l)
  (:synopsis "The array of @l")
  (cons 'tuple l))

(tm-define (lsp-list->array l)
  (cons 'tuple l))

(tm-define (lsp-number n)
  (cons 'json-number (list (number->string n))))

(tm-define (lsp-boolean b)
  (list 'json-boolean (if b "true" "false")))

(tm-define (lsp-null)
  (list 'json-null))

(tm-define (lsp-ref t . keys)
  (:synopsis "The value in @t at the keys (strings) or indices @keys, or #f")
  (cond ((null? keys) t)
        ((and (string? (car keys)) (func? t 'attr))
         (let loop ((l (cdr t)))
           (cond ((or (null? l) (null? (cdr l))) #f)
                 ((== (car l) (car keys)) (apply lsp-ref (cadr l) (cdr keys)))
                 (else (loop (cddr l))))))
        ((and (integer? (car keys)) (func? t 'tuple)
              (>= (car keys) 0) (< (car keys) (length (cdr t))))
         (apply lsp-ref (list-ref (cdr t) (car keys)) (cdr keys)))
        (else #f)))

(tm-define (lsp-items t)
  (:synopsis "The elements of the array @t (none if it is not one)")
  (if (func? t 'tuple) (cdr t) '()))

(tm-define (lsp-number-value t)
  (:synopsis "The number @t (json-number), or #f")
  (and (func? t 'json-number 1) (string->number (cadr t))))

(tm-define (lsp-true? t)
  (== t '(json-boolean "true")))

(tm-define (lsp-null? t)
  (or (not t) (func? t 'json-null)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Texts and positions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; The texts of the servers are those of the files (cork->sourcecode and
;; sourcecode->cork, as when they are saved and loaded): cork->utf8 would
;; give the typographic forms of some characters (a backquote as an opening
;; quote, three dots as an ellipsis), which are not in the files; a tab
;; of a server is the character <#9> of a line (source->cork), as when a
;; file is loaded (a byte 9 of Cork is a macron)

(tm-define (lsp-utf16-length s)
  (:synopsis "The length of the utf8 string @s in UTF-16 units")
  (let ((n (string-length s)))
    (let loop ((i 0) (r 0))
      (if (>= i n) r
          (let ((c (char->integer (string-ref s i))))
            (loop (+ i 1)
                  (cond ((< c 128) (+ r 1))
                        ((< c 192) r)          ; the rest of a character
                        ((< c 240) (+ r 1))
                        (else (+ r 2)))))))))

(tm-define (lsp-offset->character line offset)
  (:synopsis "The character of a position at @offset of the Cork @line")
  ;; (the positions of the core, System/Language/code_lines.cpp, which
  ;; tree-sitter uses too)
  (code-utf16-column line offset))

(tm-define (lsp-character->offset line ch)
  (:synopsis "The offset in the Cork @line of the character @ch of a position")
  ;; (a position inside a character, which a server should not give, is
  ;; at its start; one beyond the end of the line at its end)
  (code-utf16-offset line ch))

(define (verbatim-line t)
  ;; a line which is not a string (markup in a source file), as a string
  (let* ((opts (acons "texmacs->verbatim:encoding" "SourceCode" '()))
         (s (texmacs->verbatim t opts)))
    (sourcecode->cork (string-replace s "\n" " "))))

(tm-define (lsp-lines t)
  (:synopsis "The lines of the document @t (a tree or an stree), in Cork")
  (with s (if (tree? t) (tree->stree t) t)
    (cond ((string? s) (string-decompose s "\n"))
          ((func? s 'document)
           (map (lambda (l) (if (string? l) l (verbatim-line (stree->tree l))))
                (cdr s)))
          (else (list (verbatim-line (stree->tree s)))))))

(tm-define (lsp-lines->text lines)
  (:synopsis "The text of the Cork @lines, in utf8, for a server")
  (string-recompose (map cork->sourcecode lines) "\n"))

(tm-define (lsp-text->lines text)
  (:synopsis "The lines of the utf8 @text of a server, in Cork")
  (map (lambda (l)
         (source->cork (if (string-ends? l "\r")
                         (substring l 0 (- (string-length l) 1))
                         l)))
       (string-decompose text "\n")))

(tm-define (lsp-line lines i)
  (:synopsis "The line @i of @lines (a list or a vector), \"\" if none")
  ;; (a vector for many positions in a long file: list-ref walks the list)
  (if (vector? lines)
      (if (and (>= i 0) (< i (vector-length lines))) (vector-ref lines i) "")
      (with l (and (>= i 0) (list-tail* lines i))
        (if (pair? l) (car l) ""))))

(define (list-tail* l i)
  ;; the tail of @l after @i elements, #f if it is shorter
  (cond ((== i 0) l)
        ((pair? l) (list-tail* (cdr l) (- i 1)))
        (else #f)))

(tm-define (lsp-position lines line offset)
  (:synopsis "The position at @offset of the line @line of @lines")
  (with s (lsp-line lines line)
    (lsp-object "line" (lsp-number line)
                "character" (lsp-number (lsp-offset->character s offset)))))

(tm-define (lsp-position->line+offset lines pos)
  (:synopsis "The line and the offset in @lines of the position @pos")
  (let* ((line (or (lsp-number-value (lsp-ref pos "line")) 0))
         (ch (or (lsp-number-value (lsp-ref pos "character")) 0))
         (s (lsp-line lines line)))
    (list line (lsp-character->offset s ch))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; URIs of files
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (uri-char? c)
  (or (char-alphabetic? c) (char-numeric? c)
      (in? c '(#\- #\. #\_ #\~ #\/ #\:))))

(define (hex2 n)
  (with d "0123456789ABCDEF"
    (string (string-ref d (quotient n 16)) (string-ref d (remainder n 16)))))

(tm-define (lsp-path->uri path)
  (:synopsis "The URI of the file at the (utf8) system @path")
  (let* ((p0 (string-replace path "\\" "/"))
         ;; (c:/x and C:/x are the same file: the drive is in capitals, as
         ;; lsp-uri->path gives it)
         (p (if (and (>= (string-length p0) 2)
                     (char-alphabetic? (string-ref p0 0))
                     (== (string-ref p0 1) #\:))
                (string-append (string (char-upcase (string-ref p0 0)))
                               (substring p0 1 (string-length p0)))
                p0))
         (p* (if (string-starts? p "/") p (string-append "/" p))))
    (string-append
     "file://"
     (apply string-append
            (map (lambda (c)
                   (if (and (< (char->integer c) 128) (uri-char? c))
                       (string c)
                       (string-append "%" (hex2 (char->integer c)))))
                 (string->list p*))))))

(define (hex-digit? c)
  (or (char-numeric? c) (in? (char-downcase c) '(#\a #\b #\c #\d #\e #\f))))

(tm-define (lsp-uri->path uri)
  (:synopsis "The system path of the file of @uri (file://...), or #f")
  ;; (file:/x, without the authority, is the same file as file:///x)
  (and (string-starts? uri "file:/")
       (let loop ((l (string->list
                      (if (string-starts? uri "file://")
                          (substring uri 7 (string-length uri))
                          (substring uri 5 (string-length uri)))))
                  (r '()))
         (cond ((null? l)
                (with p (list->string (reverse r))
                  ;; file:///c:/x on Windows (C: as c: are the same drive,
                  ;; which some servers write as c%3A)
                  (if (and (>= (string-length p) 3)
                           (== (string-ref p 0) #\/)
                           (char-alphabetic? (string-ref p 1))
                           (== (string-ref p 2) #\:))
                      (string-append (string (char-upcase (string-ref p 1)))
                                     (substring p 2 (string-length p)))
                      p)))
               ((and (== (car l) #\%) (>= (length l) 3)
                     (hex-digit? (cadr l)) (hex-digit? (caddr l)))
                (loop (cdddr l)
                      (cons (integer->char
                             (string->number
                              (list->string (list (cadr l) (caddr l))) 16))
                            r)))
               (else (loop (cdr l) (cons (car l) r)))))))
