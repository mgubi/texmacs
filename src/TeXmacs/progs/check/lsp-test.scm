;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : lsp-test.scm
;; DESCRIPTION : tests of the links to language servers
;; COPYRIGHT   : (C) 2026  Massimiliano Gubinelli
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; The framing of the messages of the Language Server Protocol by the LSP
;; links (src/System/Link/lsp_link.cpp), through the glue functions
;; lsp-start, lsp-send, lsp-listen, lsp-alive? and lsp-stop. No language
;; server is needed: cat sends back the framed messages which it gets, and
;; printf writes the bytes of a server, headers included. The call back of
;; a link keeps what it gets in a list of events (kind data).
;;
;; The protocol in Scheme (progs/lsp): the messages as strees of the JSON
;; converter, the texts and positions, the URIs; the clients, on procedures
;; which keep what they send and are given the answers (no server); the
;; documents, a source file in a buffer, and clangd when it is installed;
;; with clangd, the editor (lsp-edit): the buffer followed, the diagnostics
;; underlined (alt selections), definitions and completion.

(texmacs-module (check lsp-test)
  (:use (check check-lib)
        (lsp lsp-base)
        (lsp lsp-client)
        (lsp lsp-documents)
        (lsp lsp-edit)
        (lsp lsp-actions)
        (lsp lsp-kbd)
        (lsp lsp-navigate)
        (lsp lsp-decorate)
        (lsp lsp-fold)))

(define events '())

(define (record kind data)
  (set! events (append events (list (list kind data)))))

(define (start cmd)
  (set! events '())
  (lsp-start cmd record))

(define (events-of kind)
  (map cadr (list-filter events (lambda (e) (== (car e) kind)))))

(define (wait-until id done?)
  ;; listens for at most 5 s, until done? holds
  (let loop ((i 0))
    (when (and (< i 50) (not (done?)))
      (lsp-listen id 100)
      (loop (+ i 1)))))

(define (wait-messages id n)
  (wait-until id (lambda () (>= (length (events-of "message")) n))))

(define (wait-exit id)
  (wait-until id (lambda () (nnull? (events-of "exit")))))

(define (bytes . l)
  (list->string (map integer->char l)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Messages sent and received
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (test-echo)
  (check-group "echo")
  (let* ((id (start "cat"))
         ;; the length of a message is in bytes: two for each accent
         (accented (string-append "{\"text\":\"" (bytes 195 169 195 168)
                                  "\"}"))
         (big (string-append "{\"text\":\"" (make-string 100000 #\a) "\"}")))
    (check-true (integer? id))
    (check-true (lsp-alive? id))
    (check-true (lsp-send id "{\"jsonrpc\":\"2.0\",\"id\":1}"))
    (wait-messages id 1)
    (check= (events-of "message") (list "{\"jsonrpc\":\"2.0\",\"id\":1}"))
    (lsp-send id accented)
    (lsp-send id "[]")
    (wait-messages id 3)
    (check= (cdr (events-of "message")) (list accented "[]"))
    ;; a message which comes in many parts
    (lsp-send id big)
    (wait-messages id 4)
    (check= (length (events-of "message")) 4)
    (check-true (== (cAr (events-of "message")) big))
    (check= (events-of "stderr") '())
    (lsp-stop id)
    (check-false (lsp-alive? id))
    (check-false (lsp-send id "{}"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The headers and the bytes of a server
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (test-headers)
  (check-group "headers")
  ;; several messages at once, a header in lower case with a Content-Type,
  ;; then the end of the server
  (let ((id (start (string-append
                    "printf 'Content-Length: 2\\r\\n\\r\\n{}"
                    "content-length:3\\r\\n"
                    "Content-Type: application/vscode-jsonrpc; charset=utf-8"
                    "\\r\\n\\r\\n[1]'"))))
    (wait-exit id)
    (check= (events-of "message") (list "{}" "[1]"))
    (check= (length (events-of "exit")) 1)
    (check= (car (cAr events)) "exit")
    (check-false (lsp-alive? id)))
  ;; a message whose bytes come in several writes
  (let ((id (start (string-append
                    "printf 'Content-Le'; sleep 0.2; "
                    "printf 'ngth: 7\\r\\n\\r\\n[1,'; sleep 0.2; "
                    "printf '2,3]'; sleep 0.2"))))
    (wait-exit id)
    (check= (events-of "message") (list "[1,2,3]")))
  ;; a line printed on stdout without a header is skipped, with an error
  (let ((id (start (string-append
                    "printf 'Loading...\\r\\n\\r\\n"
                    "Content-Length: 2\\r\\n\\r\\n{}'"))))
    (wait-exit id)
    (check= (events-of "message") (list "{}"))
    (check-true (string-contains? (apply string-append (events-of "stderr"))
                                  "Loading...")))
  ;; headers whose lines are ended by LF alone (against the protocol)
  (let ((id (start "printf 'Content-Length: 2\\n\\n{}Content-Length: 3\\n\\n[1]'")))
    (wait-exit id)
    (check= (events-of "message") (list "{}" "[1]")))
  ;; a negative length is not one
  (let ((id (start "printf 'Content-Length: -2\\r\\n\\r\\nContent-Length: 2\\r\\n\\r\\n{}'")))
    (wait-exit id)
    (check= (events-of "message") (list "{}")))
  ;; a line ended by LF alone, just before a header, is skipped too: the
  ;; header is still read
  (let ((id (start (string-append
                    "printf 'Starting...\\n"
                    "Content-Length: 2\\r\\n\\r\\n{}'"))))
    (wait-exit id)
    (check= (events-of "message") (list "{}"))
    (check-true (string-contains? (apply string-append (events-of "stderr"))
                                  "skipped 'Starting...'"))))

(define (test-nested)
  (check-group "waiting in a call back")
  ;; a call back which waits for the next message gets it (the call backs
  ;; are dispatched within the dispatch)
  (let* ((got '())
         (inner #f)
         (id #f))
    (set! id (lsp-start "cat"
                        (lambda (kind data)
                          (when (== kind "message")
                            (set! got (append got (list data)))
                            (when (== data "1")
                              (lsp-send id "2")
                              (let loop ((i 0))
                                (when (and (< i 30) (not (in? "2" got)))
                                  (lsp-listen id 100)
                                  (loop (+ i 1))))
                              (set! inner (in? "2" got)))))))
    (lsp-send id "1")
    (let loop ((i 0))
      (when (and (< i 50) (not inner))
        (lsp-listen id 100)
        (loop (+ i 1))))
    (check-true inner)
    (check= got '("1" "2"))
    (lsp-stop id)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Errors and the end of a server
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (test-stderr)
  (check-group "stderr")
  (let ((id (start "echo starting >&2; cat")))
    (wait-until id (lambda () (nnull? (events-of "stderr"))))
    (check= (apply string-append (events-of "stderr")) "starting\n")
    (check-true (lsp-alive? id))
    (lsp-send id "{}")
    (wait-messages id 1)
    (check= (events-of "message") (list "{}"))
    (lsp-stop id)
    ;; nothing is sent once the server is stopped, not even its end
    (lsp-listen id 100)
    (check= (events-of "exit") '())))

(define (test-blocked-server)
  (check-group "a server which does not read")
  ;; a long message to a server which does not read its input returns at
  ;; once (the editor would freeze), and a long message to one which reads
  ;; arrives whole
  (let* ((id (start "sleep 10"))
         (big (make-string 4000000 #\a))
         (t0 (texmacs-time)))
    (check-true (lsp-send id big))
    (check-true (< (- (texmacs-time) t0) 1000))
    (check-true (lsp-alive? id))
    (lsp-stop id))
  (let* ((id (start "cat"))
         (big (make-string 2000000 #\b)))
    (lsp-send id big)
    (wait-messages id 1)
    (check= (map string-length (events-of "message")) '(2000000))
    (lsp-stop id)))

(define (test-errors)
  (check-group "errors")
  (with r (lsp-start "no-such-language-server-for-texmacs" record)
    (check-true (string? r))
    (check-true (string-starts? r "Error:")))
  ;; an unknown or stopped client: nothing happens
  (check-false (lsp-alive? 123456))
  (check-false (lsp-send 123456 "{}"))
  (lsp-listen 123456 0)
  (lsp-stop 123456)
  (check-true #t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Messages, texts and URIs (lsp-base)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (test-messages)
  (check-group "messages")
  (with t (lsp-read "{\"id\":1,\"ok\":true,\"x\":null,\"a\":[2.5,\"s\"]}")
    (check= t '(attr "id" (json-number "1") "ok" (json-boolean "true")
                     "x" (json-null)
                     "a" (tuple (json-number "2.5") "s")))
    (check= (lsp-number-value (lsp-ref t "id")) 1)
    (check-true (lsp-true? (lsp-ref t "ok")))
    (check-true (lsp-null? (lsp-ref t "x")))
    (check= (lsp-ref t "a" 1) "s")
    (check= (lsp-ref t "a" 2) #f)
    (check= (lsp-ref t "nothing") #f)
    (check= (lsp-items (lsp-ref t "a")) '((json-number "2.5") "s"))
    ;; written and read again
    (check= (lsp-read (lsp-write t)) t))
  (check= (lsp-read (lsp-write (lsp-object "s" "a\"b\n" "e" (lsp-array))))
          '(attr "s" "a\"b\n" "e" (tuple)))
  (check= (lsp-read (lsp-write (lsp-object))) '(attr))
  ;; (the converter reads anything)
  (check= (lsp-read "not json at all") '(json-null))
  (check= (lsp-read "") #f))

(define e-acute (string (integer->char 233)))   ; in Cork
(define smiley "<#1F600>")                       ; beyond U+FFFF
(define smiley-utf8 (bytes 240 159 152 128))     ; the same, in utf8

(define (test-texts)
  (check-group "texts")
  (check= (lsp-utf16-length "abc") 3)
  (check= (lsp-utf16-length (cork->utf8 e-acute)) 1)
  (check= (lsp-utf16-length (cork->utf8 smiley)) 2)
  (with line (string-append "a" e-acute smiley "b")
    (check= (map (lambda (o) (lsp-offset->character line o)) '(0 1 2 10 11))
            '(0 1 2 4 5))
    (check= (map (lambda (ch) (lsp-character->offset line ch)) '(0 1 2 3 4 5 9))
            '(0 1 2 2 10 11 11)))
  ;; (the lines of one byte for each character, and the others)
  (check= (map (lambda (ch) (lsp-character->offset "abc" ch)) '(-1 0 2 9))
          '(0 0 2 3))
  (check= (map (lambda (ch) (lsp-character->offset "a<less>b" ch)) '(1 2 3))
          '(1 7 8))
  (check= (lsp-lines '(document "int x;" "" "}")) '("int x;" "" "}"))
  (check= (lsp-lines->text (list "a" e-acute "")) (string-append "a\n"
                                                    (cork->utf8 e-acute) "\n"))
  ;; the text built in C++ is the one of the lines (markup, accents, ends)
  (for (d (list `(document "a" ,e-acute "") '(document "x" (strong "y") "")
                (string-append "a\n" smiley) '(document) '(document "")))
    (with t (stree->tree d)
      (check= (lsp-document-text t) (lsp-lines->text (lsp-lines t)))))
  (check= (lsp-text->lines "a\r\nb\n") '("a" "b" ""))
  ;; the text of the file: a backquote and three dots are what they are
  (check= (lsp-lines->text '("a`b...c")) "a`b...c")
  (check= (lsp-document-text (stree->tree '(document "a`b...c" "")))
          "a`b...c\n")
  (check= (lsp-text->lines "x`y...\n") '("x`y..." ""))
  (check= (lsp-offset->character "a`b...c" 7) 7)
  ;; a tab is the character <#9> of a line (a byte 9 of Cork is a macron),
  ;; and one UTF-16 unit for the servers
  (check= (lsp-lines->text '("<#9>x")) (string-append (string #\tab) "x"))
  (check= (lsp-document-text (stree->tree '(document "<#9>x" "")))
          (string-append (string #\tab) "x\n"))
  (check= (lsp-text->lines (string-append (string #\tab) "y\n"))
          '("<#9>y" ""))
  (check= (lsp-character->offset "<#9>x" 1) 4)
  (check= (lsp-offset->character "<#9>x" 4) 1)
  (with lines (list "x" (string-append e-acute smiley "y"))
    (check= (lsp-position lines 1 9)
            '(attr "line" (json-number "1") "character" (json-number "3")))
    (check= (lsp-position->line+offset lines (lsp-position lines 1 9)) '(1 9))))

(define (test-uris)
  (check-group "uris")
  (check= (lsp-path->uri "/tmp/a b/c.cpp") "file:///tmp/a%20b/c.cpp")
  (check= (lsp-path->uri (string-append "/tmp/caf" (cork->utf8 e-acute)))
          "file:///tmp/caf%C3%A9")
  (check= (lsp-uri->path "file:///tmp/a%20b/c.cpp") "/tmp/a b/c.cpp")
  ;; (a drive is written in capitals, as servers write it otherwise)
  (check= (lsp-uri->path "file:///c:/x/y.py") "C:/x/y.py")
  (check= (lsp-uri->path "file:///c%3A/x/y.py") "C:/x/y.py")
  (check= (lsp-uri-key "file:///c%3A/x/y.py") (lsp-path->uri "C:/x/y.py"))
  (check= (lsp-uri->path "file:/tmp/x.c") "/tmp/x.c")
  (check= (lsp-uri->path "file:///a%+1b") "/a%+1b")
  (check= (lsp-uri->path "untitled:1") #f)
  (check= (lsp-path->uri "c:/x/y.py") "file:///C:/x/y.py"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Clients on procedures (lsp-client)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define sent '())

(define (fake-client)
  ;; a client whose messages are kept in sent (as strees)
  (set! sent '())
  (lsp-client-make "fake"
                   (lambda (s) (set! sent (append sent (list (lsp-read s)))))
                   ignore ignore))

(define (sent-methods)
  (map (lambda (m) (lsp-ref m "method")) sent))

(define (feed c . msgs)
  (for-each (lambda (m) (lsp-client-receive c "message" (lsp-write m))) msgs))

(define (answer-to id result)
  (lsp-object "jsonrpc" "2.0" "id" (lsp-number id) "result" result))

(define (running-client sync)
  ;; a fake client, initialized, with the textDocumentSync @sync
  (with c (fake-client)
    (lsp-client-initialize c "/tmp/project")
    (feed c (answer-to 1 (lsp-object "capabilities"
                                     (lsp-object "textDocumentSync" sync))))
    (set! sent '())
    c))

(define (test-initialize)
  (check-group "initialize")
  (with c (fake-client)
    (check= (lsp-client-state c) 'new)
    (lsp-client-initialize c "/tmp/project")
    (check= (lsp-client-state c) 'initializing)
    (check= (sent-methods) '("initialize"))
    (check= (lsp-ref (car sent) "id") '(json-number "1"))
    (check= (lsp-ref (car sent) "params" "rootUri") "file:///tmp/project")
    (check= (lsp-ref (car sent) "params" "capabilities" "general"
                     "positionEncodings" 0)
            "utf-16")
    ;; what is sent before the answer waits for it
    (lsp-notify c "textDocument/didOpen" (lsp-object))
    (check= (length sent) 1)
    (feed c (answer-to 1 (lsp-object "capabilities"
                                     (lsp-object "hoverProvider"
                                                 (lsp-boolean #t)))))
    (check= (lsp-client-state c) 'running)
    (check-true (lsp-true? (lsp-ref (lsp-client-capabilities c)
                                    "hoverProvider")))
    (check= (sent-methods)
            '("initialize" "initialized" "textDocument/didOpen"))))

(define (full-change uri text)
  (lsp-object "textDocument" (lsp-object "uri" uri "version" (lsp-number 2))
              "contentChanges" (lsp-array (lsp-object "text" text))))

(define (test-initialize-slow)
  (check-group "a server which is slow to start")
  (with c (fake-client)
    (lsp-client-initialize c "/tmp/project")
    ;; the whole texts of a document wait once (the last one), the others
    ;; and the other documents keep their order
    (lsp-notify c "textDocument/didOpen" (lsp-object))
    (lsp-notify c "textDocument/didChange" (full-change "file:///a" "1"))
    (lsp-notify c "textDocument/didChange" (full-change "file:///b" "x"))
    (lsp-notify c "textDocument/didChange" (full-change "file:///a" "2"))
    (lsp-notify c "textDocument/didChange" (full-change "file:///a" "3"))
    (check= (length (lsp-client-ref c 'queue)) 3)
    ;; a request of the server is answered at once
    (feed c (lsp-object "jsonrpc" "2.0" "id" (lsp-number 7)
                        "method" "no/such/method"))
    (check= (length sent) 2)
    (check= (lsp-ref (cAr sent) "id") '(json-number "7"))
    ;; no answer to initialize: the server is ended, the requests which
    ;; wait get an error
    (let* ((got #f)
           (id (lsp-request c "x" #f (lambda (r e) (set! got e)))))
      (lsp-client-check-initialized c)
      (check= (lsp-client-state c) 'dead)
      (check-true (and got #t))
      (check-true (string-contains? (cadr (car (lsp-client-log c)))
                                    "no answer to initialize")))))

(define (publish c uri n)
  (feed c (lsp-object "jsonrpc" "2.0"
                      "method" "textDocument/publishDiagnostics"
                      "params" (lsp-object
                                "uri" uri
                                "diagnostics"
                                (lsp-list->array
                                 (make-list n (lsp-object "message" "m")))))))

(define (test-diagnostics-state)
  (check-group "diagnostics of servers")
  (with c (running-client (lsp-number 2))
    ;; a server which writes the URI otherwise (lower case hexadecimal, an
    ;; escaped colon): the same file
    (publish c "file:///tmp/a%3ab/x%c3%a9.c" 2)
    (check= (length (lsp-diagnostics (lsp-path->uri "/tmp/a:b/x\xc3\xa9.c")))
            2)
    ;; the diagnostics of the documents of a server which ends are
    ;; forgotten
    (lsp-document-open c "file:///tmp/y.c" "c" "int y;")
    (publish c "file:///tmp/y.c" 1)
    (check= (length (lsp-diagnostics "file:///tmp/y.c")) 1)
    (lsp-client-kill c)
    (check= (lsp-diagnostics "file:///tmp/y.c") '())))

(define (test-sync-options)
  (check-group "options of the documents")
  ;; a server which does not take didOpen and didClose
  (with c (running-client (lsp-object "openClose" (lsp-boolean #f)
                                      "change" (lsp-number 2)))
    (lsp-document-open c "file:///tmp/z.c" "c" "int z;")
    (lsp-document-close c "file:///tmp/z.c")
    (check= (sent-methods) '()))
  ;; a kind of change alone (a number): the saves are sent, without text
  (with c (running-client (lsp-number 2))
    (lsp-document-open c "file:///tmp/z.c" "c" "int z;")
    (lsp-document-save c "file:///tmp/z.c")
    (check= (sent-methods) '("textDocument/didOpen" "textDocument/didSave"))
    (check= (lsp-ref (cAr sent) "params" "text") #f))
  ;; "error": null is no error; an error about no request goes to the log
  (let* ((c (running-client (lsp-number 1)))
         (got #f)
         (id (lsp-request c "x" #f (lambda (r e) (set! got (list r e))))))
    (feed c (lsp-object "jsonrpc" "2.0" "id" (lsp-number id)
                        "result" (lsp-number 5) "error" (lsp-null)))
    (check= got '((json-number "5") #f))
    (feed c (lsp-object "jsonrpc" "2.0" "id" (lsp-null)
                        "error" (lsp-object "code" (lsp-number -32700)
                                            "message" "parse error")))
    (check-true (string-contains? (cadr (car (lsp-client-log c)))
                                  "parse error"))))

(define (sym name kind line . children)
  (lsp-object "name" name "kind" (lsp-number kind)
              "range" (lsp-object "start" (lsp-object "line" (lsp-number line)
                                                      "character" (lsp-number 0))
                                  "end" (lsp-object "line" (lsp-number line)
                                                    "character" (lsp-number 9)))
              "children" (lsp-list->array children)))

(define (test-folding-ranges)
  (check-group "folding ranges")
  (with fr (lambda (a b) (lsp-object "startLine" (lsp-number a)
                                     "endLine" (lsp-number b)))
    (check= (lsp-folding-ranges-of (lsp-array (fr 0 4) (fr 2 2) (fr 1 3)))
            '((0 4) (1 3))))
  (check= (lsp-innermost-range '((0 9) (2 5) (3 4)) 4) '(3 4))
  (check= (lsp-innermost-range '((0 9) (2 5)) 6) '(0 9))
  (check= (lsp-innermost-range '((2 5)) 7) #f))

(define (test-outline)
  (check-group "outline")
  (check= (lsp-symbol-kind-name 12) "function")
  (check= (lsp-symbol-kind-name 5) "class")
  (check= (lsp-symbol-kind-name 99) "symbol")
  ;; DocumentSymbols, nested
  (check= (lsp-outline-entries
           (lsp-array (sym "Point" 23 0 (sym "x" 8 1) (sym "y" 8 2))
                      (sym "main" 12 5)))
          '((0 "struct" "Point" "" 0 0) (1 "field" "x" "" 1 0)
            (1 "field" "y" "" 2 0) (0 "function" "main" "" 5 0)))
  ;; SymbolInformations, flat, in the order of the file
  (with si (lambda (name kind line)
             (lsp-object "name" name "kind" (lsp-number kind)
                         "location"
                         (lsp-object "uri" "file:///a.c"
                                     "range" (lsp-object
                                              "start" (lsp-object
                                                       "line" (lsp-number line)
                                                       "character"
                                                       (lsp-number 2))))))
    (check= (lsp-outline-entries (lsp-array (si "b" 12 7) (si "a" 13 3)))
            '((0 "variable" "a" "" 3 2) (0 "function" "b" "" 7 2))))
  (check= (lsp-outline-entries (lsp-array)) '()))

(define (test-decorations)
  (check-group "colours and hints")
  ;; semantic tokens: relative lines and starts, the types of the legend
  (check= (lsp-decode-tokens '(0 4 5 0 0  0 6 3 1 0  2 2 4 0 0)
                             #("function" "variable"))
          '((0 4 5 "function") (0 10 3 "variable") (2 2 4 "function")))
  (check= (lsp-decode-tokens '(0 1 2 7 0) #("function")) '())
  (check= (lsp-token-color "c" "variable") #f)
  (check-true (string? (lsp-token-color "c" "function")))
  ;; the text of a hint, with its paddings
  (check= (lsp-hint-text (lsp-object "label" "x:" "paddingRight"
                                     (lsp-boolean #t)))
          "x: ")
  (check= (lsp-hint-text (lsp-object "label" (lsp-array
                                              (lsp-object "value" ": ")
                                              (lsp-object "value" "int"))))
          ": int")
  ;; by line, in Cork offsets (an accent is one unit), in their order
  (let* ((lines (vector (string-append "f" e-acute "g(1);") "int y;"))
         (hint (lsp-object "position" (lsp-object "line" (lsp-number 0)
                                                  "character" (lsp-number 4))
                           "label" "x:"))
         (d (lsp-line-decorations lines "c"
                                  '((0 2 1 "function") (0 0 1 "function")
                                    (1 0 3 "variable"))
                                  (list hint))))
    (check= (length d) 1)
    (check= (car (car d)) 0)
    (check= (map (lambda (x) (list (car x) (cadr x)))
                 (car (cadr (car d))))
            (list (list 0 1) (list 2 (+ 2 (string-length "g")))))
    (check= (cadr (cadr (car d)))
            (list (list (+ 3 (string-length e-acute)) "x:")))))

(define (test-protocol-points)
  (check-group "capabilities, versions, cancels")
  ;; a capability which a server gives as false: it has not
  (with c (fake-client)
    (lsp-client-initialize c "/tmp/project")
    (feed c (answer-to 1 (lsp-object "capabilities"
                                     (lsp-object "hoverProvider"
                                                 (lsp-boolean #t)
                                                 "renameProvider"
                                                 (lsp-boolean #f)))))
    (check-true (lsp-client-capable? c "hoverProvider"))
    (check-false (lsp-client-capable? c "renameProvider"))
    (check-false (lsp-client-capable? c "definitionProvider")))
  ;; a request which is cancelled before the server is initialized is not
  ;; sent at all
  (with c (fake-client)
    (lsp-client-initialize c "/tmp/project")
    (with id (lsp-request c "textDocument/hover" #f ignore)
      (lsp-cancel c id))
    (check= (lsp-client-ref c 'queue) '())
    (feed c (answer-to 1 (lsp-object "capabilities" (lsp-object))))
    (check= (sent-methods) '("initialize" "initialized")))
  ;; the diagnostics of an older version of a text are left
  (with c (running-client (lsp-number 1))
    (lsp-document-open c "file:///tmp/v.c" "c" "int a;")
    (lsp-document-change c "file:///tmp/v.c" "int b;")
    (with pub (lambda (v n)
                (feed c (lsp-object "jsonrpc" "2.0"
                                    "method" "textDocument/publishDiagnostics"
                                    "params" (lsp-object
                                              "uri" "file:///tmp/v.c"
                                              "version" (lsp-number v)
                                              "diagnostics"
                                              (lsp-list->array
                                               (make-list n (lsp-object
                                                             "message" "m")))))))
      (pub 1 3)
      (check= (lsp-diagnostics "file:///tmp/v.c") '())
      (pub 2 2)
      (check= (length (lsp-diagnostics "file:///tmp/v.c")) 2))
    ;; the edits for an older version are refused
    (with w (lambda (v)
              (lsp-object "documentChanges"
                          (lsp-array
                           (lsp-object "textDocument"
                                       (lsp-object "uri" "file:///tmp/v.c"
                                                   "version" (lsp-number v))
                                       "edits" (lsp-array)))))
      (check-true (lsp-workspace-edit-stale? c (w 1)))
      (check-false (lsp-workspace-edit-stale? c (w 2))))
    (lsp-client-kill c)))

(define (test-requests)
  (check-group "requests")
  (let* ((c (running-client (lsp-number 1)))
         (got '())
         (h (lambda (r e) (set! got (append got (list (list r e))))))
         (id1 (lsp-request c "textDocument/hover" (lsp-object) h))
         (id2 (lsp-request c "shutdown" #f h)))
    (check= (list id1 id2) '(2 3))
    (check= (lsp-ref (car sent) "params") '(attr))
    (check= (lsp-ref (cadr sent) "params") #f)
    ;; the answers, in another order
    (feed c (lsp-object "jsonrpc" "2.0" "id" (lsp-number 3)
                        "error" (lsp-object "code" (lsp-number -32600)
                                            "message" "no")))
    (feed c (answer-to 2 (lsp-number 42)))
    (check= (length got) 2)
    (check= (lsp-ref (cadr (car got)) "message") "no")
    (check= (car (cadr got)) '(json-number "42"))
    ;; an answer to nothing, and a null result
    (feed c (answer-to 99 (lsp-null)))
    (check= (length got) 2)
    (lsp-request c "x" #f h)
    (feed c (answer-to 4 (lsp-null)))
    (check= (cAr got) '((json-null) #f))
    ;; a cancelled request is not answered
    (with id (lsp-request c "y" #f h)
      (lsp-cancel c id)
      (check= (lsp-ref (cAr sent) "method") "$/cancelRequest")
      (feed c (answer-to id (lsp-number 1)))
      (check= (length got) 3))))

(define (test-server-requests)
  (check-group "requests of servers")
  (with c (running-client (lsp-number 1))
    (feed c (lsp-object "jsonrpc" "2.0" "id" (lsp-number 7)
                        "method" "workspace/configuration"
                        "params" (lsp-object "items" (lsp-array (lsp-object)
                                                                (lsp-object)))))
    (check= (cAr sent) '(attr "jsonrpc" "2.0" "id" (json-number "7")
                              "result" (tuple (json-null) (json-null))))
    ;; the id of the answer is the one of the request, a string here
    (feed c (lsp-object "jsonrpc" "2.0" "id" "abc"
                        "method" "window/workDoneProgress/create"
                        "params" (lsp-object "token" "t")))
    (check= (cAr sent) '(attr "jsonrpc" "2.0" "id" "abc"
                              "result" (json-null)))
    (feed c (lsp-object "jsonrpc" "2.0" "id" (lsp-number 8)
                        "method" "workspace/somethingNew"))
    (check= (lsp-ref (cAr sent) "error" "code") '(json-number "-32601"))
    ;; notifications and logs
    (feed c (lsp-object "jsonrpc" "2.0" "method" "window/logMessage"
                        "params" (lsp-object "type" (lsp-number 3)
                                             "message" "indexing")))
    (lsp-client-receive c "stderr" "some output\n")
    (lsp-client-receive c "message" "[1, 2]")
    (check= (map car (lsp-client-log c)) '("log" "stderr" "error"))
    (check= (cadr (car (lsp-client-log c))) "indexing")))

(define (test-end)
  (check-group "end")
  (let* ((c (running-client (lsp-number 1)))
         (got #f)
         (h (lambda (r e) (set! got e))))
    (lsp-request c "textDocument/hover" (lsp-object) h)
    (lsp-client-receive c "exit" "")
    (check= (lsp-client-state c) 'dead)
    (check-false (lsp-client-alive? c))
    (check= (lsp-ref got "code") '(json-number "-32099"))
    ;; a request to a dead server fails at once, and nothing is sent
    (set! got #f)
    (set! sent '())
    (lsp-request c "textDocument/hover" (lsp-object) h)
    (check-true (and got #t))
    (check= sent '())))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Documents and diagnostics (lsp-documents)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (test-documents)
  (check-group "documents")
  (with c (running-client (lsp-object "openClose" (lsp-boolean #t)
                                      "change" (lsp-number 2)
                                      "save" (lsp-object "includeText"
                                                         (lsp-boolean #t))))
    (lsp-document-open c "file:///a.c" "c" "int x;\n")
    (check= (sent-methods) '("textDocument/didOpen"))
    (check= (lsp-ref (car sent) "params" "textDocument" "version")
            '(json-number "1"))
    (check= (lsp-ref (car sent) "params" "textDocument" "text") "int x;\n")
    ;; the same text is not sent again
    (lsp-document-change c "file:///a.c" "int x;\n")
    (check= (length sent) 1)
    (lsp-document-change c "file:///a.c" "int y;\n")
    (check= (lsp-ref (cAr sent) "params" "textDocument" "version")
            '(json-number "2"))
    ;; (the server takes changes: only y replaces x)
    (check= (lsp-ref (cAr sent) "params" "contentChanges" 0)
            '(attr "range" (attr "start" (attr "line" (json-number "0")
                                               "character" (json-number "4"))
                                 "end" (attr "line" (json-number "0")
                                             "character" (json-number "5")))
                   "text" "y"))
    (check= (lsp-document-version c "file:///a.c") 2)
    (lsp-document-save c "file:///a.c")
    (check= (lsp-ref (cAr sent) "params" "text") "int y;\n")
    (lsp-document-close c "file:///a.c")
    (check= (lsp-ref (cAr sent) "method") "textDocument/didClose")
    (check-false (lsp-document-open? c "file:///a.c"))
    ;; a document which is not open is not changed
    (lsp-document-change c "file:///a.c" "int z;\n")
    (check= (length sent) 4))
  ;; a server which does not want changes, nor saves
  (with c (running-client (lsp-object "change" (lsp-number 0)))
    (lsp-document-open c "file:///b.c" "c" "1")
    (lsp-document-change c "file:///b.c" "2")
    (lsp-document-save c "file:///b.c")
    (check= (sent-methods) '("textDocument/didOpen"))))

(define (test-diagnostics)
  (check-group "diagnostics")
  (with c (running-client (lsp-number 1))
    (feed c (lsp-object
             "jsonrpc" "2.0" "method" "textDocument/publishDiagnostics"
             "params"
             (lsp-object
              "uri" "file:///d.c"
              "diagnostics"
              (lsp-array
               (lsp-object "range" (lsp-object
                                    "start" (lsp-object "line" (lsp-number 1)
                                                        "character"
                                                        (lsp-number 1))
                                    "end" (lsp-object "line" (lsp-number 1)
                                                      "character"
                                                      (lsp-number 3)))
                           "severity" (lsp-number 2)
                           "message" (string-append "caf"
                                                    (cork->utf8 e-acute)))))))
    (with l (lsp-diagnostics "file:///d.c")
      (check= (length l) 1)
      (check= (lsp-diagnostic-severity (car l)) 'warning)
      (check= (lsp-diagnostic-message (car l)) (string-append "caf" e-acute))
      (check= (lsp-diagnostic-range (car l) (list "" (string-append e-acute
                                                                    "ab")))
              '(1 1 1 3)))
    (check= (lsp-diagnostics "file:///other.c") '())))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; A source file in a buffer, and clangd
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define lsp-dir
  (string-append (url->system (url-temp-dir)) "/lsp-tmp"))

(define (lsp-file name)
  (string-append lsp-dir "/" name))

(define (setup-project)
  ;; lsp-tmp/proj, a project (compile_flags.txt) with a C file in src
  (system-mkdir (system->url (lsp-file "proj/src")))
  (string-save "" (system->url (lsp-file "proj/compile_flags.txt")))
  (string-save "int main() {\n  return x;\n}\n"
               (system->url (lsp-file "proj/src/main.c"))))

(define (test-buffer)
  (check-group "buffer")
  (setup-project)
  (with path (lsp-file "proj/src/main.c")
    (check= (lsp-project-root path) (lsp-file "proj"))
    (check= (lsp-language-id "c" path) "c")
    (check= (lsp-language-id "cpp" (lsp-file "x.cpp")) "cpp")
    ;; .c and .h files are C (the format c, the language c)
    (check= (url-format (system->url path)) "c")
    (check= (url-format (system->url (lsp-file "x.h"))) "c")
    (let ((old (current-buffer))
          (u (system->url path)))
      (load-buffer u)
      (check= (url->system (current-buffer)) path)
      (check= (lsp-buffer-language) "c")
      ;; (in the font of source files, a preference: Inconsolata)
      (check= (get-init "prog-font") (get-preference "editor:code font"))
      (check= (lsp-buffer-client*) #f)
      (check= (lsp-buffer-uri) (lsp-path->uri path))
      (check= (lsp-buffer-lines) '("int main() {" "  return x;" "}" ""))
      (check= (lsp-lines->text (lsp-buffer-lines))
              "int main() {\n  return x;\n}\n")
      (check= (lsp-buffer-text) "int main() {\n  return x;\n}\n")
      (check= (lsp-buffer-line 1) "  return x;")
      (check= (lsp-buffer-line 9) "")
      (check= (lsp-buffer-position 1 9)
              '(attr "line" (json-number "1") "character" (json-number "9")))
      (go-to-path (append (buffer-path) '(1 9)))
      (check= (lsp-cursor-position)
              '(attr "line" (json-number "1") "character" (json-number "9")))
      (test-lines-of-buffer u)
      (when (url-exists-in-path? "clangd")
        (test-clangd path)
        (test-editor)
        (test-actions))
      (buffer-close u)
      (when (buffer-exists? old) (switch-to-buffer old))))
  (system-rmdir-recursive (system->url lsp-dir)))

(define (test-lines-of-buffer u)
  ;; the decorations and the folds of the lines of @u, without a server
  ;; (its text is given back at the end)
  (check-group "decorations and folds of lines")
  (let* ((body (buffer-get-body u))
         (saved (tree->stree body))
         (uri (lsp-buffer-uri))
         (red (lambda (l) (list l (list '((0 3 "red")) '())))))
    ;; (positions outside of the buffer are in it)
    (check= (lsp-buffer-line+offset (lsp-object "line" (lsp-number 99)
                                                "character" (lsp-number 2)))
            (list (- (tree-arity body) 1)
                  (string-length (lsp-buffer-line (- (tree-arity body) 1)))))
    (check= (lsp-buffer-line+offset (lsp-object "line" (lsp-number -3)
                                                "character" (lsp-number 2)))
            '(0 2))
    (check= (lsp-apply-decorations u uri (list (red 0))) 1)
    (check= (lsp-decorations uri) '((0 ((0 3 "red")) ())))
    (check= (lsp-apply-decorations u uri (list (red 0))) 0)
    ;; a line which is edited is decorated again, with the same colours
    (tree-insert! (tree-ref body 0) 0 "x")
    (check= (lsp-apply-decorations u uri (list (red 0))) 1)
    ;; a line which moves keeps its decorations (also by several lines)
    (tree-insert! body 0 '("// a"))
    (check= (lsp-apply-decorations u uri (list (red 1))) 0)
    (tree-insert! body 0 '("// b" "// c"))
    (check= (lsp-apply-decorations u uri (list (red 3))) 0)
    (tree-remove! body 0 2)
    ;; a line which is removed loses them
    (tree-remove! body 1 1)
    (check= (lsp-apply-decorations u uri '()) 0)
    (check= (lsp-decorations uri) '())
    (tree-assign! body (stree->tree saved))
    ;; folds, those of the core (code-fold.scm): a state of the hidden
    ;; lines; a fold in a fold makes one run of hidden lines, which stays
    ;; when the first line is replaced; a hidden line which is edited
    ;; forgets it
    (lsp-fold-block 1 2)
    (check= (lsp-folds) '((1 2)))
    (lsp-fold-block 0 2)
    (check= (lsp-folds) '((0 2)))
    ;; (the cursor after the fold: arriving in a hidden line opens it)
    (go-to-path (append (buffer-path) '(3 0)))
    (tree-remove! body 0 1)
    (tree-insert! body 0 (list (car (cdr saved))))
    (check= (lsp-folds) '((0 2)))
    (check-true (lsp-line-folded? (tree-ref body 2)))
    (tree-insert! (tree-ref body 2) 0 " ")
    (check= (lsp-folds) '((0 1)))
    (go-to-path (append (buffer-path) '(0 0)))
    (lsp-unfold-all)
    (check= (lsp-folds) '())
    (check-false (lsp-line-folded? (tree-ref body 1)))
    (tree-assign! body (stree->tree saved))))

(define (test-clangd path)
  (check-group "clangd")
  (let* ((c (lsp-buffer-open))
         (uri (lsp-path->uri path)))
    (check-true (and c (lsp-client-alive? c)))
    (check= (lsp-client-ref c 'root) (lsp-file "proj"))
    (lsp-wait c (lambda () (nnull? (lsp-diagnostics uri))) 20000)
    (check= (lsp-client-state c) 'running)
    (with l (lsp-diagnostics uri)
      (check-true (nnull? l))
      (when (nnull? l)
        (check= (lsp-diagnostic-severity (car l)) 'error)
        (check= (lsp-diagnostic-range (car l) (lsp-buffer-lines))
                '(1 9 1 10))))
    ;; the cursor on main
    (go-to-path (append (buffer-path) '(0 5)))
    (with a (lsp-request-wait c "textDocument/hover"
                              (lsp-object "textDocument" (lsp-object "uri" uri)
                                          "position" (lsp-cursor-position))
                              10000)
      (check= (car a) 'result)
      (check-true (string? (lsp-ref (cadr a) "contents" "value"))))
    ;; the same client for the buffer, until it is stopped
    (check-true (eq? (lsp-buffer-client) c))
    (lsp-buffer-close)
    (check-false (lsp-document-open? c uri))
    (lsp-client-stop c)
    (check= (lsp-client-state c) 'dead)))

;; the editor, on a second file of the project

(define edit-text
  (string-append "int compute_value () { return 1; }\n"
                 "int main () {\n"
                 "  return x + compute_value ();\n"
                 "  comp\n"
                 "}\n"))

(define (cursor-rel)
  (list-tail (cursor-path) (length (buffer-path))))

(define (test-editor)
  (check-group "editor")
  (let* ((path (lsp-file "proj/src/edit.cpp"))
         (u (system->url path))
         (uri (lsp-path->uri path)))
    (string-save edit-text u)
    (load-buffer u)
    (check= (url->system (current-buffer)) path)
    ;; the editor notifies the changes, the text is given after a pause
    (notify-buffer-changed (url->string u))
    (lsp-sync-pending)
    (with c (lsp-buffer-client*)
      (check-true (and c (lsp-client-alive? c)))
      (check-true (and c (lsp-document-open? c uri)))
      (when c
        (lsp-wait c (lambda () (nnull? (lsp-diagnostics uri))) 20000)
        (lsp-show-diagnostics)
        (with errs (get-alt-selection "lsp-errors")
          (check-true (>= (length errs) 2))
          (check-true (in? (append (buffer-path) '(2 9)) errs)))
        ;; from the start to the first error
        (go-to-path (append (buffer-path) '(0 0)))
        (lsp-next-diagnostic #t)
        (check= (cursor-rel) '(2 9))
        ;; the definition of compute_value
        (go-to-path (append (buffer-path) '(2 16)))
        (lsp-goto-definition)
        (check= (current-buffer) u)
        (check= (cursor-rel) '(0 4))
        ;; completion of comp
        (go-to-path (append (buffer-path) '(3 6)))
        (check-true (lsp-completion-here?))
        (with r (lsp-request-wait c "textDocument/completion"
                                  (lsp-object "textDocument"
                                              (lsp-object "uri" uri)
                                              "position" (lsp-cursor-position))
                                  10000)
          (check= (car r) 'result)
          (check-true (in? "ute_value"
                           (lsp-completions "comp" (cadr r)))))
        ;; (the answer is awaited apart)
        (lsp-complete)
        (lsp-wait c (lambda ()
                      (> (string-length (list-ref (lsp-buffer-lines) 3)) 6))
                  10000)
        (with line (list-ref (lsp-buffer-lines) 3)
          (check-true (string-starts? line "  comp"))
          (check-true (> (string-length line) 6)))
        ;; the log of the server, on a page of its own
        (lsp-show-log)
        (with aux (current-buffer)
          (check-true (url-rooted-tmfs? aux))
          (with t (object->string (tree->stree (buffer-get-body aux)))
            (check-true (string-contains? t "clangd ("))
            (check-true (string-contains? t "stderr: ")))
          (switch-to-buffer u)
          ;; the page follows the log (after a pause, or at once here)
          (lsp-request-wait c "textDocument/hover"
                            (lsp-object "textDocument" (lsp-object "uri" uri)
                                        "position" (lsp-cursor-position))
                            10000)
          (lsp-wait c (lambda () #f) 500)
          (lsp-log-refresh)
          (with t (object->string (tree->stree (buffer-get-body aux)))
            (check-true (string-contains? t "textDocument/hover")))
          (buffer-close aux))
        (check= (current-buffer) u)
        (lsp-hide-diagnostics)
        (check= (get-alt-selection "lsp-errors") '())
        ;; information at the cursor, in a balloon at the line, which must
        ;; be typeset (the event loop does it before each key)
        (go-to-path (append (buffer-path) '(2 16)))
        (update-forced)
        (lsp-hover)
        (lsp-hover-close)
        (check-true (pair? (lsp-hover-document "**int** x")))
        ;; what is sent when the buffer is saved and closed
        (let* ((methods '())
               (send (lsp-client-ref c 'send)))
          (lsp-client-set! c 'send
                           (lambda (s)
                             (set! methods (append methods
                                                   (list (lsp-ref (lsp-read s)
                                                                  "method"))))
                             (send s)))
          (go-to-path (append (buffer-path) '(3 2)))
          (insert "x")
          (buffer-save u)
          (version-notify-saved u)
          (check= methods '("textDocument/didChange" "textDocument/didSave"))
          (check= (lsp-document-version c uri) 3)
          (buffer-close u)
          (check= (cAr methods) "textDocument/didClose")
          (check-false (lsp-document-open? c uri)))
        ;; stopped servers are not started again until they are restarted
        (lsp-stop-servers)
        (check= (lsp-client-state c) 'dead)
        (with root (lsp-file "proj")
          (check= (lsp-client-for "cpp" root) #f)
          (check-true (lsp-client-entry "cpp" root))
          (lsp-client-forget "cpp" root)
          (check= (lsp-client-entry "cpp" root) #f))))
    (when (buffer-exists? u) (buffer-close u))))

;; the edits of servers, without a server

(define (change old new)
  (with c (lsp-text-change old new)
    (list (lsp-number-value (lsp-ref c "range" "start" "line"))
          (lsp-number-value (lsp-ref c "range" "start" "character"))
          (lsp-number-value (lsp-ref c "range" "end" "line"))
          (lsp-number-value (lsp-ref c "range" "end" "character"))
          (lsp-ref c "text"))))

(define (text-edit l1 c1 l2 c2 text)
  (lsp-object "range"
              (lsp-object "start" (lsp-object "line" (lsp-number l1)
                                              "character" (lsp-number c1))
                          "end" (lsp-object "line" (lsp-number l2)
                                            "character" (lsp-number c2)))
              "newText" text))

(define (test-edits)
  (check-group "edits")
  ;; incremental changes
  (check= (change "int x;\n" "int y;\n") '(0 4 0 5 "y"))
  (check= (change "a" "ab") '(0 1 0 1 "b"))
  ;; in a long text (compared by blocks), at the end and at the start
  (let* ((line (string-append (make-string 99 #\a) "\n"))
         (body (apply string-append (make-list 1000 line)))
         (old (string-append body "x"))
         (new (string-append body "y")))
    (check= (change old new) '(1000 0 1000 1 "y"))
    (check= (change (string-append "x" body) (string-append "y" body))
            '(0 0 0 1 "y"))
    (check= (change (string-append body body) (string-append body "z" body))
            '(1000 0 1000 0 "z")))
  ;; the lines which change: (first last added)
  (check= (lsp-text-lines-change "a\nb\nc" "a\nX\nY\nc") '(1 1 1))
  (check= (lsp-text-lines-change "a\nb\nc\nd" "a\nd") '(1 3 0))
  (check= (lsp-text-lines-change "a\nb" "a\nb!") '(1 1 0))
  (check= (change "a\nb\n" "a\nx\nb\n") '(1 0 1 0 "x\n"))
  (check= (change "a\nbc\n" "a\n") '(1 0 2 0 ""))
  ;; (a change inside a character of two bytes replaces it whole)
  (check= (change (string-append (bytes 195 169) "\n")
                  (string-append (bytes 195 168) "\n"))
          (list 0 0 0 1 (bytes 195 168)))
  (check= (change (string-append "x" smiley-utf8 "y") (string-append "x" smiley-utf8 "z"))
          '(0 3 0 4 "z"))
  ;; text edits on lines, from the last one to the first
  (check= (lsp-apply-to-lines '("int x = 1;" "foo")
                              (list (text-edit 0 4 0 5 "yy")
                                    (text-edit 1 0 1 3 "bar\nbaz")))
          '("int yy = 1;" "bar" "baz"))
  (check= (lsp-apply-to-lines '("a" "b" "c") (list (text-edit 0 1 2 0 "")))
          '("ac"))
  ;; a tab of a server (a formatter which indents by tabs), after a tab
  (check= (lsp-apply-to-lines '("<#9>x" "y")
                              (list (text-edit 1 0 1 0 (string #\tab))))
          '("<#9>x" "<#9>y"))
  (check= (lsp-apply-to-lines '("<#9>x") (list (text-edit 0 1 0 2 "z")))
          '("<#9>z"))
  (check= (lsp-apply-to-lines '("ab") (list (text-edit 0 1 0 1 "1")
                                           (text-edit 0 2 0 2 "2")))
          '("a1b2"))
  ;; linear in the size of the file: the first and the last of 100000
  ;; lines (it was quadratic: a formatter froze the editor)
  (let* ((lines (make-list 100000 "int x;"))
         (t0 (texmacs-time))
         (r (lsp-apply-to-lines lines (list (text-edit 0 0 0 3 "long")
                                            (text-edit 99999 0 99999 3 "long")))))
    (check-true (< (- (texmacs-time) t0) 3000))
    (check= (list (car r) (list-ref r 99999) (length r))
            '("long x;" "long x;" 100000)))
  ;; positions beyond the end, and overlapping edits (from a bad server)
  (check= (lsp-apply-to-lines '("ab") (list (text-edit 5 0 6 0 "!"))) '("ab!"))
  (check= (lsp-apply-to-lines '("abcd") (list (text-edit 0 0 0 3 "X")
                                             (text-edit 0 1 0 4 "Y")))
          '("XY"))
  ;; edits with the same start come in the order of the server
  (check= (lsp-apply-to-lines '("x") (list (text-edit 0 0 0 0 "A")
                                          (text-edit 0 0 0 0 "B")
                                          (text-edit 0 0 0 0 "C")))
          '("ABCx"))
  (check= (lsp-apply-to-lines '("xy") (list (text-edit 0 0 0 0 "P")
                                           (text-edit 0 0 0 1 "Q")))
          '("PQy"))
  ;; the cursor through the edits: before, inside, after on the line and
  ;; below
  (with es (list (text-edit 0 4 0 5 "yy") (text-edit 1 0 1 3 "bar\nbaz"))
    (check= (lsp-move-through-edits '("int x = 1;" "foo") es '(0 2)) '(0 2))
    (check= (lsp-move-through-edits '("int x = 1;" "foo") es '(0 4)) '(0 4))
    (check= (lsp-move-through-edits '("int x = 1;" "foo") es '(0 8)) '(0 9))
    (check= (lsp-move-through-edits '("int x = 1;" "foo") es '(1 2)) '(2 3))
    (check= (lsp-move-through-edits '("int x = 1;" "foo" "z") es '(2 1))
            '(3 1)))
  ;; after an edit which makes its line longer: shifted (a rename)
  (check= (lsp-move-through-edits '("x = x + 1;")
                                  (list (text-edit 0 0 0 1 "longname")
                                        (text-edit 0 4 0 5 "longname"))
                                  '(0 10))
          '(0 24))
  (check= (lsp-move-through-edits '("x = x + 1;")
                                  (list (text-edit 0 4 0 5 "longname"))
                                  '(0 6))
          '(0 13))
  (check= (lsp-move-through-edits '("abcdef") (list (text-edit 0 1 0 5 "x"))
                                  '(0 4))
          '(0 2))
  (check= (lsp-move-through-edits '("a" "b" "c") (list (text-edit 0 1 2 0 ""))
                                  '(2 1))
          '(0 2))
  ;; the lines of a document tree, changed where they differ
  (with t (stree->tree '(document "a" "b" "c"))
    ;; (1 line replaced and 1 inserted, in 2 places of the old ones)
    (check= (lsp-set-lines t '("a" "B" "c" "d")) 3)
    (check= (tree->stree t) '(document "a" "B" "c" "d"))
    (lsp-set-lines t '("a" "d"))
    (check= (tree->stree t) '(document "a" "d")))
  ;; workspace edits, as a map or as document changes
  (check= (map car (lsp-workspace-edit-files
                    (lsp-object "changes"
                                (lsp-object "file:///a.c"
                                            (lsp-array (text-edit 0 0 0 0 "x"))
                                            "file:///b.c" (lsp-array)))))
          '("file:///a.c" "file:///b.c"))
  (check= (map car (lsp-workspace-edit-files
                    (lsp-object "documentChanges"
                                (lsp-array
                                 (lsp-object "textDocument"
                                             (lsp-object "uri" "file:///c.c"
                                                         "version" (lsp-null))
                                             "edits" (lsp-array))))))
          '("file:///c.c"))
  ;; signatures, the current parameter between guillemets (19 and 20)
  (with sig (lsp-object "signatures"
                        (lsp-array (lsp-object "label" "int f(int a, int b)"
                                               "parameters"
                                               (lsp-array
                                                (lsp-object "label" "int a")
                                                (lsp-object "label" "int b"))))
                        "activeParameter" (lsp-number 1))
    (check= (lsp-signature-markup sig)
            (string-append "int f(int a, " (string (integer->char 19))
                           "int b" (string (integer->char 20)) ")"))
    ;; (the parts, for the focus bar)
    (check= (lsp-signature-parts sig) '("int f(int a, " "int b" ")")))
  ;; offsets of a parameter in UTF-16 units; a name after the parenthesis
  (let* ((label (string-append "f(" (cork->utf8 e-acute) ", int b)"))
         (sig (lsp-object "signatures"
                          (lsp-array (lsp-object "label" label
                                                 "parameters"
                                                 (lsp-array
                                                  (lsp-object "label"
                                                              (lsp-array
                                                               (lsp-number 5)
                                                               (lsp-number 10))))))
                          "activeParameter" (lsp-number 0))))
    (check= (lsp-signature-parts sig)
            (list (string-append "f(" e-acute ", ") "int b" ")")))
  (with sig (lsp-object "signatures"
                        (lsp-array (lsp-object "label" "max(int x)"
                                               "parameters"
                                               (lsp-array
                                                (lsp-object "label" "x"))))
                        "activeParameter" (lsp-number 0))
    (check= (lsp-signature-parts sig) '("max(int " "x" ")")))
  ;; (an offset just after a character of several bytes)
  (let* ((label (string-append "f(" (cork->utf8 e-acute) ")"))
         (sig (lsp-object "signatures"
                          (lsp-array (lsp-object "label" label
                                                 "parameters"
                                                 (lsp-array
                                                  (lsp-object "label"
                                                              (lsp-array
                                                               (lsp-number 2)
                                                               (lsp-number 3))))))
                          "activeParameter" (lsp-number 0))))
    (check= (lsp-signature-parts sig) (list "f(" e-acute ")")))
  ;; a hover in plain text is not markdown
  (check= (lsp-hover-document "int *a, int *b" #f)
          '(document (verbatim "int *a, int *b")))
  ;; the edit of a completion comes before its insertText
  (check= (lsp-completions "fo" (lsp-array
                                 (lsp-object "label" "foo" "insertText" "fox"
                                             "textEdit"
                                             (lsp-object "newText" "foo"))))
          '("o"))
  ;; the progress of the work of a server, while it lasts
  (with c (lsp-client-make "fake" ignore ignore ignore)
    (define (progress value)
      (lsp-client-handle c (lsp-object "jsonrpc" "2.0" "method" "$/progress"
                                       "params" (lsp-object "token" "t"
                                                            "value" value))))
    (check= (lsp-client-progress c) #f)
    (progress (lsp-object "kind" "begin" "title" "indexing"
                          "percentage" (lsp-number 10)))
    (check= (lsp-client-progress c) "indexing 10%")
    (progress (lsp-object "kind" "report" "message" "1/2"
                          "percentage" (lsp-number 50)))
    (check= (lsp-client-progress c) "indexing 1/2 50%")
    (progress (lsp-object "kind" "end"))
    (check= (lsp-client-progress c) #f))
  (check= (lsp-signature-markup
           (lsp-object "signatures"
                       (lsp-array (lsp-object "label" "g(x)"
                                              "parameters"
                                              (lsp-array
                                               (lsp-object "label"
                                                           (lsp-array
                                                            (lsp-number 2)
                                                            (lsp-number 3))))))
                       "activeParameter" (lsp-number 0)))
          (string-append "g(" (string (integer->char 19)) "x"
                         (string (integer->char 20)) ")"))
  ;; the point under the mouse (the hovers at rest)
  (update-forced)
  (check-true (list? (xy->path 0 0))))

;; the actions of clangd, on a third file of the project

(define actions-text
  (string-append "int   twice (int v) { return 2*v; }\n"
                 "int main () {\n"
                 "  int a= twicee (1);\n"
                 "  return a + twice (a);\n"
                 "}\n"))

(define (test-actions)
  (check-group "actions")
  (let* ((path (lsp-file "proj/src/actions.cpp"))
         (u (system->url path))
         (uri (lsp-path->uri path))
         (c #f))
    (string-save actions-text u)
    (load-buffer u)
    (notify-buffer-changed (url->string u))
    (lsp-sync-pending)
    (set! c (lsp-buffer-client*))
    (check-true (and c (lsp-client-alive? c)))
    (when c
      (lsp-wait c (lambda () (nnull? (lsp-diagnostics uri))) 20000)
      ;; the page of the diagnostics
      (lsp-show-diagnostics-list)
      (with page (current-buffer)
        (check-true (url-rooted-tmfs? page))
        (check-true (string-contains?
                     (object->string (tree->stree (buffer-get-body page)))
                     "twicee"))
        (switch-to-buffer u)
        (buffer-close page))
      ;; the status in the focus bar: the server, ready, and its errors
      ;; the keys of the servers are those of the followed buffers (mode
      ;; in-lsp%, with in-prog%, which depends on the typesetting)
      (check-true (lsp-followed?))
      (with st (lsp-focus-status)
        (check-true (pair? st))
        (when (pair? st)
          (check= (car st) "clangd")
          (check= (cadr st) 'ready)
          (check-true (>= (list-ref st 3) 1))))
      (check-true (string-contains?
                   (object->string (menu-expand
                                    '(horizontal (dynamic (lsp-focus-icons)))))
                   "clangd"))
      (check-true (make-menu-widget '(horizontal (link texmacs-focus-icons)) 0))
      ;; a quick fix: did you mean twice?
      (go-to-path (append (buffer-path) '(2 10)))
      (with r (lsp-code-actions-at-cursor)
        (check-true (and r (nnull? (cadr r))))
        (when (and r (nnull? (cadr r)))
          (lsp-run-action c (car (cadr r)))))
      (check= (list-ref (lsp-buffer-lines) 2) "  int a= twice (1);")
      ;; (the cursor stays where it was, its line was replaced)
      (check= (cursor-rel) '(2 10))
      ;; the signature of twice
      (go-to-path (append (buffer-path) '(3 20)))
      (with a (lsp-request-wait c "textDocument/signatureHelp"
                                (lsp-object "textDocument" (lsp-object "uri" uri)
                                            "position" (lsp-cursor-position))
                                10000)
        (check= (car a) 'result)
        (check-true (string-contains? (object->string
                                       (lsp-signature-markup (cadr a)))
                                      "int v")))
      ;; the references of twice (after the fix: three places), and the
      ;; outline of the file, on pages
      (go-to-path (append (buffer-path) '(0 8)))
      (lsp-references)
      (with page (current-buffer)
        (check-true (url-rooted-tmfs? page))
        (with s (object->string (tree->stree (buffer-get-body page)))
          (check-true (string-contains? s "3 places"))
          (check-true (string-contains? s "src/actions.cpp"))
          (check-true (string-contains? s "twice (a)")))
        (switch-to-buffer u)
        (buffer-close page))
      (lsp-outline)
      (with page (current-buffer)
        (check-true (url-rooted-tmfs? page))
        (with s (object->string (tree->stree (buffer-get-body page)))
          (check-true (string-contains? s "function"))
          (check-true (string-contains? s "twice"))
          (check-true (string-contains? s "main")))
        (switch-to-buffer u)
        (buffer-close page))
      ;; the uses of twice, highlighted (three places)
      (go-to-path (append (buffer-path) '(0 8)))
      (lsp-highlight-at-cursor)
      (lsp-wait c (lambda () (nnull? (lsp-highlights))) 10000)
      (check= (length (lsp-highlights)) 3)
      (check= (car (lsp-highlights)) '(0 6 0 11))
      ;; folding the block of main: its lines stay in the document, but
      ;; are not shown; the cursor which comes in opens it
      (with text (lsp-buffer-text)
        (go-to-path (append (buffer-path) '(2 2)))
        (lsp-fold)
        (check= (lsp-folds) '((1 3)))
        (check-true (lsp-line-folded? (tree-ref (buffer-tree) 2)))
        (check-false (lsp-line-folded? (tree-ref (buffer-tree) 1)))
        (check= (cursor-rel) '(1 13))
        (check= (lsp-buffer-text) text)
        (lsp-unfold)
        (check= (lsp-folds) '())
        (check-false (lsp-line-folded? (tree-ref (buffer-tree) 2))))
      ;; the symbols of the project, the calls of twice
      (lsp-find-symbol "twice")
      (with page (current-buffer)
        (with s (object->string (tree->stree (buffer-get-body page)))
          (check-true (string-contains? s "twice"))
          (check-true (string-contains? s "src/actions.cpp")))
        (switch-to-buffer u)
        (buffer-close page))
      (go-to-path (append (buffer-path) '(0 8)))
      (lsp-call-hierarchy)
      (with page (current-buffer)
        (check-true (url-rooted-tmfs? page))
        (with s (object->string (tree->stree (buffer-get-body page)))
          (check-true (string-contains? s "Called by"))
          (check-true (string-contains? s "main")))
        (switch-to-buffer u)
        (buffer-close page))
      ;; the colours of the server: twice is a function (and the hint of
      ;; its parameter v, when clangd gives inlay hints)
      (lsp-decorate-current)
      (lsp-wait c (lambda () (nnull? (lsp-decorations uri))) 10000)
      (with d (assoc 0 (lsp-decorations uri))
        (check-true (and d (nnull? (cadr d))))
        (when (and d (nnull? (cadr d)))
          (check= (list (car (car (cadr d))) (cadr (car (cadr d))))
                  '(6 11))))
      ;; a rename everywhere
      (go-to-path (append (buffer-path) '(0 8)))
      (lsp-rename "thrice")
      (with all (apply string-append (lsp-buffer-lines))
        (check-false (string-contains? all "twice"))
        (check= (length (string-decompose all "thrice")) 4))
      ;; the formatting of the whole file
      (lsp-format)
      (check-false (string-starts? (list-ref (lsp-buffer-lines) 0) "int   "))
      ;; after the changes (incremental ones), the server sees no errors
      (lsp-wait c (lambda () (null? (lsp-diagnostics uri))) 20000)
      (check= (lsp-diagnostics uri) '())
      (lsp-stop-servers)
      (lsp-client-forget "cpp" (lsp-file "proj")))
    (when (buffer-exists? u) (buffer-close u))))

(define (test-ended)
  (check-group "servers which end")
  ;; the languages with servers: not latex (a .tex file is converted to a
  ;; document, not edited as source)
  (check-true (lsp-server-known? "cpp"))
  (check-false (lsp-server-known? "latex"))
  ;; a server which ends at once is not started again at each request
  (lsp-server-set! "lsp-test-lang" '("sh -c 'exit 0'"))
  (let* ((root "/tmp/lsp-test-project")
         (c (lsp-client-for "lsp-test-lang" root)))
    (check-true (and c #t))
    (when c
      (lsp-wait c (lambda () (== (lsp-client-state c) 'dead)) 5000)
      (check= (lsp-client-state c) 'dead)
      (check= (lsp-client-for "lsp-test-lang" root) #f)
      (check= (lsp-client-entry "lsp-test-lang" root) c)
      (lsp-client-forget "lsp-test-lang" root)
      (with c2 (lsp-client-for "lsp-test-lang" root)
        (check-true (and c2 (not (eq? c2 c)))))
      (lsp-client-forget "lsp-test-lang" root)))
  ;; the suffixes of completions after a . (the prefix is the dot)
  (check= (lsp-completion-tuple "" "  s." 4 '("field" "x"))
          '(tuple "." "field" "x"))
  (check= (lsp-completion-tuple "fi" "  s.fi" 6 '("eld"))
          '(tuple "fi" "eld"))
  (check= (lsp-completion-tuple "" "" 0 '("a")) #f))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The suite
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (lsp-test-failures)
  (check-suite "lsp")
  ;; (the servers are commands of sh: not on Windows)
  (when (not (or (os-mingw?) (os-win32?)))
    (test-echo)
    (test-headers)
    (test-stderr)
    (test-nested)
    (test-blocked-server)
    (test-errors))
  (test-messages)
  (test-texts)
  (test-uris)
  (test-initialize)
  (test-initialize-slow)
  (test-diagnostics-state)
  (test-sync-options)
  (test-outline)
  (test-folding-ranges)
  (test-decorations)
  (test-requests)
  (test-server-requests)
  (test-protocol-points)
  (test-end)
  (when (not (or (os-mingw?) (os-win32?)))
    (test-ended))
  (test-edits)
  (test-documents)
  (test-diagnostics)
  (test-buffer)
  (lsp-forget-all)
  (check-end))
