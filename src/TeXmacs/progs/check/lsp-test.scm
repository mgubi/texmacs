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
        (lsp lsp-actions)))

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
  (check= (lsp-lines '(document "int x;" "" "}")) '("int x;" "" "}"))
  (check= (lsp-lines->text (list "a" e-acute "")) (string-append "a\n"
                                                    (cork->utf8 e-acute) "\n"))
  (check= (lsp-text->lines "a\r\nb\n") '("a" "b" ""))
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
  (check= (lsp-uri->path "file:///c:/x/y.py") "c:/x/y.py")
  (check= (lsp-uri->path "untitled:1") #f))

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
      (check= (lsp-buffer-client*) #f)
      (check= (lsp-buffer-uri) (lsp-path->uri path))
      (check= (lsp-buffer-lines) '("int main() {" "  return x;" "}" ""))
      (check= (lsp-lines->text (lsp-buffer-lines))
              "int main() {\n  return x;\n}\n")
      (go-to-path (append (buffer-path) '(1 9)))
      (check= (lsp-cursor-position)
              '(attr "line" (json-number "1") "character" (json-number "9")))
      (when (url-exists-in-path? "clangd")
        (test-clangd path)
        (test-editor)
        (test-actions))
      (buffer-close u)
      (when (buffer-exists? old) (switch-to-buffer old))))
  (system-rmdir-recursive (system->url lsp-dir)))

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
  (check= (lsp-apply-to-lines '("ab") (list (text-edit 0 1 0 1 "1")
                                           (text-edit 0 2 0 2 "2")))
          '("a1b2"))
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
  ;; signatures, the current parameter in bold
  (with sig (lsp-object "signatures"
                        (lsp-array (lsp-object "label" "int f(int a, int b)"
                                               "parameters"
                                               (lsp-array
                                                (lsp-object "label" "int a")
                                                (lsp-object "label" "int b"))))
                        "activeParameter" (lsp-number 1))
    (check= (lsp-signature-markup sig)
            '(concat "int f(int a, " (strong "int b") ")")))
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
          '(concat "g(" (strong "x") ")"))
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
      ;; a quick fix: did you mean twice?
      (go-to-path (append (buffer-path) '(2 10)))
      (with r (lsp-code-actions-at-cursor)
        (check-true (and r (nnull? (cadr r))))
        (when (and r (nnull? (cadr r)))
          (lsp-run-action c (car (cadr r)))))
      (check= (list-ref (lsp-buffer-lines) 2) "  int a= twice (1);")
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
    (test-errors))
  (test-messages)
  (test-texts)
  (test-uris)
  (test-initialize)
  (test-requests)
  (test-server-requests)
  (test-end)
  (when (not (or (os-mingw?) (os-win32?)))
    (test-ended))
  (test-edits)
  (test-documents)
  (test-diagnostics)
  (test-buffer)
  (lsp-forget-all)
  (check-end))
