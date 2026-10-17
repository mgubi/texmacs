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

(texmacs-module (check lsp-test)
  (:use (check check-lib)))

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
                                  "Loading..."))))

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
;; The suite
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (lsp-test-failures)
  (check-suite "lsp")
  ;; (the servers are commands of sh: not on Windows)
  (when (not (or (os-mingw?) (os-win32?)))
    (test-echo)
    (test-headers)
    (test-stderr)
    (test-errors))
  (check-end))
