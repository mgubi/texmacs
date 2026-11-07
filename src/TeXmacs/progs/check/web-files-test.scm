;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : web-files-test.scm
;; DESCRIPTION : tests of the files and pictures fetched from the web
;; COPYRIGHT   : (C) 2026  Massimiliano Gubinelli
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; The files of the web (http://...) are downloaded into the temporary
;; directory and kept for the session (get_from_web, the web cache of
;; System/Files/web_files.cpp); a picture with an effect (art boxes) is
;; made by make_file (System/Files/make_file.cpp), which keeps its result
;; in $TEXMACS_HOME_PATH/system/make. A remote file is downloaded once per
;; session, the first time it is needed, and again after Tools > Update >
;; Pictures (picture-gc, remote-files-refresh); when it cannot be
;; downloaded, the copy of before is used.
;;
;; There is no internet: the files are served by a small HTTP server on
;; 127.0.0.1, a python3 script which this suite starts in the temporary
;; directory, logs the paths asked to it, and stops at the end (or after
;; ten minutes, if TeXmacs does not stop it). The script also listens on a
;; second port, where it takes the connections and never answers: a
;; download from there gives up after the network timeout (10s by
;; default, the preference "http request timeout"). Without python3, on
;; Windows, or without a way to download (Qt 6, wget or curl), the suite
;; is skipped. The time of make_file
;; (a result kept from a session before) is the same as after
;; remote-files-refresh, which the checks use: they run in one session.

(texmacs-module (check web-files-test)
  (:use (check check-lib)
        (texmacs texmacs tm-tools)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The server
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define web-dir
  (string-append (url->system (url-temp-dir)) "/web-files"))

(define (web-file name)
  (system->url (string-append web-dir "/" name)))

(define (clear-web-dir)
  (with d (system->url web-dir)
    (when (url-exists? d)
      (for-each (lambda (u)
                  (when (url-regular? u) (system-remove u)))
                (url-read-directory d "*"))
      (system-rmdir d))))

(define server-script
  (string-append
   "import http.server, os, socket, sys, threading\n"
   "d, portfile, logfile = sys.argv[1:4]\n"
   "os.chdir(d)\n"
   "threading.Timer(600, os._exit, (0,)).start()\n"
   "silent = socket.socket()\n"
   "silent.bind(('127.0.0.1', 0))\n"
   "silent.listen(5)\n"
   "with open(portfile + '-silent', 'w') as f:\n"
   "    f.write(str(silent.getsockname()[1]))\n"
   "class H(http.server.SimpleHTTPRequestHandler):\n"
   "    def log_message(self, fmt, *args):\n"
   "        with open(logfile, 'a') as f: f.write(self.path + '\\n')\n"
   "s = http.server.HTTPServer(('127.0.0.1', 0), H)\n"
   "with open(portfile + '.tmp', 'w') as f:\n"
   "    f.write(str(s.server_address[1]))\n"
   "os.rename(portfile + '.tmp', portfile)\n"
   "s.serve_forever()\n"))

(define server-port #f)
(define server-pid #f)

(define (server-start)
  ;; the port of the server, or #f
  (string-save server-script (web-file "server.py"))
  (string-save "" (web-file "log"))
  (system (string-append "python3 " (string-quote (web-dir-file "server.py"))
                         " " (string-quote web-dir)
                         " " (string-quote (web-dir-file "port"))
                         " " (string-quote (web-dir-file "log"))
                         " < /dev/null > /dev/null 2>&1 & echo $! > "
                         (string-quote (web-dir-file "pid"))))
  (with t0 (texmacs-time)
    (let loop ()
      (when (and (not (url-exists? (web-file "port")))
                 (< (- (texmacs-time) t0) 10000))
        (loop))))
  (when (url-exists? (web-file "pid"))
    (set! server-pid (string-trim-both (string-load (web-file "pid")))))
  (when (url-exists? (web-file "port"))
    (set! server-port (string-load (web-file "port"))))
  server-port)

(define (server-stop)
  ;; kill does not wait: a request made just after it could still be taken
  ;; by the server, which dies before it answers
  (when server-pid
    (system (string-append "kill " server-pid
                           "; i=0; while kill -0 " server-pid
                           " 2> /dev/null && [ $i -lt 100 ];"
                           " do sleep 0.1; i=$((i+1)); done"))
    (set! server-pid #f)))

(define (web-dir-file name)
  (string-append web-dir "/" name))

(define (web-url name)
  (string-append "http://127.0.0.1:" server-port "/" name))

(define (requests name)
  ;; the number of times the server was asked for @name
  (with l (string-tokenize-by-char (string-load (web-file "log")) #\newline)
    (length (list-filter l (cut == <> (string-append "/" name))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Files from the web
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (fetch name)
  ;; the content of the local copy of the file @name of the server
  (with local (url-concretize (web-url name))
    (if (== local "") "" (string-load (system->url local)))))

(define (test-web-cache)
  (check-group "files from the web")
  (string-save "one" (web-file "a.txt"))
  (check= (fetch "a.txt") "one")
  (with n (requests "a.txt")
    (check-true (> n 0))
    ;; kept for the session
    (string-save "two" (web-file "a.txt"))
    (check= (fetch "a.txt") "one")
    (check= (requests "a.txt") n)
    ;; downloaded again after remote-files-refresh, once
    (remote-files-refresh)
    (check= (fetch "a.txt") "two")
    (check-true (> (requests "a.txt") n))
    (with m (requests "a.txt")
      (check= (fetch "a.txt") "two")
      (check= (requests "a.txt") m))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Pictures with effects
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (with-art-buffer thunk)
  ;; a buffer with an art box which draws the remote picture pic.png with
  ;; an effect, so that it is made by make_file when the buffer is printed
  (let* ((old (current-buffer))
         (u (new-buffer)))
    (switch-to-buffer* u)
    (buffer-set-body u (stree->tree
                        `(document
                          (art-box "Hello world"
                                   (tuple "image" ,(web-url "pic.png")
                                          "effect" "0")))))
    (update-forced)
    (with r (check-run thunk)
      (when (and (pair? r) (== (car r) 'error))
        (check-report #f "the group" (object->string r)))
      (buffer-close u)
      (when (buffer-exists? old) (switch-to-buffer old)))))

(define (print-pictures)
  ;; print the current buffer, which draws its pictures
  (with pdf (web-file "out.pdf")
    (when (url-exists? pdf) (system-remove pdf))
    (print-to-file pdf)
    (url-exists? pdf)))

(define (test-remote-pictures)
  (check-group "pictures from the web with an effect")
  (system-copy (string->url "$TEXMACS_PATH/misc/images/fancy-c.png")
               (web-file "pic.png"))
  (with-art-buffer
    (lambda ()
      (check-true (print-pictures))
      (with n (requests "pic.png")
        (check= n 1)
        ;; kept for the session
        (check-true (print-pictures))
        (check= (requests "pic.png") n)
        ;; downloaded again by Tools > Update > Pictures (and in the next
        ;; session, in which the result of make_file is in system/make)
        (picture-gc)
        (check-true (print-pictures))
        (check= (requests "pic.png") (+ n 1))
        (check-true (print-pictures))
        (check= (requests "pic.png") (+ n 1))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Offline
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (test-offline)
  (check-group "offline: the copies of before")
  (server-stop)
  (remote-files-refresh)
  (check= (fetch "a.txt") "two")
  (with-art-buffer
    (lambda ()
      (picture-gc)
      (check-true (print-pictures))
      ;; the picture of before is drawn: the PDF has an image
      (check-true (string-occurs? "/Image"
                                  (string-load (web-file "out.pdf")))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; A server which does not answer
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (test-silent-server)
  ;; the download gives up (it used to wait for ever, qt_download_file)
  (check-group "a server which does not answer")
  (with silent (string-load (web-file "port-silent"))
    (check= (url-concretize
             (string-append "http://127.0.0.1:" silent "/b.txt"))
            "")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The suite
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (run-group thunk)
  ;; an error in a group counts as one failure
  (with r (check-run thunk)
    (when (and (pair? r) (== (car r) 'error))
      (check-report #f "the group" (object->string r)))))

(tm-define (web-files-test-failures)
  (:synopsis "Run the tests of the files and pictures fetched from the web")
  (check-suite "web-files")
  (clear-web-dir)
  (system-mkdir (system->url web-dir))
  (cond ((or (os-mingw?) (os-win32?))
         (display "  skipped on Windows\n"))
        ((not (url-exists-in-path? "python3"))
         (display "  skipped: no python3\n"))
        ((not (or (qt6-or-later-gui?)
                  (url-exists-in-path? "wget")
                  (url-exists-in-path? "curl")))
         (display "  skipped: no way to download (Qt 6, wget or curl)\n"))
        ((not (server-start))
         (server-stop)
         (check-report #f "the server" "python3 did not start"))
        (else
         (for-each run-group
                   (list test-web-cache test-remote-pictures
                         test-silent-server test-offline))
         (server-stop)))
  (clear-web-dir)
  (check-end))
