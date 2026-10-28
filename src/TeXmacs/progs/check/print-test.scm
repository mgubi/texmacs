;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : print-test.scm
;; DESCRIPTION : tests of File > Preview
;; COPYRIGHT   : (C) 2026  Massimiliano Gubinelli
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; File > Preview (preview-buffer, tm-print.scm) prints the document to
;; $TEXMACS_HOME_PATH/system/tmp/preview.pdf and shows it with the command
;; of preview-command-line: open on macOS, xdg-open on Linux. No viewer is
;; started here: preview-file, which runs the command, is replaced for the
;; checks of preview-buffer by a function which only records its argument.
;;
;; On macOS, a build without a PDF renderer (Vue) makes the PDF with
;; Ghostscript, found in the PATH: an application started from the Finder
;; did not find it, and the preview made no PDF and showed nothing (#546).
;; The PATH of the application is checked by tests/startup/check.sh; here
;; the PDF must be made, and a stale one must not be shown.

(texmacs-module (check print-test)
  (:use (check check-lib)
        (texmacs texmacs tm-print)))

(define preview-pdf "$TEXMACS_HOME_PATH/system/tmp/preview.pdf")

(define (pdf-file? u)
  (and (url-exists? u)
       (with s (string-load u)
         (and (>= (string-length s) 5)
              (== (substring s 0 5) "%PDF-")))))

(define (shell-word cmd prefix)
  ;; the value which /bin/sh gives to the rest of cmd after prefix
  (and (string-starts? cmd prefix)
       (eval-system (string-append "printf %s "
                                   (string-drop cmd (string-length prefix))))))

(define preview-names
  ;; a space, a quote, parentheses, and the ~ of iCloud Drive
  (list "/tmp/x.pdf" "/a b/it's (1).pdf"
        "/Users/x/Library/Mobile Documents/com~apple~CloudDocs/a.pdf"))

(define (test-command-line)
  (check-group "preview command")
  (with prefix (cond ((or (os-mingw?) (os-win32?)) #f)
                     ((os-macos?) "open ")
                     ((url-exists-in-path? "xdg-open") "xdg-open ")
                     (else #f))
    (if (not prefix)
        (check-true #t)
        ;; the file name, escaped by url-sys-concretize, comes back whole
        ;; from the shell: it is not escaped a second time
        (for-each
         (lambda (name)
           (with s (url-sys-concretize (system->url name))
             (check= (shell-word (preview-command-line s) prefix) name)))
         preview-names))))

(define (with-recorded-preview thunk)
  ;; preview-file records the file it is given instead of showing it
  (let* ((old preview-file)
         (shown '()))
    (set! preview-file (lambda (u) (set! shown (cons u shown))))
    (with r (check-run (lambda () (thunk (lambda () shown))))
      (set! preview-file old)
      (when (and (pair? r) (== (car r) 'error))
        (check-report #f "the group" (object->string r))))))

(define (test-preview-buffer)
  (check-group "preview of a document")
  (let* ((old (current-buffer))
         (u (new-buffer)))
    (switch-to-buffer* u)
    (buffer-set-body u (stree->tree '(document "A document to preview.")))
    (update-forced)
    (with-recorded-preview
      (lambda (shown)
        ;; a stale preview is replaced, and not shown if no PDF is made
        (string-save "stale" preview-pdf)
        (preview-buffer)
        (check-true (pdf-file? preview-pdf))
        (check= (length (shown)) 1)
        (check-true (and (pair? (shown))
                         (== (url-sys-concretize (car (shown)))
                             (url-sys-concretize preview-pdf))))))
    (buffer-pretend-saved u)
    (buffer-close u)
    (when (buffer-exists? old) (switch-to-buffer old))
    (when (url-exists? preview-pdf) (system-remove preview-pdf))))

(define (run-group thunk)
  ;; an error in a group counts as one failure
  (with r (check-run thunk)
    (when (and (pair? r) (== (car r) 'error))
      (check-report #f "the group" (object->string r)))))

(tm-define (print-test-failures)
  (:synopsis "Run the tests of File > Preview")
  (check-suite "print")
  (for-each run-group (list test-command-line test-preview-buffer))
  (check-end))
