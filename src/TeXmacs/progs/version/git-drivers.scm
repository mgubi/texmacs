
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : git-drivers.scm
;; DESCRIPTION : structured merging of TeXmacs documents from within Git
;; COPYRIGHT   : (C) 2026  Massimiliano Gubinelli
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Git can be told to merge TeXmacs documents with TeXmacs itself:
;;   .gitattributes:  *.tm merge=texmacs
;;   git config:      merge.texmacs.driver "<texmacs> -headless -x
;;                      '(git-merge-driver \"%O\" \"%A\" \"%B\")'"
;; The driver writes the structured three way merge into the file of our
;; version, and exits with a non zero code if conflicts remain.  The
;; conflicts are then marked up as differences, which can be resolved
;; in TeXmacs, instead of the usual textual conflict markers.

(texmacs-module (version git-drivers)
  (:use (version version-merge)
        (version git-base)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The merge driver
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (driver-url name)
  (with u (system->url name)
    (if (url-rooted? u) u (url-append (url-pwd) u))))

(define (load-document name)
  (with t (tree->stree (tree-import (driver-url name) "texmacs"))
    (and (document-body t) t)))

(tm-define (git-merge-files base ours theirs)
  (:synopsis "Merge @theirs into @ours with ancestor @base; #f on failure")
  ;; Returns the number of remaining conflicts
  (let* ((o (load-document base))
         (a (load-document ours))
         (b (load-document theirs)))
    ;; NOTE: the preamble is merged too: the style packages, the initial
    ;; environment (#676)
    (and o a b
         (with r (merge-documents o a b)
           (and (not (tree-export (stree->tree (car r)) (driver-url ours)
                                  "texmacs"))
                (cadr r))))))

(tm-define (git-textual-merge base ours theirs)
  ;; When the structured merge fails (e.g. for a document which cannot be
  ;; parsed), fall back on the textual merge of Git, with conflict markers
  (with ret (evaluate-system (list (get-preference "git executable")
                                   "merge-file" "-L" "ours" "-L" "base"
                                   "-L" "theirs"
                                   (url->system (driver-url ours))
                                   (url->system (driver-url base))
                                   (url->system (driver-url theirs)))
                             '() '() '(1 2))
    (if (== (car ret) "0") 0 1)))

(tm-define (git-merge-driver base ours theirs)
  (:synopsis "Entry point for the merge driver of Git")
  (with n (catch #t
            (lambda () (git-merge-files base ours theirs))
            (lambda args #f))
    (quit-TeXmacs-code (cond ((== n 0) 0)
                             ((number? n) 1)
                             (else (git-textual-merge base ours theirs))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Installing the driver in a repository
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (git-driver-program tp bin-path appdir appimage windows? exists?)
  (:synopsis "The program run by the merge driver, and its TEXMACS_PATH")
  ;; @tp and @bin-path: TEXMACS_PATH and TEXMACS_BIN_PATH as paths of the
  ;; system ("" when unset), @appdir and @appimage: APPDIR and APPIMAGE (the
  ;; AppImage which runs, ""), @exists?: does a path of the system exist.
  ;; The result is the program and the TEXMACS_PATH to give it (#f: none)
  (let* ((in-bin (lambda (dir names)
                   (map (lambda (n) (string-append dir "/bin/" n)) names)))
         ;; the binary of the build tree (MinGW adds .exe to texmacs.bin),
         ;; of the Windows bundle and installer (texmacs.exe), of the macOS
         ;; bundle (TEXMACS_PATH is Contents/Resources/share/TeXmacs), and
         ;; of the installs of make install (bin is TEXMACS_BIN_PATH/bin)
         (l (append (in-bin tp (if windows?
                                   '("texmacs.exe" "texmacs.bin.exe"
                                     "texmacs.bin")
                                   '("texmacs.bin")))
                    (if windows? '()
                        (list (string-append tp "/../../../MacOS/TeXmacs")))
                    (if (or windows? (== bin-path "")) '()
                        (in-bin bin-path '("texmacs.bin")))))
         (found (list-find l exists?))
         ;; Git for Windows runs the driver with its sh, for which C:/x/y
         ;; is safer than C:\x\y
         (slashes (lambda (s) (if windows? (string-replace s "\\" "/") s))))
    (cond ((and (!= appdir "") (!= appimage "") (string-starts? tp appdir))
           ;; the AppImage is mounted while it runs only: the driver runs
           ;; the AppImage itself, whose AppRun sets TEXMACS_PATH
           (list appimage #f))
          (found (list (slashes found) (slashes tp)))
          ;; the startup script, which knows its own TEXMACS_PATH
          (else (list "texmacs" #f)))))

(define (getenv* var)
  (or (getenv var) ""))

(tm-define (git-merge-driver-program)
  (:synopsis "The program run by the merge driver, and its TEXMACS_PATH")
  (git-driver-program (url->system (get-texmacs-path))
                      (getenv* "TEXMACS_BIN_PATH")
                      (getenv* "APPDIR") (getenv* "APPIMAGE")
                      (or (os-mingw?) (os-win32?))
                      (lambda (s) (url-exists? (system->url s)))))

(tm-define (git-merge-driver-command)
  (:synopsis "The shell command which Git should use for merging documents")
  ;; Git runs it with sh -c, also on Windows (the sh of Git for Windows)
  (let* ((p (git-merge-driver-program))
         (tp (cadr p)))
    (string-append (if tp (string-append "TEXMACS_PATH="
                                         (git-shell-quote tp) " ")
                       "")
                   (git-shell-quote (car p))
                   " -headless -x "
                   (git-shell-quote "(git-merge-driver \"%O\" \"%A\" \"%B\")")
                   " > /dev/null 2>&1")))

(define attributes-line "*.tm merge=texmacs")

(define (attributes-file root) (url-append root ".gitattributes"))

(tm-define (git-merge-driver-installed? root)
  (:synopsis "Does @root merge TeXmacs documents with TeXmacs?")
  (and (git-output root "config" "--get" "merge.texmacs.driver")
       (with f (attributes-file root)
         (and (url-exists? f)
              (in? attributes-line (git-split (string-load f) "\n"))))))

(tm-define (git-install-merge-driver-now root)
  (:synopsis "Make Git merge TeXmacs documents in @root with TeXmacs")
  ;; Returns (ok? . message).  Nothing is installed when Git refuses to
  ;; run, for instance in a folder which is not trusted (#676)
  ;; NOTE: the driver itself is configured locally, since it depends on
  ;; the installation; .gitattributes can be shared with collaborators,
  ;; for whom Git falls back to its textual merge
  (let* ((r1 (git-run root "config" "merge.texmacs.name"
                      "TeXmacs structured merge"))
         (r2 (if (git-ok? r1)
                 (git-run root "config" "merge.texmacs.driver"
                          (git-merge-driver-command))
                 r1)))
    (when (and (git-ok? r1) (not (git-ok? r2)))
      ;; no driver half configured
      (git-run root "config" "--unset" "merge.texmacs.name"))
    (if (not (git-ok? r2))
        (cons #f (with msg (git-message r2)
                   (if (== msg "") "Git could not be configured" msg)))
        (let* ((f (attributes-file root))
               (s (if (url-exists? f) (string-load f) "")))
          (when (nin? attributes-line (git-split s "\n"))
            (string-save (string-append s
                                        (if (or (== s "")
                                                (string-ends? s "\n"))
                                            "" "\n")
                                        attributes-line "\n")
                         f))
          (git-invalidate root)
          (cons #t "TeXmacs documents will be merged structurally")))))

(tm-define (git-install-merge-driver root)
  (:synopsis "Make Git merge TeXmacs documents in @root with TeXmacs")
  (with r (git-install-merge-driver-now root)
    (set-message (cdr r) "Git")
    (car r)))
