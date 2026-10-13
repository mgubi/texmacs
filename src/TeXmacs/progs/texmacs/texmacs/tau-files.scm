
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : tau-files.scm
;; DESCRIPTION : the files of the user and the tabs of the page of Tau
;; COPYRIGHT   : (C) 2026  Massimiliano Gubinelli
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; The documents of the user are kept in the browser: the home directory of
;; the core is in its storage (misc/tau/web/tau-pre.js), with ~/Documents
;; for what the user saves. A file to open or to save is asked of the page,
;; which shows its panel of the files (misc/tau/web/files.mjs: the folders
;; of the home directory, where files of the computer are brought in) and
;; says what was chosen (tau-file-chosen); a file is given back to the
;; computer when it is exported, or on demand (tau-download-buffer), or
;; from the panel. See docs/tau-design.md.

(texmacs-module (texmacs texmacs tau-files)
  (:use (kernel gui menu-serial)))

;; (on the desktop the files are those of the computer: the home directory
;; of the user, and its Documents when there is one)
(define desktop-home (or (and (in-desktop?) (getenv "HOME")) ""))
(define user-dir
  (cond ((== desktop-home "") "/home/tau/Documents")
        ((url-exists? (system->url (string-append desktop-home "/Documents")))
         (string-append desktop-home "/Documents"))
        (else desktop-home)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Giving a file to the user
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define last-download "")
(define last-download-time 0)

(tm-define (tau-download u)
  (:synopsis "Give the file @u to the user")
  (with path (url->system u)
    (when (and (url-exists? u)
               (not (and (== path last-download)
                         (< (- (texmacs-time) last-download-time) 1000))))
      (set! last-download path)
      (set! last-download-time (texmacs-time))
      (tau-post "download" ""
                `((path . ,path)
                  (name . ,(url->system (url-tail u))))))))

(tm-define (web-open-pdf path name)
  (:synopsis "Show the PDF file @path to the user, under the name @name")
  ;; Print and Preview write a PDF and call this (tm-print.scm), as in the
  ;; browser build of TeXmacs: the page opens it in a tab of the browser,
  ;; where it is read, printed or saved
  (tau-post "open-pdf" "" `((path . ,path) (name . ,name))))

(tm-define (tau-tooltip-show id wid x y)
  (:synopsis "Show the widget @wid as the tooltip @id of the current view")
  ;; (x, y) from the top left corner of the view, y going down
  (with l (texmacs-widget-size wid)
    (tau-post "tooltip" ""
              `((id . ,(object->string id))
                (view . ,(caddr l))
                (width . ,(quotient (car l) 256))
                (height . ,(quotient (cadr l) 256))
                (x . ,x) (y . ,y)))))

(tm-define (tau-tooltip-hide id)
  (:synopsis "Take the tooltip @id away")
  (tau-post "tooltip" "" `((id . ,(object->string id)) (view . 0))))

(tm-define (tau-saved u)
  (:synopsis "The buffer @u was saved")
  ;; (it is kept in the browser: nothing more to do)
  (noop))

(tm-define (tau-download-buffer)
  (:synopsis "Give the file of the current document to the user")
  (with u (current-buffer)
    (cond ((buffer-modified? u)
           (set-message "Save the document first" "Download"))
          ((url-exists? u) (tau-download u))
          (else (set-message "The document has no file yet" "Download")))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Asking for a file
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define file-waiting (make-ahash-table)) ;; ticket -> what to do with the file
(define file-next 1)

(define (propose-name name type)
  "The name under which @name is saved in the format @type."
  (let* ((tail (url-tail name))
         (old (url-suffix tail))
         (new (if (in? type '("" "image" "directory")) ""
                  (format-default-suffix type))))
    ;; (a new document has no name: one with the suffix of the format, so
    ;; that what is exported is not saved as a document of TeXmacs)
    (if (url-scratch? name)
        (if (in? new '("" "tm")) "" (string-append "untitled." new))
        (if (and (!= old "") (!= new "") (!= old new)
                 (!= (format-from-suffix old) type))
            (url->system (url-glue (url-unglue tail (+ (string-length old) 1))
                                   (string-append "." new)))
            (url->system tail)))))

(define home-dir (if (== desktop-home "") "/home/tau" desktop-home))
(define last-dir user-dir) ;; where a file was last chosen

(define (in-home? dir)
  (or (== dir home-dir) (string-starts? dir (string-append home-dir "/"))))

(define (buffer-dir)
  "The directory of the current document, when it is one of the user."
  (let* ((u (current-buffer))
         (dir (if (or (url-scratch? u) (url-rooted-tmfs? u)) ""
                  (url->system (url-head u)))))
    (if (in-home? dir) dir last-dir)))

(define (ask-page mode fun title type name dir)
  "Ask the page for a file, in its panel of the files."
  (with ticket file-next
    (set! file-next (+ ticket 1))
    (ahash-set! file-waiting ticket fun)
    (tau-post "files" ""
              `((mode . ,mode)
                (ticket . ,ticket)
                (title . ,(cork->utf8 (translate title)))
                (type . ,type)
                (name . ,name)
                (dir . ,dir)))))

(tm-define (tau-choose-file fun title type prompt name)
  (:synopsis "Ask the user for a file and call @fun with it")
  (cond ((== type "directory")
         (set-message "Directories cannot be chosen in the browser" title))
        ((== prompt "")
         ;; a file which is kept in the browser; one of the computer is
         ;; added there, in the panel
         (ask-page "open" fun title type "" last-dir))
        (else
         (ask-page "save"
                   (lambda (u)
                     (fun u)
                     ;; what is exported is for the computer; a document
                     ;; of TeXmacs stays in the browser
                     (when (nin? type '("" "texmacs"))
                       (delayed
                         (:idle 1)
                         (tau-download u))))
                   title type (propose-name name type) (buffer-dir)))))

(tm-define (tau-files)
  (:synopsis "Show the files which are kept in the browser")
  (tau-post "files" "" `((mode . "browse") (ticket . 0) (dir . ,(buffer-dir)))))

(tm-define (tau-file-chosen ticket u)
  (:synopsis "The page gave the file @u for the question @ticket")
  ;; no file (the user gave up) is an empty path; no ticket is a document
  ;; to open (dropped on the page, or opened from the panel of the files)
  (let* ((fun (ahash-ref file-waiting ticket))
         (path (url->system u)))
    (ahash-remove! file-waiting ticket)
    (when (string-starts? path "/")
      (when (and fun (in-home? (url->system (url-head u))))
        (set! last-dir (url->system (url-head u))))
      (exec-delayed
        (lambda ()
          (protected-call
            (lambda () (if fun (fun u) (load-document u)))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The clipboard
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (tau-paste)
  (:synopsis "Paste what the clipboard of the browser has")
  ;; The key which pastes is the browser's, which gives its clipboard with
  ;; it. A command of a menu has to ask: the page reads the clipboard (the
  ;; browser may ask the user) and pastes, or says that it could not, and
  ;; what TeXmacs kept is pasted.
  (if (or (in-browser?) (in-desktop?))
      (tau-post "paste-request" "" '())
      (kbd-paste)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The tabs
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (tau-close-buffer name)
  (:synopsis "Close the buffer @name, after a question when it is modified")
  (if (buffer-modified? name)
      (user-confirm "The document has not been saved. Really close it?" #f
        (lambda (answ)
          (when answ (buffer-close name))))
      (buffer-close name)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The preferences of the interface, on the desktop
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; The program which has the windows (tau_shell.cpp) is another process:
;; the preferences are here, and those which it reads are said to it, all
;; when it asks (tau-say-preferences) and each when it changes.

(define tau-shell-preferences
  '("icon bars" "gui theme" "icon set" "smooth zoom"))

(define (tau-say-preference which)
  (tau-post "preference" ""
            `((name . ,which) (value . ,(get-preference which)))))

(tm-define (tau-say-preferences)
  (for-each tau-say-preference tau-shell-preferences))

(tm-define (notify-preference which)
  (:require (and (in-desktop?) (in? which tau-shell-preferences)))
  (former which)
  (tau-say-preference which))
