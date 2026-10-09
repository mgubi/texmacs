
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
;; the core is in its storage (misc/tau/web/tau-pre.js), and ~/Documents is
;; where they are saved and looked for. A file of the computer is asked of
;; the page, which puts it there and says so (tau-file-chosen); a file is
;; given back to the computer when it is exported, or on demand
;; (tau-download-buffer). See docs/tau-design.md.

(texmacs-module (texmacs texmacs tau-files)
  (:use (kernel gui menu-serial)))

(define user-dir "/home/tau/Documents")

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

(define (ask-name title prompt default cont)
  "Ask a name in a dialog and call @cont with it."
  (let* ((id (tau-dialog-new))
         (val default)
         (close (lambda () (tau-dialog-close id)))
         (ok (lambda () (close) (when (!= val "") (cont val))))
         (input `(input ,(lambda (s) (when (string? s) (set! val s)))
                        "string" ,(lambda () (list default)) "24em"))
         (menu `(vertical
                  (glue #f #f 0 10)
                  (hlist (glue #f #f 16 0)
                         (aligned (item (text ,prompt) ,input))
                         (glue #f #f 16 0))
                  (glue #f #f 0 12)
                  (hlist (glue #t #f 16 0)
                         (style ,widget-style-button
                                ("Cancel" ,close) ("Save" ,ok))
                         (glue #f #f 16 0))
                  (glue #f #f 0 10))))
    (tau-dialog-show id menu title close '(submit . #t))))

(define (propose-name name type)
  "The name under which @name is saved in the format @type."
  (if (url-scratch? name) ""
      (let* ((tail (url-tail name))
             (old (url-suffix tail))
             (new (if (in? type '("" "image" "directory")) ""
                      (format-default-suffix type))))
        (if (and (!= old "") (!= new "") (!= old new)
                 (!= (format-from-suffix old) type))
            (url->system (url-glue (url-unglue tail (+ (string-length old) 1))
                                   (string-append "." new)))
            (url->system tail)))))

(define (stored-files)
  "The names of the documents which are kept, the last changed first."
  (let* ((dir (system->url user-dir))
         (l (list-filter (url-read-directory dir "*") url-regular?))
         (l* (list-sort l (lambda (u v) (> (url-last-modified u)
                                           (url-last-modified v))))))
    (map (lambda (u) (url->system (url-tail u))) l*)))

(define (stored-url name)
  (url-append (system->url user-dir) (url-tail (system->url name))))

(define (ask-computer fun title type)
  "Ask the page for a file of the computer."
  (with ticket file-next
    (set! file-next (+ ticket 1))
    (ahash-set! file-waiting ticket fun)
    (tau-post "pick" ""
              `((ticket . ,ticket)
                (title . ,(cork->utf8 (translate title)))
                (type . ,type)))))

(define (ask-stored fun title type names)
  "A dialog with the documents which are kept: one is opened or deleted,
   or a file of the computer is asked for."
  (let* ((id (tau-dialog-new))
         (sel (car names))
         (close (lambda () (tau-dialog-close id)))
         (open (lambda () (close) (fun (stored-url sel))))
         (computer (lambda () (close) (ask-computer fun title type)))
         (delete (lambda ()
                   (close)
                   (user-confirm `(concat "Delete " (verbatim ,sel) "?") #f
                     (lambda (answ)
                       (when answ (url-remove (stored-url sel)))
                       (tau-choose-file fun title type "" (url-none))))))
         (menu `(vertical
                  (glue #f #f 0 8)
                  (hlist (glue #f #f 16 0)
                         (vertical
                           (text "Documents kept in this browser")
                           (glue #f #f 0 4)
                           (resize ,(lambda () "26em") ,(lambda () "16em")
                             (choice ,(lambda (x) (when (string? x) (set! sel x)))
                                     ,(lambda () names)
                                     ,(lambda () sel))))
                         (glue #f #f 16 0))
                  (glue #f #f 0 12)
                  (hlist (glue #f #f 16 0)
                         (style ,widget-style-button
                                ("From this computer" ,computer)
                                ("Delete" ,delete))
                         (glue #t #f 16 0)
                         (style ,widget-style-button
                                ("Cancel" ,close) ("Open" ,open))
                         (glue #f #f 16 0))
                  (glue #f #f 0 10))))
    (tau-dialog-show id menu title close)))

(tm-define (tau-choose-file fun title type prompt name)
  (:synopsis "Ask the user for a file and call @fun with it")
  (cond ((== type "directory")
         (set-message "Directories cannot be chosen in the browser" title))
        ((== prompt "")
         ;; a document which is kept, or a file of the computer: of the
         ;; computer at once when nothing is kept, or for an image
         (with names (if (in? type '("" "texmacs" "generic")) (stored-files) '())
           (if (null? names)
               (ask-computer fun title type)
               (ask-stored fun title type names))))
        (else
         (ask-name title "File name:" (propose-name name type)
                   (lambda (s)
                     (with u (stored-url s)
                       (fun u)
                       ;; what is exported is for the computer; a document
                       ;; of TeXmacs stays in the browser
                       (when (nin? type '("" "texmacs"))
                         (delayed
                           (:idle 1)
                           (tau-download u)))))))))

(tm-define (tau-file-chosen ticket u)
  (:synopsis "The page gave the file @u for the question @ticket")
  (with fun (ahash-ref file-waiting ticket)
    (ahash-remove! file-waiting ticket)
    (exec-delayed
      (lambda ()
        (protected-call
          (lambda () (if fun (fun u) (load-document u))))))))

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
