
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

;; The core has a file system of its own, in memory. A file of the user is
;; asked of the page, which puts its bytes under /user and says so
;; (tau-file-chosen); a file which is saved or exported is written under
;; /user and given to the page, which hands it to the user (docs/tau-design.md)

(texmacs-module (texmacs texmacs tau-files)
  (:use (kernel gui menu-serial)))

(define user-dir "/user")

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
  (:synopsis "The buffer @u was saved: a file of the user goes to the user")
  (when (string-starts? (url->system u) (string-append user-dir "/"))
    (tau-download u)))

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

(tm-define (tau-choose-file fun title type prompt name)
  (:synopsis "Ask the user for a file and call @fun with it")
  (cond ((== type "directory")
         (set-message "Directories cannot be chosen in the browser" title))
        ((== prompt "")
         (with ticket file-next
           (set! file-next (+ ticket 1))
           (ahash-set! file-waiting ticket fun)
           (tau-post "pick" ""
                     `((ticket . ,ticket)
                       (title . ,(cork->utf8 (translate title)))
                       (type . ,type)))))
        (else
         (ask-name title "File name:" (propose-name name type)
                   (lambda (s)
                     (with u (url-append (system->url user-dir)
                                         (url-tail (system->url s)))
                       (fun u)
                       (delayed
                         (:idle 1)
                         (tau-download u))))))))

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
