
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : file-chooser.scm
;; DESCRIPTION : a file chooser made of the widgets of TeXmacs
;; COPYRIGHT   : (C) 2026  Massimiliano Gubinelli
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; The file chooser of the interfaces which have none of the system: the
;; Vue interface on a Linux without the desktop portal and without zenity
;; (vue_gui.cpp, issue #374). As the chooser of Widkit
;; (src/Plugins/Widkit/Misc/file_chooser_widget.cpp): a folder, its
;; folders and its files (those of the format, or all of them), a name.

(texmacs-module (utils misc file-chooser))

;; the state of the chooser (one is open at a time)
(define fc-dir "")         ; the folder shown, a system path
(define fc-type "generic") ; the format of the files, or "directory"
(define fc-save? #f)       ; a Save dialog
(define fc-all? #f)        ; all the files, not those of the format only
(define fc-name "")        ; the name of the file
(define fc-done #f)        ; called with the system path chosen, or #f

(define (fc-home)
  (or (getenv "HOME") (getenv "USERPROFILE") "/"))

(define (fc-hidden? name)
  (and (> (string-length name) 0) (== (string-ref name 0) #\.)))

(define fc-image-suffixes
  '("png" "jpg" "jpeg" "gif" "svg" "pdf" "eps" "ps" "tif" "tiff" "bmp"))

(define (fc-suffixes)
  ;; the suffixes of the files of the format, or #f for all of them
  (cond ((== fc-type "image") fc-image-suffixes)
        ((in? fc-type '("generic" "directory")) #f)
        (else
          ;; (tuple "tm" "ts" "tp")
          (let* ((t (format-get-suffixes* fc-type))
                 (l (if (and (pair? t) (== (car t) 'tuple)) (cdr t) t)))
            (and (pair? l) l)))))

(define (fc-entries)
  ;; the folders and the files of the folder: (names-of-folders . names)
  (let* ((u (system->url fc-dir))
         (l (if (url-directory? u) (url-read-directory u "*") '()))
         (sufs (and (not fc-all?) (fc-suffixes)))
         (dirs '()) (files '()))
    (for (x l)
      (let ((name (url->system (url-tail x))))
        (when (and (!= name "") (not (fc-hidden? name)))
          (cond ((url-directory? x) (set! dirs (cons name dirs)))
                ((== fc-type "directory") (noop))
                ((or (not sufs) (in? (locase-all (url-suffix x)) sufs))
                 (set! files (cons name files)))))))
    (cons (sort dirs string<?) (sort files string<?))))

(define (fc-path name)
  (url->system (url-append (system->url fc-dir) (system->url name))))

(define (fc-refresh)
  (refresh-now "file-chooser-folder")
  (refresh-now "file-chooser-lists"))

(define (fc-go dir)
  ;; the folder @dir, a system path (~ and relative paths allowed)
  (let* ((d (if (string-starts? dir "~")
                (string-append (fc-home) (string-drop dir 1))
                dir))
         (u (system->url d))
         (u* (if (url-rooted? u) u (url-append (system->url fc-dir) u))))
    (when (url-directory? u*)
      (set! fc-dir (url->system u*))
      (fc-refresh))))

(define (fc-up)
  (let ((parent (url->system (url-head (system->url fc-dir)))))
    (when (and (!= parent "") (!= parent fc-dir))
      (fc-go parent))))

(define (fc-pick-file name)
  (set! fc-name name)
  (form-named-set "file-chooser" "name" name)
  (refresh-now "file-chooser-name"))

(define (fc-filter-names)
  (let ((all "All files")
        (sufs (fc-suffixes)))
    (if (not sufs) (list all)
        (list (string-append (format-get-name* fc-type) " ("
                             (string-recompose
                              (map (lambda (s) (string-append "*." s)) sufs)
                              " ")
                             ")")
              all))))

(define (format-get-name* type)
  (if (== type "image") "Images"
      (or (format-get-name type) type)))

(define (fc-set-filter answer)
  (set! fc-all? (== answer "All files"))
  (refresh-now "file-chooser-lists"))

(define (fc-finish result quit)
  ;; (the answer goes to a procedure, or to a command of C++: vue_gui.cpp)
  (with done fc-done
    (set! fc-done #f)
    (quit)
    (cond ((procedure? done) (done result))
          (done (command-apply done result)))))

(define (fc-ok name quit)
  ;; the name typed: a folder is entered, a file is the answer
  (let* ((name (or name fc-name ""))
         (path (and (!= name "") (fc-path name))))
    (cond ((== fc-type "directory")
           (fc-finish (if path (if (url-directory? (system->url path)) path fc-dir)
                          fc-dir)
                      quit))
          ((not path) (noop))
          ((url-directory? (system->url path))
           (fc-pick-file "")
           (fc-go path))
          ((or fc-save? (url-exists? (system->url path)))
           (fc-finish path quit))
          (else (set-message (string-append "No file " name) "Open")))))

(tm-widget ((file-chooser-widget) quit)
  (padded
    (form "file-chooser"
      (refreshable "file-chooser-folder"
        (hlist
          (text "Folder:") //
          (input (when answer (fc-go answer)) "string" (list fc-dir) "420px")
          //
          (explicit-buttons
            ("Up" (fc-up)) // ("Home" (fc-go (fc-home))))))
      ===
      (refreshable "file-chooser-lists"
        (hlist
          (resize '("260px" "260px" "260px") '("300px" "300px" "300px")
            (choice (fc-go (fc-path answer)) (car (fc-entries)) ""))
          (assuming (!= fc-type "directory")
            // //
            (resize '("260px" "260px" "260px") '("300px" "300px" "300px")
              (choice (fc-pick-file answer) (cdr (fc-entries)) "")))))
      (assuming (!= fc-type "directory")
        ===
        (aligned
          (item (text (if fc-save? "Save as:" "Name:"))
            (refreshable "file-chooser-name"
              (form-input "name" "string" (list fc-name) "420px")))
          (item (text "Show:")
            (enum (fc-set-filter answer) (fc-filter-names)
                  (car (fc-filter-names)) "420px"))))
      ===
      (bottom-buttons
        ("Cancel" (fc-finish #f quit)) >>
        (assuming (== fc-type "directory")
          ("Choose" (fc-ok "" quit)))
        (assuming (and (!= fc-type "directory") fc-save?)
          ("Save" (fc-ok (form-ref "name") quit)))
        (assuming (and (!= fc-type "directory") (not fc-save?))
          ("Open" (fc-ok (form-ref "name") quit)))))))

(tm-define (open-file-chooser done title type save? dir name)
  (:synopsis "Choose a file with the widgets of TeXmacs")
  (:argument done "called with the system path chosen, or #f (or a command)")
  (:argument title "the title of the window")
  (:argument type "the format of the files, or \"directory\" or \"generic\"")
  (:argument save? "a Save dialog")
  (:argument dir "the folder to start at (a system path, or \"\")")
  (:argument name "the name proposed (for a Save dialog)")
  (set! fc-type type)
  (set! fc-save? save?)
  (set! fc-all? #f)
  (set! fc-name name)
  (set! fc-done done)
  (set! fc-dir (fc-home))
  (when (and (string? dir) (!= dir "")) (fc-go dir))
  (form-named-set "file-chooser" "name" name)
  ;; (the last argument: the window closed by its button is a cancel)
  (dialogue-window (file-chooser-widget)
                   (lambda x (fc-finish #f noop))
                   (if (== title "") (if save? "Save file" "Open file") title)
                   (lambda () (fc-finish #f noop))))
