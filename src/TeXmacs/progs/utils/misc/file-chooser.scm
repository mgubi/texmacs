
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
;; (vue_gui.cpp, issue #374). It is the chooser of Widkit, the interface
;; of TeXmacs on X11 (src/Plugins/Widkit/Misc/file_chooser_widget.cpp),
;; made of the widgets of TeXmacs:
;; - the fields Directory (Return goes there), File and Suffixes (the
;;   suffixes of the files shown, Return shows them);
;; - the directories (with ..) and the files, side by side: a click
;;   highlights an entry (a file goes into File), a click on the
;;   highlighted entry enters the directory or chooses the file;
;; - for an image, its width, height and position, and a preview;
;; - the buttons Home, Texts (~/.TeXmacs/texts), Ok and Cancel.
;; The field File is a field of a form: Ok reads what was typed in it.
;;
;; Each chooser has a state of its own, a table made by open-file-chooser
;; and given to the widget, so that several may be open: its fields are
;;   id        the name of its form and of its refreshables
;;   dir       the directory shown, a system path
;;   type      the format of the files, "image" or "directory"
;;   save?     a Save dialog
;;   suffixes  the suffixes shown, ("tm" "ts" "tp"); () for all the files
;;   name      the name of the file
;;   hilight   the entry highlighted, "d:name" or "f:name"
;;   done      called with the answer, or #f (a procedure, or a command
;;             of C++: vue_gui.cpp); #f once it has been called

(texmacs-module (utils misc file-chooser))

(define (fc-ref st key) (ahash-ref st key))
(define (fc-set! st key val) (ahash-set! st key val))

(define (fc-kind st what)
  (string-append (fc-ref st 'id) "-" what))

(define (fc-home)
  (or (getenv "HOME") (getenv "USERPROFILE") "/"))

(define fc-image-suffixes
  '("png" "jpg" "jpeg" "gif" "svg" "pdf" "eps" "ps" "tif" "tiff" "bmp"))

(define (fc-format-suffixes type)
  ;; the suffixes of the files of the format (Widkit: format-get-suffixes*)
  (cond ((== type "image") fc-image-suffixes)
        ((in? type '("generic" "directory")) '())
        (else
          ;; (tuple "tm" "ts" "tp")
          (let* ((t (format-get-suffixes* type))
                 (l (if (and (pair? t) (== (car t) 'tuple)) (cdr t) t)))
            (if (list? l) l '())))))

(define (fc-suffix-string st)
  ;; as the field Suffixes shows them: ".tm .ts .tp"
  (string-recompose (map (lambda (s) (string-append "." s))
                         (fc-ref st 'suffixes))
                    " "))

(define (fc-parse-suffixes s)
  ;; ".tm .ts" or "tm, ts" -> ("tm" "ts"); "" or "*" -> () (all the files)
  (let* ((s (string-replace s "," " "))
         (l (list-filter (string-decompose s " ") (cut != <> ""))))
    (list-filter (map (lambda (x) (cond ((string-starts? x "*.") (string-drop x 2))
                                        ((string-starts? x ".") (string-drop x 1))
                                        (else x)))
                      l)
                 (lambda (x) (and (!= x "") (!= x "*"))))))

(define (fc-entries st)
  ;; the directories (.. first) and the files of the directory
  (let* ((u (system->url (fc-ref st 'dir)))
         (l (if (url-directory? u) (url-read-directory u "*") '()))
         (suffixes (fc-ref st 'suffixes))
         (dirs? (== (fc-ref st 'type) "directory"))
         (shown? (lambda (x)
                   (let ((name (url->system (url-tail x))))
                     (and (!= name "") (not (string-starts? name "."))))))
         (names (lambda (l) (sort (map (lambda (x) (url->system (url-tail x))) l)
                                  string<?)))
         (dirs (list-filter l (lambda (x) (and (shown? x) (url-directory? x)))))
         (files (list-filter l (lambda (x)
                                 (and (not dirs?) (shown? x)
                                      (not (url-directory? x))
                                      (or (null? suffixes)
                                          (in? (locase-all (url-suffix x))
                                               suffixes)))))))
    (cons (cons ".." (names dirs)) (names files))))

(define (fc-path st name)
  (url->system (url-append (system->url (fc-ref st 'dir)) (system->url name))))

(define (fc-refresh st)
  (fc-set! st 'hilight "")
  (refresh-now (fc-kind st "directory"))
  (refresh-now (fc-kind st "lists")))

(define (fc-go st dir)
  ;; the directory @dir, a system path (~ and paths relative to the one
  ;; shown allowed)
  (let* ((d (if (string-starts? dir "~")
                (string-append (fc-home) (string-drop dir 1))
                dir))
         (u (system->url d))
         (u* (if (url-rooted? u) u
                 (url-append (system->url (fc-ref st 'dir)) u))))
    (when (url-directory? u*)
      (fc-set! st 'dir (url->system u*))
      (fc-refresh st))))

(define (fc-up st)
  (let* ((dir (fc-ref st 'dir))
         (parent (url->system (url-head (system->url dir)))))
    (when (and (!= parent "") (!= parent dir))
      (fc-go st parent))))

(define (fc-set-name st name)
  (fc-set! st 'name name)
  (form-named-set (fc-ref st 'id) "file" name)
  (refresh-now (fc-kind st "file"))
  (when (== (fc-ref st 'type) "image") (refresh-now (fc-kind st "preview"))))

(define (fc-click-directory st name)
  ;; a click highlights, a click on the highlighted entry enters it
  (with key (string-append "d:" name)
    (cond ((!= (fc-ref st 'hilight) key) (fc-set! st 'hilight key))
          ((== name "..") (fc-up st))
          (else (fc-go st (fc-path st name))))))

(define (fc-click-file st name quit)
  ;; a click highlights and fills File, a click on the highlighted file
  ;; chooses it
  (with key (string-append "f:" name)
    (if (!= (fc-ref st 'hilight) key)
        (begin
          (fc-set! st 'hilight key)
          (fc-set-name st name))
        (fc-ok st name quit))))

(define (fc-set-suffixes st s)
  (fc-set! st 'suffixes (fc-parse-suffixes s))
  (fc-refresh st))

(define (fc-finish st result quit)
  ;; the answer goes to a procedure, or to a command of C++ (vue_gui.cpp);
  ;; once only, also when the window is closed after (dialogue-window)
  (with done (fc-ref st 'done)
    (fc-set! st 'done #f)
    (quit)
    (cond ((procedure? done) (done result))
          (done (command-apply done result)))))

(define (fc-image-answer st path)
  ;; an image comes with the width, height and position typed, as in
  ;; Widkit; without them, the path alone (its size is then that of the
  ;; image, see vue_chooser_widget_rep::callback)
  (let ((params (map (lambda (f) (or (form-named-ref (fc-ref st 'id) f) ""))
                     '("width" "height" "xpos" "ypos"))))
    (if (list-and (map (cut == <> "") params)) path
        (cons path params))))

(define (fc-ok st name quit)
  ;; the name typed: a directory is entered, a file is the answer
  (let* ((name (or name (fc-ref st 'name) ""))
         (path (and (!= name "") (fc-path st name)))
         (dir? (and path (url-directory? (system->url path)))))
    (cond ((== (fc-ref st 'type) "directory")
           (fc-finish st (if dir? path (fc-ref st 'dir)) quit))
          ((not path) (noop))
          (dir?
           (fc-set-name st "")
           (fc-go st path))
          ((and (not (fc-ref st 'save?)) (not (url-exists? (system->url path))))
           (set-message (string-append "No file " name) "Open"))
          ((== (fc-ref st 'type) "image")
           (fc-finish st (fc-image-answer st path) quit))
          (else (fc-finish st path quit)))))

(define (fc-preview st)
  ;; the image whose name is in File, as a document
  (let* ((name (fc-ref st 'name))
         (path (and (!= name "") (fc-path st name))))
    (if (and path (url-exists? (system->url path))
             (not (url-directory? (system->url path))))
        `(document (image ,path "200px" "" "" ""))
        `(document ""))))

(define (fc-texts)
  (url->system (url-expand (string->url "$TEXMACS_HOME_PATH/texts"))))

(tm-widget ((file-chooser-widget st) quit)
  (padded
    (form (fc-ref st 'id)
      (aligned
        (item (text "Directory:")
          (refreshable (fc-kind st "directory")
            (input (when answer (fc-go st answer)) "string"
                   (list (fc-ref st 'dir)) "460px")))
        (assuming (!= (fc-ref st 'type) "directory")
          (item (text "File:")
            (refreshable (fc-kind st "file")
              (form-input "file" "string" (list (fc-ref st 'name)) "460px")))
          (item (text "Suffixes:")
            (input (when answer (fc-set-suffixes st answer)) "string"
                   (list (fc-suffix-string st)) "460px"))))
      ===
      (refreshable (fc-kind st "lists")
        (hlist
          (resize '("250px" "250px" "250px") '("280px" "280px" "280px")
            (choice (fc-click-directory st answer) (car (fc-entries st)) ""))
          (assuming (!= (fc-ref st 'type) "directory")
            //
            (resize '("250px" "250px" "250px") '("280px" "280px" "280px")
              (choice (fc-click-file st answer quit)
                      (cdr (fc-entries st)) "")))))
      (assuming (== (fc-ref st 'type) "image")
        ===
        (hlist
          (aligned
            (item (text "width:")
              (form-input "width" "string" '("") "100px"))
            (item (text "height:")
              (form-input "height" "string" '("") "100px"))
            (item (text "x-position:")
              (form-input "xpos" "string" '("") "100px"))
            (item (text "y-position:")
              (form-input "ypos" "string" '("") "100px")))
          // >>
          (refreshable (fc-kind st "preview")
            (resize "221px" "160px"
              (texmacs-output (fc-preview st) '(style "generic"))))))
      ===
      (explicit-buttons
        (hlist
          ("Home" (fc-go st (fc-home))) //
          ("Texts" (fc-go st (fc-texts)))
          >>>
          ("Ok" (fc-ok st (form-ref "file") quit)) //
          ("Cancel" (fc-finish st #f quit)))))))

(tm-define (open-file-chooser done title type save? dir name)
  (:synopsis "Choose a file with the widgets of TeXmacs")
  (:argument done "called with the answer, or #f (or a command)")
  (:argument title "the title of the window")
  (:argument type "the format of the files, or \"directory\" or \"generic\"")
  (:argument save? "a Save dialog")
  (:argument dir "the directory to start at (a system path, or \"\")")
  (:argument name "the name proposed")
  ;; the answer: the system path of the file chosen; for an image with a
  ;; size or a position typed, (path width height xpos ypos)
  (let ((st (make-ahash-table))
        (id (string-append "file-chooser-" (number->string (texmacs-time)))))
    (for (kv (list (cons 'id id) (cons 'type type) (cons 'save? save?)
                   (cons 'suffixes (fc-format-suffixes type))
                   (cons 'name name) (cons 'hilight "") (cons 'done done)
                   (cons 'dir (fc-home))))
      (fc-set! st (car kv) (cdr kv)))
    (when (and (string? dir) (!= dir "")) (fc-go st dir))
    (for (f '("width" "height" "xpos" "ypos"))
      (form-named-set id f ""))
    (form-named-set id "file" name)
    ;; (the last argument: the window closed by its button is a cancel)
    (dialogue-window (file-chooser-widget st)
                     (lambda x (fc-finish st #f noop))
                     (if (== title "") (if save? "Save file" "Open file") title)
                     (lambda () (fc-finish st #f noop)))))
