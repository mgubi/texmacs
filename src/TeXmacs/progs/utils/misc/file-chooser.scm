
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
;; - the directories (with ..) and the files, side by side, with the
;;   type, size and date of an entry in its balloon: a click enters a
;;   directory, a click on a file puts it into File, a second click
;;   chooses it;
;; - for an image, its width, height and position, and a preview;
;; - the type of the file chosen, when TeXmacs knows its format;
;; - the button Up beside Directory (as ..), and Home, Texts
;;   (~/.TeXmacs_Upsilon/texts), Ok and Cancel.
;; The dialog may be resized: the lists take the room.
;; The field File is a field of a form: Ok reads what was typed in it.
;; The state of a chooser is local to its widget (as select-remote-plugin-
;; widget in dynamic/session-menu.scm), and its form has a name of its
;; own: several choosers may be open.

(texmacs-module (utils misc file-chooser))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Directories and suffixes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (file-chooser-home)
  (or (getenv "HOME") (getenv "USERPROFILE") "/"))

(define (file-chooser-texts)
  (url->system (url-expand (string->url "$TEXMACS_HOME_PATH/texts"))))

(define file-chooser-image-suffixes
  '("png" "jpg" "jpeg" "gif" "svg" "pdf" "eps" "ps" "tif" "tiff" "bmp"))

(define (file-chooser-suffixes type)
  ;; the suffixes of the files of the format (Widkit: format-get-suffixes*)
  (cond ((== type "image") file-chooser-image-suffixes)
        ((in? type '("generic" "directory")) '())
        (else
          ;; (tuple "tm" "ts" "tp")
          (let* ((t (format-get-suffixes* type))
                 (l (if (and (pair? t) (== (car t) 'tuple)) (cdr t) t)))
            (if (list? l) l '())))))

(define (suffixes->string l)
  ;; as the field Suffixes shows them: ".tm .ts .tp"
  (string-recompose (map (lambda (s) (string-append "." s)) l) " "))

(define (string->suffixes s)
  ;; ".tm .ts" or "tm, ts" -> ("tm" "ts"); "" or "*" -> () (all the files)
  (let* ((s (string-replace s "," " "))
         (l (list-filter (string-decompose s " ") (cut != <> ""))))
    (list-filter (map (lambda (x) (cond ((string-starts? x "*.") (string-drop x 2))
                                        ((string-starts? x ".") (string-drop x 1))
                                        (else x)))
                      l)
                 (lambda (x) (and (!= x "") (!= x "*"))))))

(define (file-chooser-entries dir suffixes dirs-only?)
  ;; the directories (.. first) and the files of @dir
  (let* ((u (system->url dir))
         (l (if (url-directory? u) (url-read-directory u "*") '()))
         (shown? (lambda (x)
                   (with name (url->system (url-tail x))
                     (and (!= name "") (not (string-starts? name "."))))))
         (names (lambda (l)
                  (sort (map (lambda (x) (url->system (url-tail x))) l)
                        string<?)))
         (dirs (list-filter l (lambda (x) (and (shown? x) (url-directory? x)))))
         (files (list-filter l (lambda (x)
                                 (and (not dirs-only?) (shown? x)
                                      (not (url-directory? x))
                                      (or (null? suffixes)
                                          (in? (locase-all (url-suffix x))
                                               suffixes)))))))
    (cons (cons ".." (names dirs)) (names files))))

(define (file-chooser-path dir name)
  (url->system (url-append (system->url dir) (system->url name))))

(define (file-chooser-directory dir from)
  ;; the directory @dir (~ and paths relative to @from allowed), or #f
  (let* ((d (if (string-starts? dir "~")
                (string-append (file-chooser-home) (string-drop dir 1))
                dir))
         (u (system->url d))
         (u* (if (url-rooted? u) u (url-append (system->url from) u))))
    (and (url-directory? u*) (url->system u*))))

(define (file-chooser-type path)
  ;; the name of the format of the file @path, or "" (not recognized)
  (if (or (not path) (url-directory? (system->url path))) ""
      (with fm (url-format (system->url path))
        (or (and (string? fm) (!= fm "") (!= fm "generic")
                 (format-get-name fm))
            ""))))

(define (file-chooser-size n)
  (cond ((< n 1024) (string-append (number->string n) " bytes"))
        ((< n (* 1024 1024))
         (string-append (number->string (quotient (+ n 512) 1024)) " KB"))
        (else (string-append (number->string
                              (quotient (+ n (* 512 1024)) (* 1024 1024)))
                             " MB"))))

(define (file-chooser-info path)
  ;; the balloon of an entry: its type, its size and when it was modified
  (let* ((u (system->url path))
         (dir? (url-directory? u))
         (type (file-chooser-type path))
         (size (and (not dir?) (url-size u)))
         (time (url-last-modified u))
         (l (list (cond (dir? "Folder") ((!= type "") type) (else "File"))
                  (if (and (number? size) (>= size 0))
                      (file-chooser-size size) "")
                  (if (and (number? time) (> time 0))
                      (string-append "modified " (pretty-time time)) ""))))
    (string-recompose (list-filter l (cut != <> "")) ", ")))

(define (file-chooser-menu dir entries click)
  ;; the entries of a list as buttons, each with its balloon (a list of
  ;; choice has none); made again at each refresh of the lists (a promise,
  ;; which must yield an item: a glue when there are no entries)
  (if (null? entries) '(glue #f #f 0 0)
      (cons 'vertical
            (map (lambda (f)
                   (list (list 'balloon f
                               (if (== f "..") "The folder above"
                                   (file-chooser-info
                                     (file-chooser-path dir f))))
                         (lambda () (click f))))
                 entries))))

(define (file-chooser-preview path)
  ;; the image @path, as a document
  (if (and path (url-exists? (system->url path))
           (not (url-directory? (system->url path))))
      `(document (image ,path "200px" "" "" ""))
      `(document "")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The chooser
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define file-chooser-forms 0)

(tm-widget ((file-chooser-widget id type save? dir0 name0 finish) quit)
  ;; finish: the answer, see open-file-chooser
  (let* ((dirs-only? (== type "directory"))
         (dir dir0)
         (suffixes (file-chooser-suffixes type))
         (name name0)
         (hilight "")       ; the file clicked last
         (refresh
          (lambda ()
            (set! hilight "")
            (refresh-now "file-chooser-directory")
            (refresh-now "file-chooser-lists")))
         (go
          (lambda (d)
            (and-with d* (file-chooser-directory d dir)
              (set! dir d*)
              (refresh))))
         (up
          (lambda ()
            (with parent (url->system (url-head (system->url dir)))
              (when (and (!= parent "") (!= parent dir))
                (go parent)))))
         (set-name
          (lambda (s)
            (set! name s)
            (form-named-set id "file" s)
            (refresh-now "file-chooser-file")
            (refresh-now "file-chooser-type")
            (when (== type "image") (refresh-now "file-chooser-preview"))))
         (image-answer
          ;; with the width, height and position typed, as in Widkit;
          ;; without them, the path alone (its size is then that of the
          ;; image, see vue_chooser_widget_rep::callback)
          (lambda (path)
            (with params (map (lambda (f) (or (form-named-ref id f) ""))
                              '("width" "height" "xpos" "ypos"))
              (if (list-and (map (cut == <> "") params)) path
                  (cons path params)))))
         (ok
          ;; the name typed: a directory is entered, a file is the answer
          (lambda (s)
            (let* ((s (or s name ""))
                   (path (and (!= s "") (file-chooser-path dir s)))
                   (dir? (and path (url-directory? (system->url path)))))
              (cond (dirs-only? (finish (if dir? path dir)) (quit))
                    ((not path) (noop))
                    (dir? (set-name "") (go path))
                    ((and (not save?) (not (url-exists? (system->url path))))
                     (set-message (string-append "No file " s) "Open"))
                    ((== type "image") (finish (image-answer path)) (quit))
                    (else (finish path) (quit))))))
         (click-directory
          ;; a click enters the directory
          (lambda (s)
            (if (== s "..") (up) (go (file-chooser-path dir s)))))
         (click-file
          ;; a click fills File, a second click on the file chooses it
          (lambda (s)
            (if (!= hilight s)
                (begin (set! hilight s) (set-name s))
                (ok s)))))
    (padded
      (form id
        (aligned
          (item (text "Directory:")
            (hlist
              (refreshable "file-chooser-directory"
                (input (when answer (go answer)) "string" (list dir) "1w"))
              // (explicit-buttons ("Up" (up)))))

          (assuming (not dirs-only?)
            (item (text "File:")
              (refreshable "file-chooser-file"
                (form-input "file" "string" (list name) "1w")))
            (item (text "Type:")
              ;; (a text in a refreshable is evaluated when the dialog is
              ;; made: a promise makes it again at each refresh)
              (refreshable "file-chooser-type"
                ;; (in a row: a text alone in the menu of the refreshable
                ;; would be one of its labels, with their margin)
                (promise (list 'hlist
                               (list 'text (file-chooser-type
                                             (and (!= name "")
                                                  (file-chooser-path dir name))))))))

            (item (text "Suffixes:")
              (input (when answer
                       (set! suffixes (string->suffixes answer))
                       (refresh))
                     "string" (list (suffixes->string suffixes)) "1w"))))
        ===
        (refreshable "file-chooser-lists"
          (hlist
            (resize '("200px" "250px" "9999px") '("150px" "280px" "9999px")
              (scrollable
                (promise
                  (file-chooser-menu
                    dir (car (file-chooser-entries dir suffixes dirs-only?))
                    click-directory))))
            (assuming (not dirs-only?)
              //
              (resize '("200px" "250px" "9999px") '("150px" "280px" "9999px")
                (scrollable
                  (promise
                    (file-chooser-menu
                      dir (cdr (file-chooser-entries dir suffixes dirs-only?))
                      click-file)))))))
        (assuming (== type "image")
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
            (refreshable "file-chooser-preview"
              (resize "221px" "160px"
                (texmacs-output
                  (file-chooser-preview
                    (and (!= name "") (file-chooser-path dir name)))
                  '(style "generic"))))))
        ======
        (explicit-buttons
          (hlist
            ("Home" (go (file-chooser-home))) //
            ("Texts" (go (file-chooser-texts)))
            >>>
            ("Ok" (ok (form-ref "file"))) //
            ("Cancel" (finish #f) (quit))))))))

(tm-define (open-file-chooser done title type save? dir name)
  (:synopsis "Choose a file with the widgets of TeXmacs")
  (:argument done "called with the answer, or #f (or a command)")
  (:argument title "the title of the window")
  (:argument type "the format of the files, or \"directory\" or \"generic\"")
  (:argument save? "a Save dialog")
  (:argument dir "the directory to start at (a system path, or \"\")")
  (:argument name "the name proposed")
  ;; the answer: the system path of the file chosen; for an image with a
  ;; size or a position typed, (path width height xpos ypos). It is given
  ;; once (finish), also when the window is closed by its button; done is
  ;; a procedure, or a command of C++ (vue_gui.cpp)
  (set! file-chooser-forms (+ file-chooser-forms 1))
  (let* ((id (string-append "file-chooser-" (number->string file-chooser-forms)))
         (answered? #f)
         (finish (lambda (result)
                   (when (not answered?)
                     (set! answered? #t)
                     (if (procedure? done) (done result)
                         (command-apply done result)))))
         (dir* (or (and (string? dir) (!= dir "")
                        (file-chooser-directory dir (file-chooser-home)))
                   (file-chooser-home))))
    (for (f '("width" "height" "xpos" "ypos"))
      (form-named-set id f ""))
    (form-named-set id "file" name)
    (dialogue-window (file-chooser-widget id type save? dir* name finish)
                     (lambda x (finish #f))
                     (if (== title "") (if save? "Save file" "Open file") title)
                     (lambda () (finish #f)))))
