
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : part-tmfs.scm
;; DESCRIPTION : parts of documents as documents
;; COPYRIGHT   : (C) 2015  Joris van der Hoeven
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (part part-tmfs)
  (:use (part part-shared)
        (texmacs texmacs tm-files)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Useful subroutines
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (part-master name)
  (with pos (string-search-forwards ".tm/" 0 name)
    (if (<= pos 0)
        (tmfs-string->url name)
        (tmfs-string->url (substring name 0 (+ pos 3))))))

(tm-define (part-file name)
  (with pos (string-search-forwards ".tm/" 0 name)
    (if (<= pos 0)
        (tmfs-string->url name)
        (let* ((m (tmfs-string->url (substring name 0 (+ pos 3))))
               (s (substring name (+ pos 4) (string-length name)))
               (f (tmfs-string->url s)))
          (if (string-starts? s "here/")
              (url-relative m f)
              f)))))

(tm-define (part-open-name u)
  (with s (url->string u)
    (if (string-starts? s "tmfs://part/")
	(string-drop s (string-length "tmfs://part/"))
	(url->tmfs-string u))))

(tm-define (part-url master file)
  (cond ((== master file)
         (string-append "tmfs://part/" (url->tmfs-string master)))
        ((url-descends? file (url-head master))
         (string-append "tmfs://part/" (url->tmfs-string master)
                        "/" (url->tmfs-string (url-delta master file))))
        (else
         (string-append "tmfs://part/" (url->tmfs-string master)
                        "/" (url->tmfs-string file)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Subroutines for managing the initial environment
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (unpack-extra-inits l)
  (if (or (null? l) (null? (cdr l))) (list)
      (cons `(associate ,(car l) ,(cadr l))
            (unpack-extra-inits (cddr l)))))

(define (get-extra-init t delta)
  (cons `(associate "part-flag" "true")
        (if (and (tm-func? t 'tuple)
                 (tm-equal? (tm-ref t 0) delta))
            (unpack-extra-inits (cdr (tm-children t)))
            (list))))

(define (exclude-from-inherit)
  (list "preamble" "mode"
        "page-medium" "page-printed" "page-first"))

(define (master-inits mas u m)
  (let* ((mt (or (tmfile-extract mas 'initial) `(collection)))
         (xt (collection-exclude mt (exclude-from-inherit)))
         (aux (tmfile-extract mas 'auxiliary))
         (parts (collection-ref aux "parts"))
         (delta (url->unix (url-delta m u)))
         (l (if (tm-func? parts 'document) (tm-children parts) (list)))
         (xinit (append-map (cut get-extra-init <> delta) l))
         (t (collection-append xt `(collection ,@xinit)))
         (refs (tmfile-extract mas 'references))
         (lab (string-append "part:" delta))
         (ref (and refs (collection-ref refs lab))))
    (when (and ref (tm-func? ref 'tuple)
               (tm-ref ref 1)
               (tm-atomic? (tm-ref ref 1))
               (string-number? (tm->string (tm-ref ref 1))))
      (set! t (collection-set t "page-first"
                              (tm->string (tm-ref ref 1)))))
    t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Titles
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tmfs-title-handler (part name doc)
  (let* ((m (part-master name))
         (f (part-file name)))
    (if (== m f)
        (url->unix (url-basename (url-tail m)))
        (string-append (url->unix (url-basename (url-tail m))) " - "
                       (if (url-rooted-tmfs? f)
                           (tmfs-title f doc)
                           (url->unix (url-basename (url-tail f))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; What was loaded
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; A part view shows its file and, in place of each include of it, the body
;; of the included file in a (shared id file body) copy. For each file of a
;; part view, part-loaded holds the body of the file on disk and the body
;; of the copy when the view was loaded or last saved, under the key
;; (name . file), where name is the part view's name (part-open-name) and
;; file the url->unix of the file. Saving writes the files whose copy
;; differs from what was loaded, and asks first if the file on disk
;; changed since.

(define part-loaded (make-ahash-table))

(define (part-remember name u disk start)
  (ahash-set! part-loaded (cons name (url->unix u))
              (cons (tm->stree disk) (tm->stree start))))

(define (part-loaded-disk name u)
  (and-with p (ahash-ref part-loaded (cons name (url->unix u)))
    (car p)))

(define (part-loaded-start name u)
  (and-with p (ahash-ref part-loaded (cons name (url->unix u)))
    (cdr p)))

(define (part-file-body u)
  ;; the body of the file @u on disk, or #f
  (and (url-exists? u)
       (tmfile-extract (tree-import u "texmacs") 'body)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Loading
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (make-shared u body)
  (let* ((id (create-unique-id))
         (fid (url->unix u)))
    `(shared ,id ,fid ,(tm->stree body))))

(define (part-expand-body doc u name)
  (cond ((tm-func? doc 'document)
         (cons (tm-label doc)
               (map (cut part-expand-body <> u name) (tm-children doc))))
        ((and (or (tm-func? doc 'include 1)
                  (tm-func? doc 'include* 1))
              (tm-atomic? (tm-ref doc 0)))
         (let* ((su (url-relative u (tm->string (tm-ref doc 0))))
                (sdoc (tree-import su "texmacs"))
                (sbody (tmfile-extract sdoc 'body))
                ;; an open buffer of the file: its copy starts from it, so
                ;; that its unsaved changes are kept (part-shared.scm)
                (body (if (and sbody (buffer-exists? su))
                          (tm->stree (buffer-get-body su))
                          sbody)))
           (when sbody (part-remember name su sbody body))
           (make-shared su body)))
        (else doc)))

(define (part-expand doc mas u m name)
  (cond ((tm-atomic? doc) doc)
        ((tm-func? doc 'body 1)
         (with body (part-expand-body (tm-ref doc 0) u name)
           (part-remember name u (tm-ref doc 0) (tm-ref doc 0))
           (if (!= u m) (set! body (make-shared u body)))
           `(body ,body)))
        ((tm-in? doc '(style references auxiliary))
         (with val (tmfile-extract mas (tm-label doc))
           (if val `(,(tm-label doc) ,val) doc)))
        ((tm-is? doc 'initial)
         (let* ((mt (master-inits mas u m))
                (ft (tm-ref doc 0))
                (t  (collection-append mt ft)))
           `(initial ,t)))
        ((tm-is? doc 'document)
         (when (and (tmfile-extract doc 'body)
                    (not (tmfile-extract doc 'initial)))
           (set! doc (tmfile-assign doc 'initial (assoc->collection (list)))))
         (cons 'document
               (map (cut part-expand <> mas u m name)
                    (tm-children doc))))
        (else doc)))

(tmfs-load-handler (part name)
  (let* ((m (part-master name))
         (f (part-file name))
         (doc (tree-import f "texmacs"))
         (mas (if (== m f) doc (tree-import m "texmacs")))
         (exp (part-expand doc mas f m name)))
    (tmfs-document exp)))

(tmfs-master-handler (part name)
  (part-file name))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The copies of the included files in a part view
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (part-compress-body doc u)
  (cond ((tm-func? doc 'document)
         (cons (tm-label doc)
               (map (cut part-compress-body <> u) (tm-children doc))))
        ((tm-func? doc 'shared 3)
         (let* ((name  (tm->string (tm-ref doc 1)))
                (delta (url-delta u name)))
           `(include ,(url->unix delta))))
        (else doc)))

(define (part-own-body body u m)
  ;; the body of the file @u of a part view of the master @m, without the
  ;; shared copy which holds it when @u is not the master
  (when (and (!= u m) (tm-func? body 'shared))
    (set! body (tm-ref body :last)))
  (when (and (!= u m) (tm-func? body 'document 1)
             (tm-func? (tm-ref body 0) 'shared))
    (set! body (tm-ref body 0 :last)))
  body)

(define (part-copies body)
  ;; the list of (file . body) of the included files in @body
  (cond ((tm-func? body 'document)
         (append-map part-copies (tm-children body)))
        ((and (tm-func? body 'shared 3) (tm-atomic? (tm-ref body 1)))
         (list (cons (tm->string (tm-ref body 1))
                     (tm->stree (tm-ref body 2)))))
        (else (list))))

(define (part-view-info buf)
  ;; for the part view @buf: its name, file, own body and copies
  (let* ((name (part-open-name buf))
         (m (part-master name))
         (f (part-file name))
         (body (part-own-body (tm->stree (buffer-get-body buf)) f m)))
    (list name f body (part-copies body))))

(tm-define (part-view? buf)
  (and (url? buf) (url-rooted-tmfs-protocol? buf "part")))

(tm-define (part-view-unsaved-files buf)
  ;; the files of the part view @buf whose copies changed since they were
  ;; loaded or saved: its own file first, then the included files
  (with (name f body copies) (part-view-info buf)
    (append
     (with start (part-loaded-start name f)
       (if (and start (!= (part-compress-body body f) start)) (list f) (list)))
     (list-filter
      (map (lambda (c)
             (with start (part-loaded-start name (car c))
               (and start (!= (cdr c) start) (unix->url (car c)))))
           copies)
      (lambda (x) x)))))

(tm-define (part-file-unsaved? m u)
  ;; does the file @u of the master @m have unsaved changes, in a buffer of
  ;; its own or in a part view of @m?
  (with fid (url->unix u)
    (or (and (buffer-exists? u) (buffer-modified? u))
        (list-or
         (map (lambda (b)
                (and (part-view? b)
                     (buffer-modified? b)
                     (== (part-master (part-open-name b)) m)
                     (in? fid (map url->unix (part-view-unsaved-files b)))))
              (buffer-list))))))

(define (part-file-names l)
  (with names (map (lambda (u) (url->unix (url-tail u))) l)
    (string-recompose names ", ")))

(tm-define (buffer-close-question buf)
  (:require (part-view? buf))
  (with l (and (buffer-exists? buf) (part-view-unsaved-files buf))
    (if (and l (nnull? l))
        (string-append "The document has not been saved (changes in "
                       (part-file-names l) "). Really close it?")
        (former buf))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Saving the included files
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (part-writable? u)
  (or (and (url-test? u "f") (url-test? u "w"))
      (and (not (url-test? u "f")) (url-test? u "c"))))

(define (part-copy-action name fid body)
  ;; what saving does with the copy @body of the included file @fid:
  ;;   same        nothing changed in the copy (or the file has it)
  ;;   write       write the file
  ;;   buffer      save the open buffer of the file, which has the copy
  ;;   sync        set the open buffer, without changes of its own, to the
  ;;               copy and save it
  ;;   conflict    the open buffer has other unsaved changes: refused
  ;;   remote      a remote file or a file of the TeXmacs file system:
  ;;               refused
  ;;   unwritable  refused
  (let* ((u (unix->url fid))
         (start (part-loaded-start name u)))
    (cond ((and start (== body start)) 'same)
          ((or (url-rooted-tmfs? u) (url-rooted-web? u)) 'remote)
          ((buffer-exists? u)
           (with own (tm->stree (buffer-get-body u))
             (cond ((== own body) (if (buffer-modified? u) 'buffer 'same))
                   ((buffer-modified? u) 'conflict)
                   (else 'sync))))
          ((== (part-file-body u) body) 'same)
          ((not (part-writable? u)) 'unwritable)
          (else 'write))))

(define (part-changed-on-disk? name u action)
  ;; did the file @u change on disk since the part view @name loaded it?
  (cond ((== action 'buffer)
         (and-with mod-t (url-last-modified u)
           (and-with save-t (buffer-last-save u)
             (> mod-t save-t))))
        ((in? action '(write sync))
         (with disk (part-loaded-disk name u)
           (and disk (!= (part-file-body u) disk))))
        (else #f)))

(tm-define (part-save-plan buf)
  ;; for the part view @buf: the list of (file action changed-on-disk?) of
  ;; its included files
  (buffer-shared-flush)
  (with (name f body copies) (part-view-info buf)
    (map (lambda (c)
           (let* ((u (unix->url (car c)))
                  (action (part-copy-action name (car c) (cdr c))))
             (list u action (part-changed-on-disk? name u action))))
         copies)))

(define (part-write-body u body)
  ;; write @body into the file @u, keeping the rest of the file; #t on error
  (let* ((old (and (url-exists? u) (tree-import u "texmacs")))
         (doc (or (and old (tmfile-extract old 'body)
                       (tmfile-assign old 'body body))
                  `(document (TeXmacs ,(texmacs-version))
                             (style (tuple "generic"))
                             (body ,body)))))
    (tree-export (tm->tree doc) u "texmacs")))

(define part-save-overwrite (list))

;; The files written and refused (as (file . reason)) by the last save of a
;; part view; the part file comes first.
(tm-define part-save-written (list))
(tm-define part-save-refused (list))

(define (part-save-copies name copies)
  (with refuse (lambda (u why)
                 (set! part-save-refused
                       (append part-save-refused (list (cons u why)))))
    (for (c copies)
      (let* ((u (unix->url (car c)))
             (body (cdr c))
             (action (part-copy-action name (car c) body)))
        (cond ((== action 'same) (noop))
              ((== action 'remote)
               (refuse u "remote files are not saved from a part view"))
              ((== action 'conflict)
               (refuse u "it is open with other unsaved changes"))
              ((== action 'unwritable)
               (refuse u "the file is not writable"))
              ((and (part-changed-on-disk? name u action)
                    (nin? (url->unix u) part-save-overwrite))
               (refuse u "the file changed on disk"))
              (else
               (when (== action 'sync)
                 (buffer-shared-set-body u body))
               (if (if (== action 'write)
                       (part-write-body u body)
                       (buffer-save u))
                   (refuse u "the file could not be written")
                   (begin
                     (when (== action 'write)
                       (part-remember name u body body))
                     (when (!= action 'write)
                       (part-remember name u (or (part-file-body u) body)
                                      body))
                     (set! part-save-written
                           (append part-save-written (list u)))))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Saving
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (part-compress doc ori mas u m)
  (cond ((tm-atomic? doc) doc)
        ((tm-func? doc 'body 1)
         `(body ,(part-compress-body (part-own-body (tm-ref doc 0) u m) u)))
        ((tm-in? doc '(style references auxiliary))
         (with val (tmfile-extract ori (tm-label doc))
           (cond (val `(,(tm-label doc) ,val))
                 ((tm-is? doc 'style) doc)
                 (else `(,(tm-label doc) ,(assoc->collection (list)))))))
        ((tm-is? doc 'initial)
         (let* ((mt (master-inits mas u m))
                (ft (tm-ref doc 0))
                (t  (collection-delta mt ft)))
           `(initial ,t)))
        ((tm-is? doc 'document)
         (cons 'document
               (map (cut part-compress <> ori mas u m)
                    (tm-children doc))))
        (else doc)))

(tmfs-save-handler (part name doc)
  (buffer-shared-flush)
  (let* ((m (part-master name))
         (f (part-file name))
         (ori (tree-import f "texmacs"))
         (mas (if (== m f) doc (tree-import m "texmacs")))
         (com (part-compress doc ori mas f m))
         (body (part-own-body (tmfile-extract doc 'body) f m)))
    ;;(display* "com= " (tm->stree com) "\n")
    (set! part-save-written (list))
    (set! part-save-refused (list))
    (if (tree-export (tm->tree com) f "texmacs")
        (begin
          (buffer-pretend-modified f)
          (set! part-save-refused
                (list (cons f "the file could not be written"))))
        (begin
          (buffer-pretend-saved f)
          (with b (tmfile-extract com 'body)
            (part-remember name f b b))
          (set! part-save-written (list f))))
    (part-save-copies name (part-copies body))))

(tmfs-wrap-handler (part name)
  (part-file name))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Save command for part views
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (part-save-message)
  ;; the message after the last save of a part view
  (with refused (map (lambda (r)
                       (string-append (part-file-names (list (car r)))
                                      " (" (cdr r) ")"))
                     part-save-refused)
    (cond ((null? refused)
           (string-append "Saved " (part-file-names part-save-written)))
          ((null? part-save-written)
           (string-append "Not saved: " (string-recompose refused ", ")))
          (else
           (string-append "Saved " (part-file-names part-save-written)
                          "; not saved: " (string-recompose refused ", "))))))

(define (part-save-now buf)
  (if (buffer-save buf)
      (begin
        (buffer-pretend-modified buf)
        (set-message `(concat "Could not save " (verbatim ,(url->unix buf)))
                     "Save file"))
      (with msg (part-save-message)
        (set! part-save-overwrite (list))
        (set-message `(verbatim ,(utf8->cork msg)) "Save file")
        (when (nnull? part-save-refused)
          ;; the view keeps the changes which were not saved
          (buffer-pretend-modified buf)
          (show-message (utf8->cork msg) "Save file")))))

(tm-define (save-buffer-main . args)
  (:require (part-view? (if (and (nnull? args) (url? (car args)))
                            (car args) (current-buffer))))
  (let* ((buf (if (and (nnull? args) (url? (car args)))
                  (car args) (current-buffer)))
         (name (part-open-name buf))
         (f (part-file name))
         (changed (map car (list-filter (part-save-plan buf) caddr)))
         (disk (part-loaded-disk name f)))
    (when (and disk (!= (part-file-body f) disk))
      (set! changed (cons f changed)))
    (cond ((not (buffer-modified? buf))
           (set-message "No changes need to be saved" "Save file"))
          ((nnull? changed)
           (user-confirm
               (string-append (part-file-names changed)
                              " changed on disk since it was loaded."
                              " Really save?") #f
             (lambda (answ)
               (when answ
                 (set! part-save-overwrite (map url->unix changed))
                 (part-save-now buf)))))
          (else (part-save-now buf)))))
