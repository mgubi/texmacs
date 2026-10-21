
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
        (texmacs texmacs tm-files)
        (utils library cursor)))

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

(define part-loaded-style (make-ahash-table))

(define (part-style s)
  ;; the style @s as a tuple: a file with one style has it as a string
  (with s (and s (tm->stree s))
    (if (string? s) `(tuple ,s) s)))

(define (part-remember-style name style)
  ;; the style of the master when the part view @name was loaded or saved
  (ahash-set! part-loaded-style name (part-style style)))

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
        ((and (tm-in? doc '(show-part hide-part)) (== (tm-arity doc) 3))
         ;; a document saved with its parts: the body of a part is expanded,
         ;; its alternative (evaluated when it is hidden) is not
         `(,(tm-label doc) ,(tm-ref doc 0)
           ,(part-expand-body (tm-ref doc 1) u name) ,(tm-ref doc 2)))
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
           (when (tm-is? doc 'style)
             (part-remember-style name (or val (tm-ref doc 0))))
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

(define (part-paragraphs-flat l)
  ;; the paragraphs @l, with the paragraphs of the bodies of the parts
  ;; (show-part, hide-part) in place of the parts
  (append-map (lambda (x)
                (if (and (tm-in? x '(show-part hide-part)) (== (tm-arity x) 3))
                    (with b (tm-ref x 1)
                      (part-paragraphs-flat
                       (if (tm-func? b 'document) (tm-children b) (list b))))
                    (list x)))
              l))

(define (part-compress-body doc u)
  ;; the parts (show-part, hide-part) are kept, with the copies in their
  ;; bodies and alternatives turned back into inclusions
  (cond ((tm-func? doc 'document)
         (cons (tm-label doc)
               (map (cut part-compress-body <> u) (tm-children doc))))
        ((and (tm-in? doc '(show-part hide-part)) (== (tm-arity doc) 3))
         `(,(tm-label doc) ,(tm-ref doc 0)
           ,(part-compress-body (tm-ref doc 1) u)
           ,(part-compress-body (tm-ref doc 2) u)))
        ((tm-func? doc 'shared 3)
         (let* ((name  (tm->string (tm-ref doc 1)))
                (delta (url-delta u name)))
           `(include ,(url->unix delta))))
        (else doc)))

(define (part-own-body body u m)
  ;; the body of the file @u of a part view of the master @m, without the
  ;; shared copy which holds it when @u is not the master
  (when (and (!= u m) (tm-func? body 'document))
    (set! body (cons 'document (part-paragraphs-flat (tm-children body)))))
  (when (and (!= u m) (tm-func? body 'shared))
    (set! body (tm-ref body :last)))
  (when (and (!= u m) (tm-func? body 'document 1)
             (tm-func? (tm-ref body 0) 'shared))
    (set! body (tm-ref body 0 :last)))
  body)

(define (part-copies body)
  ;; the list of (file . body) of the included files in @body
  (cond ((tm-func? body 'document)
         (append-map part-copies (part-paragraphs-flat (tm-children body))))
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
  ;; loaded or saved: its own file first, then the included files, then the
  ;; master when the style of the view changed
  (with (name f body copies) (part-view-info buf)
    (let* ((own (with start (part-loaded-start name f)
                  (if (and start (!= (part-compress-body body f) start))
                      (list f) (list))))
           (incl (list-filter
                  (map (lambda (c)
                         (with start (part-loaded-start name (car c))
                           (and start (!= (cdr c) start) (unix->url (car c)))))
                       copies)
                  (lambda (x) x)))
           (m (part-master name))
           (l (append own incl)))
      (if (and (part-style-changed? name (part-buffer-style buf))
               (nin? (url->unix m) (map url->unix l)))
          (append l (list m))
          l))))

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
;; part view; the part file comes first. The pending files are open masters
;; which got the style of the view but are not saved (they have other
;; unsaved changes).
(tm-define part-save-written (list))
(tm-define part-save-refused (list))
(tm-define part-save-pending (list))

(define (part-save-refuse u why)
  (set! part-save-refused (append part-save-refused (list (cons u why)))))

(define (part-save-wrote u)
  (set! part-save-written (append part-save-written (list u))))

(define (part-save-copies name copies)
  (with refuse part-save-refuse
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
                     (part-save-wrote u)))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The style of the master
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; A part view has the style of its master, and a change of style in the
;; view is a change of the style of the master: saving the view gives the
;; master the new style. When the master is open, its buffer gets the style
;; and is saved, unless it has other unsaved changes: then it is only left
;; modified (part-save-pending). Otherwise the style is written into the
;; file of the master, whose other contents stay as they are on disk.
;;
;; The references and the auxiliary data of a part view are not saved:
;; they are computed for the part only (labels of the copies, pages which
;; do not follow those of the master), so that they would replace the
;; complete ones of the master with partial ones. The file of the part
;; keeps those on disk, and the master gets its own when it is typeset.

(define (part-file-style u)
  ;; the style of the file @u on disk, or #f
  (and (url-exists? u)
       (part-style (tmfile-extract (tree-import u "texmacs") 'style))))

(define (part-buffer-style buf)
  ;; the style of the buffer @buf
  (part-style (buffer-get-style buf)))

(define (part-style-changed? name style)
  ;; is the style @style of the part view @name not the one of its master
  ;; when the view was loaded or saved?
  (with start (ahash-ref part-loaded-style name)
    (and style start (!= (part-style style) start))))

(define (part-set-buffer-style buf style)
  ;; give the open buffer @buf the style @style, in all its views (and in
  ;; its data, for a master loaded without a view for the references of a
  ;; chapter)
  (with st (part-style style)
    (when (!= (part-buffer-style buf) st)
      (buffer-set-style buf st))))

(define (part-write-style u style)
  ;; write @style into the file @u, keeping the rest of the file; #t on
  ;; error
  (with doc (tmfile-assign (tree-import u "texmacs") 'style style)
    (or (not doc) (tree-export (tm->tree doc) u "texmacs"))))

(define (part-master-style-action name m style)
  ;; what saving the part view @name, whose style is @style, does with the
  ;; style of its master @m (an included file is the part file):
  ;;   same        the style did not change (or the master has it)
  ;;   buffer      set the style in the open buffer of the master, save it
  ;;   pending     set the style in the open buffer of the master, which
  ;;               has other unsaved changes: it is left modified
  ;;   write       write the style into the file of the master
  ;;   remote      refused (remote file, TeXmacs file system)
  ;;   unwritable  refused
  (cond ((not (part-style-changed? name style)) 'same)
        ((or (url-rooted-tmfs? m) (url-rooted-web? m)) 'remote)
        ((buffer-exists? m)
         (cond ((buffer-modified? m) 'pending)
               ((== (part-buffer-style m) (part-style style)) 'same)
               (else 'buffer)))
        ((== (part-file-style m) (part-style style)) 'same)
        ((not (part-writable? m)) 'unwritable)
        (else 'write)))

(define (part-style-changed-on-disk? name m action)
  ;; did the master @m change on disk since the part view @name was loaded?
  (cond ((== action 'buffer)
         (and-with mod-t (url-last-modified m)
           (and-with save-t (buffer-last-save m)
             (> mod-t save-t))))
        ((== action 'write)
         (!= (part-file-style m) (ahash-ref part-loaded-style name)))
        (else #f)))

(tm-define (part-master-style-plan buf)
  ;; for the part view @buf: (master action changed-on-disk?) for the
  ;; style of its master
  (let* ((name (part-open-name buf))
         (m (part-master name))
         (f (part-file name))
         (style (part-buffer-style buf))
         (action (cond ((!= f m) (part-master-style-action name m style))
                       ((part-style-changed? name style) 'write)
                       (else 'same))))
    (list m action (part-style-changed-on-disk? name m action))))

(define (part-save-master-style name m style)
  (with action (part-master-style-action name m style)
    (cond ((== action 'same)
           (when style (part-remember-style name style)))
          ((== action 'remote)
           (part-save-refuse m "style: remote files are not saved from a part view"))
          ((== action 'unwritable)
           (part-save-refuse m "style: the file is not writable"))
          ((and (part-style-changed-on-disk? name m action)
                (nin? (url->unix m) part-save-overwrite))
           (part-save-refuse m "style: the file changed on disk"))
          ((== action 'write)
           (if (part-write-style m style)
               (part-save-refuse m "style: the file could not be written")
               (begin
                 (part-remember-style name style)
                 (part-save-wrote m))))
          ((== action 'pending)
           ;; (the style is still remembered as the old one: the file of
           ;; the master does not have the new one yet)
           (part-set-buffer-style m style)
           (set! part-save-pending (append part-save-pending (list m))))
          (else
           (part-set-buffer-style m style)
           (if (buffer-save m)
               (part-save-refuse m "style: the file could not be written")
               (begin
                 (part-remember-style name style)
                 (part-save-wrote m)))))))

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

;; The own file of a part view may be open as an ordinary buffer. The view
;; of a master is not shared with the buffer of the master, and the buffer
;; of an included file diverges from the view of that file when both have
;; changes (part-shared.scm). That buffer is marked saved only when it has
;; what is written, and its unsaved changes are never lost: as for an
;; included file (part-copy-action), a buffer with the body of the view
;; and changes of its own (outside the body: style, initial environment)
;; is saved itself, and a buffer with other changes of the body is
;; refused.

(define (part-buffer-own-body f)
  ;; the body of the open buffer of the file @f, with inclusions in place
  ;; of the copies of the included files which it may have received from
  ;; a part view (buffer-restore in part-shared.scm)
  (part-compress-body (tm->stree (buffer-get-body f)) f))

(define (part-own-action f body)
  ;; what saving a part view does with an open buffer of its own file @f,
  ;; which gets the body @body:
  ;;   none      no open buffer: the file is written
  ;;   same      the buffer, without changes, has @body: the file is
  ;;             written
  ;;   buffer    the buffer has @body and changes of its own: it is saved
  ;;   sync      the buffer, without changes of its own, gets @body: the
  ;;             file is written
  ;;   conflict  the buffer has other unsaved changes: refused
  (cond ((not (buffer-exists? f)) 'none)
        ((!= (part-buffer-own-body f) (tm->stree body))
         (if (buffer-modified? f) 'conflict 'sync))
        ((buffer-modified? f) 'buffer)
        (else 'same)))

;; The initial environment of the own file: when the open buffer of the
;; file is saved itself, it first gets the entries which the view changed
;; (those which the file would get from the view and which it does not
;; have on disk), the view winning as for the style.

(define (part-init-entries c)
  ;; the (var val) of the collection @c
  (if (not (and c (tm-func? c 'collection))) (list)
      (map (lambda (a) (list (tm->string (tm-ref a 0)) (tm->stree (tm-ref a 1))))
           (list-filter (tm-children c)
                        (lambda (a) (and (tm-func? a 'associate 2)
                                         (tm-atomic? (tm-ref a 0))))))))

(define (part-view-init-changes f com ori)
  ;; the entries of the initial environment which the file @f gets from
  ;; the view (in @com), which it does not have on disk (in @ori), and
  ;; which its open buffer does not have
  (let* ((new (part-init-entries (tmfile-extract com 'initial)))
         (old (part-init-entries (tmfile-extract ori 'initial)))
         (own (part-init-entries (tmfile-extract (buffer-get f) 'initial))))
    (list-filter new (lambda (x) (and (nin? x old) (nin? x own))))))

(define (part-buffer-set-init f l)
  ;; give the open buffer @f the entries @l of the initial environment
  (when (nnull? l)
    (if (nnull? (buffer->views f))
        (with-buffer f
          (for (x l) (init-env-tree (car x) (cadr x))))
        (with doc (tm->stree (buffer-get f))
          (with init (or (tmfile-extract doc 'initial) '(collection))
            (for (x l) (set! init (collection-set init (car x) (cadr x))))
            (buffer-set f (tmfile-assign doc 'initial init))
            (buffer-pretend-modified f))))))

(define (part-own-body-set f body)
  ;; give the open buffer of @f the body @body, unless it has it already
  ;; (it may have copies of the included files instead of inclusions)
  (when (!= (tm->stree (buffer-get-body f)) (tm->stree body))
    (buffer-shared-set-body f body)))

(tm-define (part-own-plan buf)
  ;; for the part view @buf: (file action changed-on-disk?) for its own
  ;; file
  (buffer-shared-flush)
  (with (name f body copies) (part-view-info buf)
    (with action (part-own-action f (part-compress-body body f))
      (list f action (part-changed-on-disk? name f action)))))

(tmfs-save-handler (part name doc)
  (buffer-shared-flush)
  (let* ((m (part-master name))
         (f (part-file name))
         (ori (tree-import f "texmacs"))
         (mas (if (== m f) doc (tree-import m "texmacs")))
         (com (part-compress doc ori mas f m))
         (body (part-own-body (tmfile-extract doc 'body) f m))
         (style (tmfile-extract doc 'style))
         (master-style? (and (== f m) (part-style-changed? name style)))
         (own-ok? #f))
    ;;(display* "com= " (tm->stree com) "\n")
    (set! part-save-written (list))
    (set! part-save-refused (list))
    (set! part-save-pending (list))
    (when (and master-style?
               (part-style-changed-on-disk? name m 'write)
               (nin? (url->unix m) part-save-overwrite))
      ;; the view of the master, whose style changed on disk: the file
      ;; keeps the style on disk (its body is written)
      (part-save-refuse m "style: the file changed on disk")
      (set! master-style? #f))
    (when master-style?
      ;; the view of the master: its file is written with the new style
      (set! com (tmfile-assign com 'style style)))
    (let* ((b (tmfile-extract com 'body))
           (action (part-own-action f b)))
      (cond ((== action 'conflict)
             ;; an open buffer of the file has unsaved changes of its own
             ;; in the body, which the file would lose (and the buffer
             ;; would overwrite the view): the file is not written, as an
             ;; included file (the master does not get the style of the
             ;; view either)
             (part-save-refuse f "it is open with other unsaved changes"))
            ((and (== action 'buffer)
                  (part-changed-on-disk? name f action)
                  (nin? (url->unix f) part-save-overwrite))
             (part-save-refuse f "the file changed on disk"))
            ((== action 'buffer)
             ;; an open buffer with the body of the view and changes of its
             ;; own (style, initial environment) is saved itself; the view
             ;; of the master gives it its new style first (as an open
             ;; master gets the style of a view of an included file)
             (part-own-body-set f b)
             (when master-style?
               (part-set-buffer-style f style))
             (part-buffer-set-init f (part-view-init-changes f com ori))
             (if (buffer-save f)
                 (part-save-refuse f "the file could not be written")
                 (begin
                   (when master-style?
                     (part-remember-style name style))
                   (part-remember name f (or (part-file-body f) b) b)
                   (set! own-ok? #t)
                   (set! part-save-written (cons f part-save-written)))))
            ((tree-export (tm->tree com) f "texmacs")
             (buffer-pretend-modified f)
             (part-save-refuse f "the file could not be written"))
            (else
             (when (in? action '(same sync))
               ;; an open buffer without changes of its own gets the body
               ;; which was written
               (part-own-body-set f b))
             (when master-style?
               ;; which an open buffer of the master gets too
               (when (buffer-exists? m)
                 (part-set-buffer-style m style))
               (part-remember-style name style))
             ;; (the open buffer has what was written)
             (when (!= action 'none)
               (buffer-pretend-saved f))
             (part-remember name f b b)
             (set! own-ok? #t)
             (set! part-save-written (cons f part-save-written)))))
    (part-save-copies name (part-copies body))
    (when (and (!= f m) own-ok?)
      ;; (the master does not get the style of a view whose file could not
      ;; be written: the view stays modified)
      (part-save-master-style name m style))))

(tmfs-wrap-handler (part name)
  (part-file name))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Save command for part views
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (part-save-message)
  ;; the message after the last save of a part view
  (let* ((refused (map (lambda (r)
                         (string-append (part-file-names (list (car r)))
                                        " (" (cdr r) ")"))
                       part-save-refused))
         (msg (cond ((null? refused)
                     (string-append "Saved "
                                    (part-file-names part-save-written)))
                    ((null? part-save-written)
                     (string-append "Not saved: "
                                    (string-recompose refused ", ")))
                    (else
                     (string-append "Saved "
                                    (part-file-names part-save-written)
                                    "; not saved: "
                                    (string-recompose refused ", "))))))
    (if (null? part-save-pending) msg
        (string-append msg "; new style in "
                       (part-file-names part-save-pending)
                       " (open with other unsaved changes, not saved)"))))

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

(define (part-urls-unique l)
  ;; the urls @l, each once
  (if (null? l) l
      (with u (url->unix (car l))
        (cons (car l)
              (part-urls-unique
               (list-filter (cdr l) (lambda (v) (!= (url->unix v) u))))))))

(tm-define (part-save-changed buf)
  ;; the files which saving the part view @buf would write, and which
  ;; changed on disk since they were loaded (each once: for the view of
  ;; the master, its own file and the style of the master are one file)
  (let* ((name (part-open-name buf))
         (f (part-file name))
         (changed (map car (list-filter
                            (cons* (part-own-plan buf)
                                   (part-master-style-plan buf)
                                   (part-save-plan buf))
                            caddr)))
         (disk (part-loaded-disk name f)))
    (part-urls-unique
     (if (and disk (!= (part-file-body f) disk))
         (cons f changed)
         changed))))

(tm-define (save-buffer-main . args)
  (:require (part-view? (if (and (nnull? args) (url? (car args)))
                            (car args) (current-buffer))))
  (let* ((buf (if (and (nnull? args) (url? (car args)))
                  (car args) (current-buffer)))
         (changed (part-save-changed buf)))
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
