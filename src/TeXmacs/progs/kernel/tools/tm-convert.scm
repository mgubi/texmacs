
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : tm-convert.scm
;; DESCRIPTION : Declaration of data formats and converters
;; COPYRIGHT   : (C) 2003  Joris van der Hoeven
;;
;; This software falls under the GNU general public license and comes WITHOUT
;; ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for details.
;; If you don't have this file, write to the Free Software Foundation, Inc.,
;; 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (kernel tools tm-convert)
  (:use (kernel texmacs tm-define) (kernel texmacs tm-modes))
  (:export
    format-cmd format-sub converter-save converter-load ; for format macro
    define-format
    format-skip-spaces format-skip-line format-test?
    format-get-suffixes* format-default-suffix
    format? format-recognizes? format-from-suffix format-determine
    converter-cmd converter-sub ; for converter macro
    converter
    converters-from converters-to
    converter-search convert convert-to-file
    image->postscript texmacs->generic generic->texmacs
    converter-from-menu converter-to-menu
    tmfile? tmfile-extract tmfile-init
    with-aux))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Adding new converters
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define converter-forward (make-ahash-table))
(define converter-backward (make-ahash-table))
(define converter-function (make-ahash-table))
(define converter-options (make-ahash-table))
(define converter-option-for (make-ahash-table))
(define converter-distance (make-ahash-table))
(define converter-path (make-ahash-table))

(define (converter-set-penalty from to penalty)
  (if (not (ahash-ref converter-forward from))
      (ahash-set! converter-forward from (make-ahash-table)))
  (ahash-set! (ahash-ref converter-forward from) to penalty)
  (if (not (ahash-ref converter-backward to))
      (ahash-set! converter-backward to (make-ahash-table)))
  (ahash-set! (ahash-ref converter-backward to) from penalty))

(define (converter-remove from to)
  (converter-set-penalty from to #f)
  (ahash-remove! (ahash-ref converter-forward from) to)
  (ahash-remove! (ahash-ref converter-backward to) from))

(define (converter-change-option from to option val)
  (with key (list from to)
    (if (not (ahash-ref converter-options key))
	(ahash-set! converter-options key '()))
    (ahash-set! converter-options key
		(assoc-set! (ahash-ref converter-options key) option val))))

(define (converter-set-option option val)
  (with key (ahash-ref converter-option-for option)
    (if key (converter-change-option (car key) (cadr key) option val))))

(define (converter-define-option from to option val)
  (with key (list from to)
    (ahash-set! converter-option-for option key)
    (converter-change-option from to option key)
    (define-preferences
      (option val converter-set-option))))

(define (converter-cmd from to cmd)
  (cond ((func? cmd :penalty 1)
	 (converter-set-penalty from to (second cmd)))
        ((func? cmd :require 1)
	 (if (not ((second cmd))) (converter-remove from to)))
        ((func? cmd :option 2)
	 (converter-define-option from to (second cmd) (third cmd)))
        ((func? cmd :function 1)
	 (ahash-set! converter-function (list from to)
		     (lambda (x opts) ((second cmd) x))))
        ((func? cmd :function-with-options 1)
	 (ahash-set! converter-function (list from to) (second cmd)))
        ((func? cmd :shell)
	 (if (not (url-exists-in-path? (second cmd)))
	     (converter-remove from to))
	 (ahash-set! converter-function (list from to)
		     (lambda (what opts)
		       (converter-shell (cdr cmd) what opts))))))

(define (converter-sub cmd)
  (cond ((and (list? cmd) (= (length cmd) 2)
	      (in? (car cmd) '(:function :function-with-options)))
	 (list (car cmd) (list 'unquote (cadr cmd))))
	((and (list? cmd) (= (length cmd) 2)
	      (in? (car cmd) '(:require)))
	 (list (car cmd) (list 'unquote `(lambda () ,(cadr cmd)))))
	(else cmd)))

(define-macro (converter from* to* . options)
  (let* ((from (if (string? from*) from* (symbol->string from*)))
	 (to (if (string? to*) to* (symbol->string to*))))
    (set! converter-distance (make-ahash-table))
    (set! converter-path (make-ahash-table))
    (converter-set-penalty from to 1.0)
    `(for-each (lambda (x) (converter-cmd ,from ,to x))
	       ,(list 'quasiquote (map converter-sub options)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Special converters
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (converter-shell-cmd l from to)
  (if (null? l) ""
      (with x (car l)
	(string-append (cond ((== x 'from) (url-concretize from))
			     ((== x 'to) (url-concretize to))
			     (else x))
		       " "
		       (converter-shell-cmd (cdr l) from to)))))

(define (converter-shell l from opts)
  (let* ((last? (assoc-ref opts 'last?))
	 (dest (assoc-ref opts 'dest))
	 (to (if (and last? dest) dest (url-temp)))
	 (cmd (converter-shell-cmd l from to)))
    (system cmd)
    to))

(define (converter-save s opts)
  (let* ((last? (assoc-ref opts 'last?))
	 (dest (assoc-ref opts 'dest))
	 (to (if (and last? dest) dest (url-temp))))
    (string-save s to)
    to))

(define (converter-load u opts)
  (string-load u))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Finding converters from and to a format
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (converters-sub l h p)
  (cond ((null? l) (map car (ahash-table->list h)))
	((ahash-ref h (car l)) (converters-sub (cdr l) h p))
	(else (let* ((hn (ahash-ref p (car l)))
		     (next (if hn (map car (ahash-table->list hn)) '())))
		(ahash-set! h (car l) #t)
		(converters-sub (append next (cdr l)) h p)))))

(define (converters-from . from)
  (converters-sub from (make-ahash-table) converter-forward))

(define (converters-to . to)
  (converters-sub to (make-ahash-table) converter-backward))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Finding the shortest path
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (converter-insert from to penalty path)
  (if (ahash-ref converter-distance (list from to)) #f
      (begin
	(ahash-set! converter-distance (list from to) penalty)
	(ahash-set! converter-path (list from to) path)
	#t)))

(define (converter-walk from l*)
  ;(display* "convert-walk " from ", " l* "\n")
  (if (not (null? l*))
      (let* ((l (list-sort l* (lambda (x y) (< (cadr x) (cadr y)))))
	     (aux (caar l))
	     (d (cadar l))
	     (path (caddar l)))
	(if (converter-insert from aux d (reverse path))
	    (let* ((hn (ahash-ref converter-forward aux))
		   (next (if hn (ahash-table->list hn) '()))
		   (r (map (lambda (x) (list (car x)
					     (+ d (cdr x))
					     (cons (car x) path)))
			   next)))
	      (converter-walk from (append (cdr l) r)))
	    (converter-walk from (cdr l))))))

(define (converter-search from to)
  (converter-walk from (list (list from 0.0 (list from))))
  (ahash-ref converter-path (list from to)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Actual conversion
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (convert-via what from path options)
  ;(display* "convert-via " what ", " from ", " path ", " options "\n")
  (if (null? path) what
      (with fun (ahash-ref converter-function (list from (car path)))
	(if fun
	    (let* ((last? (null? (cdr path)))
		   (opts1 (acons 'last? last? options))
		   (opts2 (ahash-ref converter-options (list from (car path))))
		   (what* (fun what (append opts1 opts2)))
		   (result (convert-via what* (car path) (cdr path) options)))
	      (if (and (not last?) (string-ends? (car path) "-file"))
		  (system-remove what*))
	      result)
	    #f))))

(define (convert what from to . options)
  ;(display* "convert " what ", " from ", " to ", " options "\n")
  (with path (converter-search from to)
    (if path
	(convert-via what from (cdr path) options)
	#f)))

(define (convert-to-file what from to dest . options)
  (apply convert (cons* what from to (acons 'dest dest options))))

(define (image->postscript name)
  (let* ((suffix (url-suffix name))
	 (fm (string-append (format-from-suffix suffix) "-file"))
	 (s (convert name fm "postscript-document")))
    (if (string? s) s "")))

(define (texmacs->generic doc fm)
  (with r (convert doc "texmacs-tree" fm)
    (if r r "Error: bad format or data")))

(define (generic->texmacs s fm)
  (with r (convert s fm "texmacs-tree")
    (if r r (object->tree '(error "bad format or data")))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Setting up conversion menus
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (format<=? fm1 fm2)
  (string<=? (ahash-ref format-name fm1) (ahash-ref format-name fm2)))

(define (converters-from-special fm suf tm?)
  (let* ((l1 (converters-from fm))
	 (l2 (list-filter l1 (lambda (s) (string-ends? s suf))))
	 (l3 (map (lambda (s) (string-drop-right s (string-length suf))) l2))
	 (l4 (if tm? l3 (list-filter l3 (lambda (s) (not (== s "texmacs")))))))
    (list-sort l4 format<=?)))

(define (converters-to-special fm suf tm?)
  (let* ((l1 (converters-to fm))
	 (l2 (list-filter l1 (lambda (s) (string-ends? s suf))))
	 (l3 (map (lambda (s) (string-drop-right s (string-length suf))) l2))
	 (l4 (if tm? l3 (list-filter l3 (lambda (s) (not (== s "texmacs")))))))
    (list-sort l4 format<=?)))

(define (converter-build-menu item-builder l)
  (define (menu-item fm)
    (item-builder fm (ahash-ref format-name fm)))
  (map menu-item l))

(define (converter-from-menu fm special tm? item-builder)
  (with l (converters-from-special fm special tm?)
    (converter-build-menu item-builder l)))

(define (converter-to-menu fm special tm? item-builder)
  (with l (converters-to-special fm special tm?)
    (converter-build-menu item-builder l)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Other useful subroutines
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (tmfile? doc)
  (and (tmfile-extract doc 'TeXmacs) (tmfile-extract doc 'body)))

(define (tmfile-extract doc what)
  (if (not (func? doc 'document)) #f
      (with val (assoc-ref (cdr doc) what)
	(if val (car val) val))))

(define (default-init var)
  ;; FIXME: should use C++ code
  (cond ((== var "mode") "text")
	((== var "language") "english")
	(else "")))

(define (tmfile-init doc var)
  (with init (tmfile-extract doc 'initial)
    (if (not init) (default-init var)
	(with item (list-find (cdr init) (lambda (x) (== (cadr x) var)))
	  (if item (caddr item) (default-init var))))))

(define-macro (with-aux u . prg)
  `(let* ((u ,u)
	  (t (texmacs-load-tree u "texmacs"))
	  (name (get-name-buffer)))
     (set-aux-buffer "* Aux *" u t)
     (with r (begin ,@prg)
       (switch-to-buffer name)
       r)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Adding new formats
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define format-name (make-ahash-table))
(define format-suffixes (make-ahash-table))
(define format-mime (make-ahash-table))
(define format-recognize (make-ahash-table))
(define format-must-recognize (make-ahash-table))

(define (format-cmd name cmd)
  (cond ((func? cmd :name 1)
	 (ahash-set! format-name name (second cmd)))
	((func? cmd :suffix)
	 (ahash-set! format-suffixes name (cdr cmd))
	 (for-each (lambda (s) (ahash-set! format-mime s name)) (cdr cmd)))
	((func? cmd :recognize 1)
	 (ahash-set! format-recognize name (second cmd)))
	((func? cmd :must-recognize 1)
	 (ahash-set! format-recognize name (second cmd))
	 (ahash-set! format-must-recognize name #t))))

(define (format-sub cmd)
  (if (and (list? cmd) (= (length cmd) 2)
	   (in? (car cmd) '(:recognize :must-recognize)))
      (list (car cmd) (list 'unquote (cadr cmd)))
      cmd))

(define-macro (define-format name* . options)
  (let* ((name (if (string? name*) name* (symbol->string name*)))
	 (name-document (string-append name "-document"))
	 (name-file (string-append name "-file")))
    `(begin
       (converter ,name-document ,name-file
	 (:function-with-options converter-save))
       (converter ,name-file ,name-document
	 (:function-with-options converter-load))
       (for-each (lambda (x) (format-cmd ,name x))
		 ,(list 'quasiquote (map format-sub options))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Useful routines for format recognition
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (format-skip-spaces s pos)
  (cond ((>= pos (string-length s)) pos)
	((char-whitespace? (string-ref s pos))
	 (format-skip-spaces s (+ pos 1)))
	(else pos)))

(define (format-skip-line s pos)
  (cond ((>= pos (string-length s)) pos)
	((in? (string-ref s pos) '(#\newline #\cr)) (+ pos 1))
	(else (format-skip-line s (+ pos 1)))))

(define (format-test? s pos what)
  (with end (+ pos (string-length what))
    (and (>= (string-length s) end)
	 (== (string-downcase (substring s pos end)) what))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Getting suffix information
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (format-get-suffixes-sub fm)
  (with l (ahash-ref format-suffixes fm)
    (if l l '())))

(define (format-image-suffixes)
  (with l (converters-to-special "postscript-file" "-file" #f)
    (apply append (map format-get-suffixes-sub l))))

(define (format-get-suffixes fm)
  (cond ((and (== fm "image") (os-win32?))
         '("ps" "eps" "bmp" "gif" "ico" "tga" "pcx" "wbmp" "wmf" "jpg"
	     "jpeg" "png" "tif" "jbig" "ras" "pnm" "jp2" "jpc" "pgx"
           "cut" "iff" "lbm" "jng" "koa" "mng" "pbm" "pcd" "pcx"
           "pgm" "ppm" "psd" "tga" "tiff" "xbm" "xpm"))
        ((== fm "image") (format-image-suffixes))
        (else (format-get-suffixes-sub fm))))

(define (format-get-suffixes* fm)
  (cons 'tuple (format-get-suffixes fm)))

(define (format-default-suffix fm)
  (with l (ahash-ref format-suffixes fm)
    (cond ((== fm "image") "png")
	  ((or (not l) (null? l)) "")
	  (else (car l)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Automatic determination of the format
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (format? fm)
  (not (not (ahash-ref format-name fm))))

(define (format-recognizes? doc fm)
  (with pred? (ahash-ref format-recognize fm)
    (and pred? (pred? doc))))

(define (format-from-suffix suffix)
  (with fm (ahash-ref format-mime suffix)
    (if fm fm "generic")))

(define (format-determine body suffix)
  (with p (list-find (ahash-table->list format-recognize)
		     (lambda (p) ((cdr p) body)))
    (if p (car p)
	(with fm (ahash-ref format-mime suffix)
	  (cond ((not fm) "verbatim")
		((ahash-ref format-must-recognize fm) "verbatim")
		(else fm))))))
