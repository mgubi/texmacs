
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : tmhtml.scm
;; DESCRIPTION : conversion of TeXmacs trees into Html trees
;; COPYRIGHT   : (C) 2002  Joris van der Hoeven, David Allouche
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (convert html tmhtml)
  (:use (convert tools tmconcat)
	(convert mathml tmmath)
	(convert tools stm)
	(convert tools tmlength)
	(convert tools tmtable)
	(convert tools old-tmtable)
	(convert tools sxml)
	(convert tools sxhtml)
	(convert tools css)
	(convert html htmlout)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Initialization
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define tmhtml-env (make-ahash-table))
(define tmhtml-css? #t)
(define tmhtml-mathjax? #f)
(define tmhtml-mathml? #f)
(define tmhtml-static-math? #f)
(define tmhtml-static-cache (make-ahash-table))
(define tmhtml-in-file? #f)
(define tmhtml-images? #f)
(define tmhtml-image-serial 0)
(define tmhtml-image-cache (make-ahash-table))
(define tmhtml-snippet-ids (make-ahash-table))
(define tmhtml-static-texs (make-ahash-table))
(define tmhtml-image-root-url (unix->url "image"))
(define tmhtml-image-root-string "image")
(define tmhtml-image-loose? #t)
(define tmhtml-site-version #f)
(define tmhtml-web-formats (list "gif" "jpg" "jpeg" "png" "bmp" "svg"))

(tm-define (tmhtml-initialize opts)
  (set! tmhtml-env (make-ahash-table))
  (set! tmhtml-css?
	(== (assoc-ref opts "texmacs->html:css") "on"))
  (set! tmhtml-mathjax?
	(== (assoc-ref opts "texmacs->html:mathjax") "on"))
  (set! tmhtml-mathml?
	(== (assoc-ref opts "texmacs->html:mathml") "on"))
  (set! tmhtml-images?
	(== (assoc-ref opts "texmacs->html:images") "on"))
  (set! tmhtml-static-math?
	(== (assoc-ref opts "texmacs->html:static-math") "on"))
  ;; MathML only when it is the format of the formulas, for the doctype and
  ;; the suffix of the links: an older preference may remain on with another
  (when (or tmhtml-static-math? tmhtml-mathjax?)
    (set! tmhtml-mathml? #f))
  (set! tmhtml-static-cache (make-ahash-table))
  (set! tmhtml-in-file? #f)
  (when tmhtml-static-math? (html-math-defs))
  (set! tmhtml-image-cache (make-ahash-table))
  (set! tmhtml-snippet-ids (make-ahash-table))
  (set! tmhtml-static-texs (make-ahash-table))
  (set! tmhtml-image-serial 0)
  (set! tmhtml-image-loose? (url-none? current-save-target))
  (if tmhtml-image-loose?
      (begin
	(set! tmhtml-image-root-url (unix->url "image"))
	(set! tmhtml-image-root-string "image"))
      (begin
	(set! tmhtml-image-root-url (url-drop-suffix current-save-target))
	(set! tmhtml-image-root-string
	      (url->unix (url-tail tmhtml-image-root-url))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Empty handler and strings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (tmhtml-noop l) '())

(define (cork->html s)
  (utf8->html (cork->utf8 s)))

(define (tmhtml-sub-token s pos)
  (with ss (substring s pos (- (string-length s) 1))
    (if (= (string-length ss) 1) ss
	(tmhtml-math-token (string-append "<" ss ">")))))

(define (tmhtml-math-token s)
  (cond ((= (string-length s) 1)
	 (cond ((== s "*") " ")
	       ((in? s '("+" "-" "=")) (string-append " " s " "))
	       ((char-alphabetic? (string-ref s 0)) `(h:var ,s))
	       (else s)))
	((string-starts? s "<cal-")
	 `(h:font (@ (face "Zapf Chancery")) ,(tmhtml-sub-token s 5)))
	((string-starts? s "<b-cal-")
	 `(h:u (h:font (@ (face "Zapf Chancery")) ,(tmhtml-sub-token s 7))))
	((string-starts? s "<frak-")
	 `(h:u ,(tmhtml-sub-token s 6)))
	((string-starts? s "<bbb-") `(h:u (h:b ,(tmhtml-sub-token s 5))))
	((string-starts? s "<up-") (tmhtml-sub-token s 4))
	((string-starts? s "<b-up-") `(h:b ,(tmhtml-sub-token s 6)))
	((string-starts? s "<b-") `(h:b (h:var ,(tmhtml-sub-token s 3))))
	((string-starts? s "<")
	 (with encoded (cork->utf8 s)
           (if (== s encoded)
             (utf8->html (old-tm->xml-cdata s))
             `(h:var ,(utf8->html encoded)))))
	(else s)))

(define (tmhtml-string s)
  (if (ahash-ref tmhtml-env :math)
      (tmhtml-post-simplify-nodes
       (map tmhtml-math-token (tmconcat-tokenize-math s)))
      (list (cork->html s))))

(define (tmhtml-text s)
  (if (or (ahash-ref tmhtml-env :math) (ahash-ref tmhtml-env :preformatted)
          (ahash-ref tmhtml-env :verbatim))
      (tmhtml-string s)
      (tmhtml-string (make-ligatures s))))

(define cork-endash (char->string (integer->char 21)))
(define cork-ldquo (char->string (integer->char 16)))
(define cork-rdquo (char->string (integer->char 17)))

(define (make-ligatures s)
  ;; Make texmacs ligatures in Cork encoding
  (string-replace
   (string-replace
    (string-replace s "--" cork-endash) "``" cork-ldquo) "''" cork-rdquo))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Entire documents
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (tmhtml-find-title doc)
  (cond ((npair? doc) #f)
	((func? doc 'doc-title 1) (cadr doc))
	((func? doc 'web-title 1) (cadr doc))
	((func? doc 'tmdoc-title 1) (cadr doc))
	((func? doc 'tmdoc-title* 2) (cadr doc))
	((func? doc 'tmdoc-title** 3) (caddr doc))
	((func? doc 'hidden-title 1) (cadr doc))
	(else (with title (tmhtml-find-title (car doc))
		(if title title
		    (tmhtml-find-title (cdr doc)))))))

(define (tmhtml-css-header)
  ;; TODO: return only used CSS properties
  (let ((html
	 (string-append
	  "body { text-align: justify } "
	  "h5 { display: inline; padding-right: 1em } "
	  "h6 { display: inline; padding-right: 1em } "
	  "table { border-collapse: collapse } "
	  "td { padding: 0.2em; vertical-align: baseline } "
          "dt { float: left; min-width: 1.75em; text-align: right; "
          "padding-right: 0.75em; font-weight: bold; } "
          "dd { margin-left: 2.75em; padding-bottom: 0.25em; } "
          "dd p { padding-top: 0em; } "
	  ".subsup { display: inline; vertical-align: -0.2em } "
	  ".subsup td { padding: 0px; text-align: left} "
	  ".fraction { display: inline; vertical-align: -0.8em } "
	  ".fraction td { padding: 0px; text-align: center } "
	  ".wide { position: relative; margin-left: -0.4em } "
	  ".accent { position: relative; margin-left: -0.4em; top: -0.1em } "
	  ".title-block { width: 100%; text-align: center } "
	  ".title-block p { margin: 0px } "
	  ".compact-block p { margin-top: 0px; margin-bottom: 0px } "
	  ".left-tab { text-align: left } "
	  ".center-tab { text-align: center } "
          ".balloon-anchor { border-bottom: 1px dotted #000000; outline: none;"
          " cursor: help; position: relative; } "
          ".balloon-anchor [hidden] { margin-left: -999em; position: absolute;"
	  " display: none; } "
          ".balloon-anchor: hover [hidden] { position: absolute; left: 1em;"
	  " top: 2em; z-index: 99; margin-left: 0;"
	  " width: 500px; display: inline-block; } "
          ".balloon-body { } "
	  ".ornament { border-width: 1px; border-style: solid;"
	  " border-color: black; display: inline-block; padding: 0.2em; } "
	  ".right-tab { float: right; position: relative; top: -1em; } "
	  ".no-breaks { white-space: nowrap; } "
	  ".underline { text-decoration: underline; } "
	  ".overline { text-decoration: overline; } "
	  ".strike-through { text-decoration: line-through; } "
	  "del { text-decoration: line-through wavy red; } "
	  ".fill-out { text-decoration: underline dotted; } "))
	(mathml "math { font-family: cmr, times, verdana } "))
    (if tmhtml-mathml? (string-append html mathml) html)))

(define (with-extract-sub w var post)
  (cond ((and (pair? w) (== (car w) 'with)
	      (pair? (cdr w)) (== (cadr w) var)
	      (pair? (cddr w)))
	 (post (caddr w)))
	((and (pair? w) (== (car w) 'with)
	      (pair? (cdr w)) (pair? (cddr w)))
	 (with-extract-sub `(with ,@(cdddr w)) var post))
	(else #f)))

(define (with-extract w var)
  (with-extract-sub w var tmhtml-force-string))

(define (with-extract* w var)
  (with post (lambda (x) (if (tm-func? x 'quote 1) (cadr x) x))
    (with-extract-sub w var post)))

(define (tmhtml-file l)
  (set! tmhtml-in-file? #t)
  ;; This handler is special:
  ;; Since !file is a special node used only at the top of trees
  ;; it produces a single node, and not a nodeset like other handlers.
  (let* ((doc (car l))
	 (styles (cdadr l))
	 (lang (caddr l))
	 (tmpath (cadddr l))
	 (title (tmhtml-find-title doc))
	 (css `(h:style (@ (type "text/css")) ,(tmhtml-css-header)))
	 (xhead '())
	 (body (tmhtml doc)))
    (set! tmhtml-site-version
          (with-extract doc "html-site-version"))
    (set! title
	  (cond ((with-extract doc "html-title")
                 (with-extract doc "html-title"))
		((and (not title) (with-extract doc "html-doc-title"))
                 (with-extract doc "html-doc-title"))
		((not title) "No title")
		((or (in? "tmdoc" styles)
                     (in? "tmweb" styles) (in? "tmweb2" styles))
		 `(concat ,(utf8->cork (tmhtml-force-string title))
                          " (FSF GNU project)"))
		(else (utf8->cork (tmhtml-force-string title)))))
    (set! css
	  (cond ((with-extract doc "html-css")
		 `(h:link (@ (rel "stylesheet")
			     (href ,(with-extract doc "html-css"))
			     (type "text/css"))))
		(else css)))
    (if (with-extract doc "html-head-javascript-src")
	(let* ((src (with-extract doc "html-head-javascript-src"))
	       (script `(h:script (@ (language "javascript")
                                     (src ,src)))))
	  (set! xhead (append xhead (list script)))))
    (if (with-extract doc "html-head-javascript")
	(let* ((code (with-extract doc "html-head-javascript"))
	       (script `(h:script (@ (language "javascript")) ,code)))
	  (set! xhead (append xhead (list script)))))
    (if (with-extract doc "html-head-favicon")
	(let* ((code (with-extract doc "html-head-favicon"))
	       (icon `(h:link (@ (rel "icon") (href ,code)))))
	  (set! xhead (append xhead (list icon)))))
    (if (and (not (func? css 'h:link))
             (string-ends? (get-preference "texmacs->html:css-stylesheet")
                           ".css"))
        (with src (get-preference "texmacs->html:css-stylesheet")
          (with link-css `(h:link (@ (rel "stylesheet")
                                     (href ,src)
                                     (type "text/css")))
            (set! xhead (append xhead (list link-css))))))
    (if (tm-func? (with-extract* doc "html-extra-css") 'tuple)
        (for (src (cdr (with-extract* doc "html-extra-css")))
          (with link-css `(h:link (@ (rel "stylesheet")
                                     (href ,src)
                                     (type "text/css")))
            (set! xhead (append xhead (list link-css))))))
    (if (tm-func? (with-extract* doc "html-extra-javascript-src") 'tuple)
        (for (src (cdr (with-extract* doc "html-extra-javascript-src")))
          (with script `(h:script (@ (language "javascript")
                                     (src ,src)
                                     (defer "<implicit>")))
            (set! xhead (append xhead (list script))))))
    (if (tm-func? (with-extract* doc "html-extra-javascript") 'tuple)
        (for (code (cdr (with-extract* doc "html-extra-javascript")))
          (with script `(h:script (@ (language "javascript")
                                     (defer "<implicit>")) ,code)
            (set! xhead (append xhead (list script))))))
    (if tmhtml-static-math?
	(with style `(h:style (@ (type "text/css"))
		      ,(string-append ".tm-src { " tmhtml-static-hidden " } "
                                      ".tm-math { " tmhtml-static-math " } "
                                      ".tm-tex { " tmhtml-static-text " }"))
	  (set! xhead (append xhead (list style)))))
    (if (and tmhtml-mathjax? (not tmhtml-static-math?))
	(let* ((site "https://cdn.jsdelivr.net/")
               (loc "npm/mathjax@3/es5/tex-mml-chtml.js")
               (src (string-append site loc))
	       (script `(h:script (@ (language "javascript") (src ,src)))))
	  (set! xhead (append xhead (list script)))))
    (if (or (in? "tmdoc" styles)
            (in? "tmweb" styles) (in? "tmweb2" styles)
            (in? "mmxdoc" styles) (in? "magix-web" styles)
            (in? "max-web" styles) (in? "node-web" styles))
	(set! body (tmhtml-tmdoc-post body)))
    (if tmhtml-css?
        (set! body (tmhtml-css-post body)))
    `(h:html
      (h:head
       (h:meta (@ (charset "utf-8")))
       (h:title ,@(tmhtml title))
       (h:meta (@ (name "generator")
		  (content ,(string-append "TeXmacs " (texmacs-version)))))
       ,css
       ,@xhead)
      (h:body ,@(tmhtml-static-defs) ,@body))))

(define (tmhtml-finalize-document top)
  ;; @top must be a node produced by tmhtml-file
  "Prepare a XML document for serialization"
  (define xmlns-attrs
    '((xmlns "http://www.w3.org/1999/xhtml")
      (xmlns:m "http://www.w3.org/1998/Math/MathML")
      (xmlns:x "https://www.texmacs.org/2002/extensions")))
  (define doctype-list
    (let ((html       "-//W3C//DTD XHTML 1.1//EN")
          (mathml     "-//W3C//DTD XHTML 1.1 plus MathML 2.0//EN")
	  (html-drd   "http://www.w3.org/TR/xhtml11/DTD/xhtml11.dtd")
	  (mathml-drd (string-append
                        "http://www.w3.org/2002/04/xhtml-math-svg/"
                        "xhtml-math-svg.dtd")))
      (if tmhtml-mathml? (list mathml mathml-drd) (list html html-drd))))
  `(*TOP* (*PI* xml "version=\"1.0\" encoding=\"UTF-8\"")
	  (*DOCTYPE* html PUBLIC ,@doctype-list)
	  ,((cut sxml-set-attrs <> xmlns-attrs)
	    (sxml-strip-ns-prefix "h" (sxml-strip-ns-prefix "m" top)))))

(define (tmhtml-finalize-selection l)
  ;; @l is a nodeset produced by any handler _but_ tmhtml-file
  "Prepare a HTML node-set for serialization."
  `(*TOP* ,@(map (cut sxml-strip-ns-prefix "h" <>) (map (cut sxml-strip-ns-prefix "m" <>) l))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Block structures
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (tmhtml-document-elem x)
  ;; NOTE: this should not really be necessary, but it improves
  ;; the layout of verbatim environments with a missing block structure
  (if (and (list-2? x)
	   (or (== (car x) 'verbatim) (== (car x) 'code))
	   (not (func? (cadr x) 'document)))
      (tmhtml (list (car x) (list 'document (cadr x))))
      (tmhtml x)))

(define (tmhtml-compute-max-vspace l after?)
  (and (nnull? l)
    (with s1 (tmhtml-compute-vspace (car l) after?)
      (with s2 (tmhtml-compute-max-vspace (cdr l) after?)
	(cond ((not s1) s2)
	      ((not s2) s1)
	      (else
		(with l1 (string->tmlength s1)
		  (with l2 (string->tmlength s2)
		    (if (== (tmlength-unit l1) (tmlength-unit l2))
			(if (>= (tmlength-value l1) (tmlength-value l2)) s1 s2)
			s1 ;; FIXME: do something more subtle here
			)))))))))

(define (tmhtml-compute-vspace x after?)
  (cond ((and (not after?) (func? x 'vspace* 1)) (tmhtml-force-string (cadr x)))
	((and after? (func? x 'vspace 1)) (tmhtml-force-string (cadr x)))
	;;((and (not after?) (func? x 'document)) (tmhtml-compute-vspace (cadr x) #f))
	;;((and after? (func? x 'document)) (tmhtml-compute-vspace (cAr x) #t))
	((func? x 'concat) (tmhtml-compute-max-vspace (cdr x) after?))
	((func? x 'with) (tmhtml-compute-vspace (cAr x) after?))
	;;((func? x 'surround) (tmhtml-compute-max-vspace (cDdr x) after?))
	;;((func? x 'surround) (tmhtml-compute-max-vspace (cdr x) after?))
	(else #f)))

(define (tmhtml-div-merged-attrs x1 x2)
  (let* ((c1 (sxml-attr x1 'class))
         (c2 (sxml-attr x2 'class))
         (c  (if (and c1 c2) (string-append c1 " " c2) (or c1 c2)))
         (s1 (sxml-attr x1 'style))
         (s2 (sxml-attr x2 'style))
         (s  (if (and s1 s2) (css-merge-styles s1 s2) (or s1 s2)))
         (o? (lambda (x) (or (npair? x) (nin? (car x) '(style class)))))
         (other (list-filter (append (sxml-attr-list x1)
                                     (sxml-attr-list x2)) o?)))
    (append (if c (list (list 'class c)) (list))
            (if s (list (list 'style s)) (list))
            other)))

(define (tmhtml-simplify-div x)
  (or (and (func? x 'h:div)
           (with l1 (or (sxml-content x) (list))
             (and (== (length l1) 1)
                  (func? (car l1) 'h:div)
                  (let* ((l2 (or (sxml-content (car l1)) (list)))
                         (a  (tmhtml-div-merged-attrs x (car l1)))
                         (c1 (sxml-attr x 'class))
                         (c2 (sxml-attr (car l1) 'class)))
                    (and (not (and c1 c2))
                         `(h:div (@ ,@a) ,@l2))))))
      x))

(define (xhtml-block? x)
  (tm-in? x '(h:p h:div h:center h:pre h:h1 h:h2 h:h3 h:h4 h:h5 h:h6
              h:ol h:ul h:dl h:table)))

(define (mixed-block l)
  (cond ((null? l) l)
        ((xhtml-block? (car l)) (cons (car l) (mixed-block (cdr l))))
        (else
          (let* ((i (or (list-find-index l xhtml-block?) (length l)))
                 (s (sublist l 0 i))
                 (r (mixed-block (sublist l i (length l)))))
            (cons `(h:div (@ (style "display: inline")) ,@s) r)))))

(define (as-block x)
  ;;(display* "--- " x "\n")
  (cond ((func? x 'h:div) (tmhtml-simplify-div x))
        ((and (func? x 'h:p)
              (not (forall? (negate xhtml-block?) (sxml-content x)))
              (not (forall? xhtml-block? (sxml-content x))))
         (let* ((a (sxml-attr-list x))
                (m (mixed-block (sxml-content x))))
           (if (null? a) `(h:p ,@m) `(h:p (@ ,@a) ,@m))))
        (else x)))

(define (force-block? x)
  (or (and (tm-in? x '(h:p h:div h:center h:pre h:h1 h:h2 h:h3 h:h4
                       h:ol h:ul h:dl h:table))
           (not (and-with style (sxml-attr x 'style)
                  (string-contains? style "display: inline")))
           (not (and-with class (sxml-attr x 'class)
                  (string-contains? class "-license"))))
      (and (tm-in? x '(h:i h:b h:u h:var h:font h:class))
           ;; FIXME: we should really restructure this kind of Html output
           ;; such that these tags never contain block content
           (exists? force-block? (sxml-content x)))))

(define (as-blocks l)
  (cond ((null? l) l)
        ((and (func? (car l) 'h:p) (list-find (cdar l) force-block?))
         (let* ((sl (cdar l))
                (i  (list-find-index sl force-block?))
                (l1 (sublist sl 0 i))
                (x2 (list-ref sl i))
                (l2 (sublist sl (+ i 1) (length sl)))
                (h  (if (null? l1) (list x2) (list `(h:p ,@l1) x2)))
                (t  (if (null? l2) (cdr l) (cons `(h:p ,@l2) (cdr l)))))
           (append (map as-block h) (as-blocks t))))
        (else (cons (as-block (car l))
                    (as-blocks (cdr l))))))

(define (add-style-attr x s1 s2)
  (with s (or (and s1 s2 (string-append s1 "; " s2)) s1 s2)
    (cond ((not s) x)
          ((and (xhtml-block? x) (null? (sxml-attr-list x)))
           `(,(car x) (@ (style ,s)) ,@(cdr x)))
          ((and (xhtml-block? x) (not (sxml-attr x 'style)))
           `(,(car x) (@ (style ,s) ,@(sxml-attr-list x)) ,@(sxml-content x)))
          ((func? x 'h:p)
           `(h:p (@ (style ,s)) (h:div ,(cdr x))))
          (else `(h:div (@ (style ,s)) ,x)))))

(define (tmhtml-p x)
  (let* ((body (tmhtml-document-elem x))
         (bl (as-blocks (list `(h:p ,@body))))
	 (l1 (tmhtml-compute-vspace x #f))
	 (l2 (tmhtml-compute-vspace x #t))
	 (h1 (and l1 (tmlength->htmllength l1 #t)))
	 (h2 (and l2 (tmlength->htmllength l2 #t)))
	 (s1 (and h1 (string-append "margin-top: " h1)))
	 (s2 (and h2 (string-append "margin-bottom: " h2))))
    ;;(display* "  <<<<< " body "\n")
    ;;(display* "  >>>>> " bl "\n")
    (cond ((null? bl) bl)
          ((null? (cdr bl)) (list (add-style-attr (car bl) s1 s2)))
          (else (append (list (add-style-attr (car bl) s1 #f))
                        (cdr (cDr bl))
                        (list (add-style-attr (cAr bl) #f s2)))))))

(define (tmhtml-document l)
  (cond ((null? l) '())
	((ahash-ref tmhtml-env :preformatted)
	 (tmhtml-post-simplify-nodes
	  (list-concatenate
	   ((cut list-intersperse <> '("\n"))
	    (map tmhtml l)))))
	(else
          (with pars (map tmhtml-p l)
            ;;(display* "===== Got  " l "\n")
            ;;(display* "----- Post " (apply append pars) "\n\n")
            (apply append pars)))))

(define (tmhtml-paragraph l)
  (let rec ((l l))
    (if (null? l) '()
	(let ((h (tmhtml (car l)))
	      (r (rec (cdr l))))
	  (cond ((null? h) r)		; correct when r is null too
		((null? r) h)
		(else `(,@h (h:br) ,@r)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Surrounding block structures
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define document-done '())
(define concat-done '())

(define (serialize-print x)
  (set! concat-done (cons x concat-done)))

(define (serialize-paragraph x)
  (serialize-concat x)
  (with l (tmconcat-simplify (reverse concat-done))
    (set! document-done (cons (cons 'concat l) document-done))
    (set! concat-done '())))

(define (serialize-concat x)
  (cond ((in? x '("" (document) (concat))) (noop))
	((func? x 'document)
	 (for-each serialize-paragraph (cDdr x))
	 (serialize-concat (cAr x)))
	((func? x 'concat)
	 (for-each serialize-concat (cdr x)))
	((func? x 'surround 3)
	 (serialize-concat (cadr x))
	 (serialize-concat (cadddr x))
	 (serialize-concat (caddr x)))
        ((func? x 'with 1) (serialize-concat (cadr x)))
        ((and (func? x 'with)
              (in? cadr (list "locus-color" "visited-color")))
         (serialize-concat `(with ,@(cdddr x))))
	((func? x 'with)
	 (let* ((r (simplify-document (cAr x)))
		(w (lambda (y) `(with ,@(cDdr x) ,y))))
	   (if (not (func? r 'document))
	       (serialize-print (w r))
	       (let* ((head (cadr r))
		      (body `(document ,@(cDr (cddr r))))
		      (tail (cAr r)))
		 (serialize-paragraph (w head))
                 (for (x (cDr (cddr r)))
                   (serialize-paragraph (w x)))
                 ;;(when (nnull? (cdr body))
                 ;;  (set! document-done (cons (w body) document-done)))
		 (serialize-concat (w tail))))))
        ((func? x 'locus) (serialize-concat (cAr x)))
	(else (serialize-print x))))

(define (simplify-document x)
  (with-global document-done '()
    (with-global concat-done '()
      (serialize-paragraph x)
      (if (list-1? document-done)
	  (car document-done)
	  (cons 'document (reverse document-done))))))

(define (block-document? x)
  (cond ((func? x 'document) #t)
	((func? x 'concat) (list-any block-document? (cdr x)))
	((func? x 'surround 3) (block-document? (cAr x)))
	((func? x 'with) (block-document? (cAr x)))
	(else #f)))

(define (blockify x)
  (cond ((func? x 'document) x)
	((or (func? x 'surround 3) (func? x 'with))
	 (rcons (cDr x) (blockify (cAr x))))
	(else `(document ,x))))

(define (tmhtml-surround l)
  (let* ((r1 `(surround ,@l))
	 (r2 (simplify-document r1))
	 (f? (and (block-document? r1) (not (func? r2 'document))))
	 (r3 (if f? (blockify r2) r2)))
    (tmhtml r3)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Horizontal concatenations
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (tmhtml-glue-scripts l)
  (cond ((or (null? l) (null? (cdr l))) l)
	((and (func? (car l) 'rsub 1) (func? (cadr l) 'rsup 1))
	 (cons `(rsubsup ,(cadar l) ,(cadadr l))
	       (tmhtml-glue-scripts (cddr l))))
	(else (cons (car l) (tmhtml-glue-scripts (cdr l))))))

(define (heading? l)
  (cond ((null? l) #f)
	((sxhtml-label? (car l)) (heading? (cdr l)))
	((sxhtml-heading? (car l)) #t)
	(else #f)))

(define (tmhtml-post-heading l)
  ;; Post-process the converted result of a concat containing a section title.
  ;;
  ;; Any label preceding the section is moved after it.
  ;;
  ;; The first label after the section is changed to an 'id' attribute in the
  ;; heading element, if it has not already an 'id' attribute.
  ;;
  ;; NOTE: assumes the heading is the first node (not counting labels)
  (receive (labels-before rest) (list-span l sxhtml-label?)
    (receive (heading rest) (car+cdr rest)
      (if (sxml-attr heading 'id)
	  `(,heading ,@labels-before ,@rest)
	  (receive (labels-after rest) (list-partition rest sxhtml-label?)
	    (let ((labels (append labels-before labels-after)))
	      (if (null? labels) l
		  (cons (sxml-prepend (sxhtml-glue-label heading (car labels))
				      (cdr labels))
			rest))))))))

(define (tmhtml-post-table l)
  ;; Post process the converted result of a concat containing a table.
  ;;
  ;; If a label is adjacent to the table, use it to set the table id. If there
  ;; are several labels adjacent to the table, leave all but one label
  ;; untouched. There is no guarantee on which label is glued.
  (define (glue-label-to-table x knil)
    (cond ((null? knil) (list x))
	  ((and (sxhtml-label? x)
		(sxhtml-table? (car knil))
		(not (sxml-attr (car knil) 'id)))
	   (cons (sxhtml-glue-label (car knil) x)
		 (cdr knil)))
	  ((and (sxhtml-table? x)
		(not (sxml-attr x 'id))
		(sxhtml-label? (car knil)))
	   (cons (sxhtml-glue-label x (car knil))
		 (cdr knil)))
	  (else (cons x knil))))
  (list-fold-right glue-label-to-table '() l))

(define (tmhtml-concat l)
  (set! l (tmhtml-glue-scripts l))
  ;;(display* "l << " l "\n")
  (set! l (tmconcat-structure-tabs l))
  ;;(display* "l >> " l "\n")
  (tmhtml-static-bind-punctuation
   (tmhtml-post-simplify-nodes
    (let ((l (tmhtml-list l)))
      (cond ((null? l) '())
            ((string? (car l)) l)
            ((heading? l) (tmhtml-post-heading l))
            ((list-any sxhtml-table? l) (tmhtml-post-table l))
            ((and (null? (cdr l)) (pair? (car l))
                  (== (caar l) 'h:div) (== (cadar l) '(@ (class "left-tab"))))
             (cddar l))
            (else l))))))

(define (tmhtml-align-left l)
  (with r (tmhtml-concat l)
    (if (in? r '(() (""))) '()
	`((h:div (@ (class "left-tab")) ,@r)))))

(define (tmhtml-align-middle l)
  (with r (tmhtml-concat l)
    (if (in? r '(() (""))) '()
	`((h:div (@ (class "center-tab")) ,@r)))))

(define (tmhtml-align-right l)
  (with r (tmhtml-concat l)
    (if (in? r '(() (""))) '()
	`((h:div (@ (class "right-tab")) ,@r)))))

(define (tmhtml-post-simplify-nodes l)
  ;; Catenate adjacent string nodes and remove empty string nodes
  (let rec ((l l))
    (cond ((null? l) '())
	  ((and (string? (car l)) (string-null? (car l)))
	   (rec (cdr l)))
	  ((null? (cdr l)) l)
	  ((and (string? (car l)) (string? (cadr l)))
	   (rec (cons (string-append (car l) (cadr l)) (cddr l))))
	  (else (cons (car l) (rec (cdr l)))))))

(define (tmhtml-post-simplify-element e)
  ;; Simplify the nodes of the element content
  (list (append (sxml-element-head e)
		(tmhtml-post-simplify-nodes (sxml-content e)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Formatting text
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (tmhtml-hidden l)
  ;; FIXME: distinguish inline and block?
  (with r (tmhtml (car l))
    `((div (@ (class "toggle") (style "display: none")) ,@r))))

(define (tmhtml-shown l)
  ;; FIXME: distinguish inline and block?
  (with r (tmhtml (car l))
    `((div (@ (class "toggle") (style "display: block")) ,@r))))

(define (tmhtml-hspace l)
  (with len (tmlength->htmllength (if (list-1? l) (car l) (cadr l)) #t)
    (if (not len) '()
	`((h:span (@ (style ,(string-append "margin-left: " len))))))))

(define (tmhtml-vspace l)
  '())

(define (tmhtml-move l)
  (tmhtml (car l)))

(define (tmhtml-resize l)
  (tmhtml (car l)))

(define (tmhtml-float l)
  (let* ((type (tmhtml-force-string (car l)))
         (cl (if (== type "") "unknown-float" type))
         (r (tmhtml (cAr l))))
    `((div (@ (class ,cl)) ,@r))))

(define (tmhtml-repeat l)
  (tmhtml (car l)))

(define (tmhtml-underline l)
   `((h:span (@ (class "underline")) ,@(tmhtml (cAr l)) )))

(define (tmhtml-overline l)
   `((h:span (@ (class "overline")) ,@(tmhtml (cAr l)) )))

(define (tmhtml-strike-through l)
   `((h:span (@ (class "strike-through")) ,@(tmhtml (cAr l)) )))
   
(define (tmhtml-deleted l)
   `((h:del ,@(tmhtml (cAr l)) )))

(define (tmhtml-marked l)
   `((h:mark ,@(tmhtml (cAr l)) )))
   
(define (tmhtml-fill-out l)
   `((h:span (@ (class "fill-out")) ,@(tmhtml (cAr l)) )))

(define (tmhtml-datoms l)
  (tmhtml (cAr l)))

(define (tmhtml-new-line l)
  '((h:br)))

(define (tmhtml-next-line l)
  '((h:br)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Mathematics
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (tmhtml-id l)
  (tmhtml (car l)))

(define (tmhtml-big l)
  (cond ((in? (car l) '("sum" "prod" "int" "oint" "amalg"))
	 (tmhtml (string-append "<" (car l) ">")))
	((in? (car l) '("<cap>" "<cup>" "<vee>" "<wedge>"))
	 (with s (substring (car l) 1 (- (string-length (car l)) 1))
	   (tmhtml (string-append "<big" s ">"))))
	((== (car l) ".") '())
	(else (tmhtml (car l)))))

(define (tmhtml-below l)
  `("below (" ,@(tmhtml (car l)) ", " ,@(tmhtml (cadr l)) ")"))

(define (tmhtml-above l)
  `("above (" ,@(tmhtml (car l)) ", " ,@(tmhtml (cadr l)) ")"))

(define (tmhtml-sub l)
  `((h:sub ,@(tmhtml (car l)))))

(define (tmhtml-sup l)
  `((h:sup ,@(tmhtml (car l)))))

(define (tmhtml-subsup l)
  (let* ((sub (tmhtml (car l)))
	 (sup (tmhtml (cadr l)))
	 (r1 `(h:tr (h:td ,@sup)))
	 (r2 `(h:tr (h:td ,@sub))))
    `((h:sub (h:table (@ (class "subsup")) ,r1 ,r2)))))

;;(define (tmhtml-frac l)
;;  (let* ((num (tmhtml (car l)))
;;	 (den (tmhtml (cadr l))))
;;    `("frac (" ,@num ", " ,@den ")")))

(define (tmhtml-frac l)
  (let* ((num (tmhtml (car l)))
	 (den (tmhtml (cadr l)))
	 (n `(h:tr (h:td (@ (style "border-bottom: solid 1px")) ,@num)))
	 (d `(h:tr (h:td ,@den))))
    `((h:table (@ (class "fraction")) ,n ,d))))

(define (tmhtml-sqrt l)
  (if (= (length l) 1)
      `("sqrt (" ,@(tmhtml (car l)) ")")
      `("sqrt" (h:sub ,@(tmhtml (cadr l)))
	" (" ,@(tmhtml (car l)) ")")))

(define (tmhtml-short? l)
  (and (list-1? l)
       (or (string? (car l))
	   (and (func? (car l) 'h:i) (tmhtml-short? (cdar l)))
	   (and (func? (car l) 'h:b) (tmhtml-short? (cdar l)))
	   (and (func? (car l) 'h:u) (tmhtml-short? (cdar l)))
	   (and (func? (car l) 'h:var) (tmhtml-short? (cdar l)))
	   (and (func? (car l) 'h:font) (tmhtml-short? (cdar l))))))

(define (tmhtml-wide l)
  (let* ((body (tmhtml (car l)))
	 (acc (tmhtml (cadr l)))
	 (class (if (in? acc '(("^") ("~"))) "accent" "wide")))
    (if (tmhtml-short? body)
	`(,@body (h:sup (@ (class ,class)) ,@acc))
	`("(" ,@body ")" (h:sup ,@acc)))))

(define (tmhtml-neg l)
  `("not(" ,@(tmhtml (car l)) ")"))

(define (tmhtml-tree l)
  (tmhtml-png `(tree ,@l)))

(define (tmhtml-syntax l)
  (tmhtml (car l)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Shape conversions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (tmshape->htmllength x)
  (if (== (tmhtml-force-string x) "rounded") "15px" "0px"))

 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Color conversions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; A value which goes into a style attribute cannot end its declaration or
;; call a script: only the characters of colors and lengths are kept, and
;; url(, expression( and the like are refused.
(tm-define (tmhtml-css-safe s fallback)
  (let ((low (string-downcase s)))
    (if (and (null? (filter (lambda (c) (not (or (char-alphabetic? c)
                                                 (char-numeric? c)
                                                 (in? c '(#\space #\# #\( #\)
                                                          #\, #\. #\% #\+
                                                          #\- #\/ #\_)))))
                            (string->list s)))
             (not (string-occurs? "url(" low))
             (not (string-occurs? "expression" low))
             (not (string-occurs? "javascript" low)))
        s
        fallback)))

;; The address of a link or of an image: javascript:, vbscript: and data:
;; (but data:image/png, jpeg, gif and webp for an image) are not followed.
;; The scheme is read as a browser reads it, without spaces and controls,
;; with the entities for the colon and the other ones removed.
;; only ASCII matters for the scheme of an address (integer->char fails above
;; 255 on Guile 1.8): any other code point is a placeholder, removed later
(define (tmhtml-entity-char n)
  (if (and (> n 32) (< n 127)) (integer->char n) #\space))

(define (tmhtml-url-normalize u)
  ;; &#106; &#x6A; (leading zeros, the ; is optional) and the names colon, Tab
  ;; and NewLine are decoded as a browser does before the scheme is read
  (let ((n (string-length u)))
    (define (digits i hex?)
      (let loop ((j i))
        (if (and (< j n)
                 (let ((c (char-downcase (string-ref u j))))
                   (or (char-numeric? c)
                       (and hex? (string-index "abcdef" c)))))
            (loop (+ j 1))
            j)))
    (let loop ((i 0) (acc '()))
      (if (>= i n)
          (string-downcase (apply string-append (reverse acc)))
          (let ((c (string-ref u i)))
            (cond ((and (== c #\&) (< (+ i 1) n) (== (string-ref u (+ i 1)) #\#))
                   (let* ((hex? (and (< (+ i 2) n)
                                     (in? (string-ref u (+ i 2)) '(#\x #\X))))
                          (start (if hex? (+ i 3) (+ i 2)))
                          (end (digits start hex?)))
                     (if (= end start)
                         (loop (+ i 1) (cons "&" acc))
                         (let* ((code (string->number (substring u start end)
                                                      (if hex? 16 10)))
                                (next (if (and (< end n) (== (string-ref u end) #\;))
                                          (+ end 1) end)))
                           (loop next (cons (if (or (<= code 32) (== code 127)) ""
                                                 (string (tmhtml-entity-char code)))
                                            acc))))))
                  ((and (== c #\&) (htmlout-entity-end u i))
                   (let* ((e (htmlout-entity-end u i))
                          (name (string-downcase (substring u (+ i 1) e))))
                     (loop (+ e 1) (cons (if (== name "colon") ":" "") acc))))
                  ((or (<= (char->integer c) 32) (== (char->integer c) 127))
                   (loop (+ i 1) acc))
                  (else (loop (+ i 1) (cons (string c) acc)))))))))

(tm-define (tmhtml-safe-url u . image?)
  (let ((n (tmhtml-url-normalize u)))
    (if (or (string-starts? n "javascript:") (string-starts? n "vbscript:")
            (and (string-starts? n "data:")
                 (not (and (list>0? image?) (car image?)
                           (or (string-starts? n "data:image/png")
                               (string-starts? n "data:image/jpeg")
                               (string-starts? n "data:image/gif")
                               (string-starts? n "data:image/webp"))))))
        "#"
        u)))

(tm-define (tmcolor->htmlcolor x)
  (with s (tmhtml-force-string x)
    (cond ((== s "light grey") "#d0d0d0")
	  ((== s "pastel grey") "#dfdfdf")
	  ((== s "dark grey") "#707070")
	  ((== s "dark red") "#800000")
	  ((== s "dark green") "#008000")
	  ((== s "dark blue") "#000080")
	  ((== s "dark yellow") "#808000")
	  ((== s "dark magenta") "#800080")
	  ((== s "dark cyan") "#008080")
	  ((== s "dark orange") "#804000")
	  ((== s "dark brown") "#401000")
	  ((== s "broken white") "#ffffdf")
	  ((== s "pastel red") "#ffdfdf")
	  ((== s "pastel green") "#dfffdf")
	  ((== s "pastel blue") "#dfdfff")
	  ((== s "pastel yellow") "#ffffdf")
	  ((== s "pastel magenta") "#ffdfff")
	  ((== s "pastel cyan") "#dfffff")
	  ((== s "pastel orange") "#ffdfbf")
	  ((== s "pastel brown") "#dfbfbf")
	  (else (tmhtml-css-safe s "inherit")))))

(tm-define (tmbgcolor->htmlbgcolor x)
  (with s (tmhtml-force-string x)
    (cond ((== s "light grey") "#80808060")
	  ((== s "pastel grey") "#80808040")
	  ((== s "broken white") "#ffff0020")
	  ((== s "pastel red") "#ff000020")
	  ((== s "pastel green") "#00ff0020")
	  ((== s "pastel blue") "#0000ff20")
	  ((== s "pastel yellow") "#ffff0020")
	  ((== s "pastel magenta") "#ff00ff20")
	  ((== s "pastel cyan") "#00ffff20")
	  ((== s "pastel orange") "#ff800040")
	  ((== s "pastel brown") "#80000040")
	  ((== s "#c0c0ff") "#8080ff80")
	  ((== s "#c0ffc0") "#80ff8080")
	  ((== s "#ffc0c0") "#ff808080")
	  ((== s "#aaf") "#0000ff55")
	  ((== s "#afa") "#00ff0055")
	  ((== s "#faa") "#ff000055")
	  ((== s "#ffa") "#ffff0055")
	  ((== s "#faf") "#ff00ff55")
	  ((== s "#aff") "#00ffff55")
	  ((== s "#fa6") "#ff800099")
          ((== s "") "#00000000")
	  (else (tmcolor->htmlcolor x)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Length conversions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-table tmhtml-length-table
  ("mm" . 0.1)
  ("cm" . 1.0)
  ("in" . 2.54)
  ("pt" . 3.514598e-2)
  ("tmpt" . 2.7457797e-5)
  ("fn" . 0.4)
  ("em" . 0.4)
  ("ex" . 0.2)
  ("spc" . 0.2)
  ("pc" . 0.42175)
  ("par" . 16)
  ("pag" . 12)
  ("px" . 0.025)
  ("ln" . 0.025)
  ("%" . 1)
  ("@" . 1))

(define (make-exact x)
  (number->string (inexact->exact x)))

(define (number->htmlstring x)
  (number->string (if (exact? x)
		      (if (integer? x) x (exact->inexact x))
		      (if (and (integer? (inexact->exact x))
			       (= x (exact->inexact (inexact->exact x))))
			  (inexact->exact x) x))))

(tm-define (tmlength->htmllength len . css?)
  (if (list>0? css?) (set! css? (car css?)) (set! css? #t))
  (and-let* ((s (tmhtml-force-string len))
             (len-str (if (string->number s) (string-append s "tmpt") s))
	     (tmlen (string->tmlength len-str))
	     (dummy2? (not (tmlength-null? tmlen)))
	     (val (tmlength-value tmlen))
	     (unit (symbol->string (tmlength-unit tmlen)))
	     (incm (ahash-ref tmhtml-length-table unit))
	     (cmpx (/ 1 (ahash-ref tmhtml-length-table "px"))))
    (cond ((== unit "px") (number->htmlstring val))
	  ((in? unit '("par" "pag"))
	   (string-append (number->htmlstring (* 100 val)) "%"))
	  ((and css? (== unit "tmpt"))
	   (string-append (number->htmlstring (* cmpx val incm)) "px"))
	  ((and css? (== unit "fn"))
	   (string-append (number->htmlstring val) "em"))
	  ((and css? (== unit "spc"))
	   (string-append (number->htmlstring (/ val 2)) "em"))
	  ((and css? (== unit "ln"))
	   (string-append (number->htmlstring val) "px"))
	  ((and css? (== unit "@")) "auto")
	  (css? (tmhtml-css-safe s "auto"))
	  (else (number->htmlstring (* cmpx val incm))))))

(define (tmlength->px len)
  (and-let* ((tmlen (string->tmlength len))
	     (dummy? (not (tmlength-null? tmlen)))
	     (val (tmlength-value tmlen))
	     (unit (symbol->string (tmlength-unit tmlen)))
	     (incm (ahash-ref tmhtml-length-table unit))
	     (cmpx (/ 1 (ahash-ref tmhtml-length-table "px"))))
    (* cmpx val incm)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Local environment changes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (tmhtml-with-mode val arg)
  (ahash-with tmhtml-env :math (== val "math")
    (tmhtml (if (== val "prog") `(verbatim ,arg) arg))))

(define (tmhtml-with-math-display val arg)
  (ahash-with tmhtml-env :math-display (== val "true")
    (tmhtml arg)))

(define (tmhtml-with-color val arg)
  `((h:font (@ (color ,(tmcolor->htmlcolor val))) ,@(tmhtml arg))))

(define (tmhtml-with-font-size val arg)
  (ahash-with tmhtml-env :mag val
    (let* ((x (* (or (string->number val) 1) 100))
           (c (string-append "font-size: " (number->string x) "%"))
	   (s (cond ((< x 1) "-4") ((< x 55) "-4") ((< x 65) "-3")
		    ((< x 75) "-2") ((< x 95) "-1") ((< x 115) "0")
		    ((< x 135) "+1") ((< x 155) "+2") ((< x 185) "+3")
		    ((< x 225) "+4") ((< x 500) "+5") (else "+5"))))
      (cond (tmhtml-css? `((h:font (@ (style ,c)) ,@(tmhtml arg))))
            (s `((h:font (@ (size ,s)) ,@(tmhtml arg))))
            (else (tmhtml arg))))))

(define (tmhtml-with-block style arg)
  (with r (tmhtml (blockify arg))
    (if (in? r '(() ("") ((h:p)) ((h:p "")))) '()
	`((h:div (@ (style ,style)) ,@r)))))

(define (tmhtml-with-par-left val arg)
  (with x (tmlength->px val)
    (if (not x) (tmhtml arg)
	(with d (- x (ahash-ref tmhtml-env :left-margin))
	  (with s (string-append "margin-left: " (number->htmlstring d) "px")
	    (ahash-with tmhtml-env :left-margin x
	      (tmhtml-with-block s arg)))))))

(define (tmhtml-with-par-right val arg)
  (with x (tmlength->px val)
    (if (not x) (tmhtml arg)
	(with d (- x (ahash-ref tmhtml-env :right-margin))
	  (with s (string-append "margin-right: " (number->htmlstring d) "px")
	    (ahash-with tmhtml-env :right-margin x
	      (tmhtml-with-block s arg)))))))

(define (tmhtml-with-par-first val arg)
  (with x (tmlength->htmllength val #t)
    (if (not x) (tmhtml arg)
	(with s (string-append "text-indent: " x)
	  (tmhtml-with-block s arg)))))

(define (tmhtml-with-par-par-sep val arg)
  (with x (tmlength->px val)
    (if (and x (== (inexact->exact x) 0))
	`((h:div (@ (class "compact-block")) ,@(tmhtml arg)))
	(tmhtml arg))))

(define (tmhtml-with-one var val arg)
  (cond ((logic-ref tmhtml-with-cmd% (list var val)) =>
	 (lambda (w) (list (append w (tmhtml arg)))))
	((logic-ref tmhtml-with-cmd% (list var)) =>
	 (lambda (x) (ahash-with tmhtml-env x val (tmhtml arg))))
	((logic-ref tmhtml-with-cmd% var) =>
	 (lambda (h) (h val arg)))
	(else (tmhtml arg))))

(define (tmhtml-force-string x)
  (cond ((string? x) x)
	((func? x 'quote 1) (tmhtml-force-string (cadr x)))
	((func? x 'tmlen 1)
	 (string-append (tmhtml-force-string (cadr x)) "tmpt"))
	((func? x 'tmlen 3)
	 (string-append (tmhtml-force-string (caddr x)) "tmpt"))
	((func? x 'tuple)
         (apply string-append (list-intersperse
                                (map tmhtml-force-string (cdr x)) ";")))
	;;(else (force-string x))))
	(else (texmacs->code x "utf-8"))))

(define (tmhtml-with l)
  (cond ((null? l) '())
	((null? (cdr l)) (tmhtml (car l)))
	((null? (cddr l)) '())
	((func? (cAr l) 'graphics) (tmhtml-png (cons 'with l)))
	;; not through tmhtml-with-one: each number is a new value
	((== (car l) "html-formula-env")
	 (tmhtml-with-formula-env (cadr l) `(with ,@(cddr l))))
	(else
	 (let* ((var (tmhtml-force-string (car l)))
		(val (tmhtml-force-string (cadr l)))
		(next (cddr l)))
	   (tmhtml-with-one var val `(with ,@next))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Other macro-related primitives
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (tmhtml-compound l)
  ;; Explicit expansions are converted and handled as implicit expansions.
  (tmhtml-implicit-compound (cons (string->symbol (car l)) (cdr l))))

(define (tmhtml-mark l)
  ;; Explicit expansions are converted and handled as implicit expansions.
  (tmhtml (cadr l)))

(define (tmhtml-include l)
  (if (or (npair? l) (nstring? (car l))) '()
      (with u (url-relative (save-source-url) (unix->url (car l)))
	(if (not (url-exists? u)) '()
	    (with-global current-save-source u
	      (with t (tree-inclusion u)
		(tmhtml (tm->stree t))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Source code
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (blue sym)
  `(h:font (@ (color "blue")) ,sym))

(define (tmhtml-src-args l)
  (if (null? l) l
      `(,(blue "|")
	,@(tmhtml (car l))
	,@(tmhtml-src-args (cdr l)))))

(define (tmhtml-inline-tag l)
  `(,(blue "&lt;")
    ,@(tmhtml (car l))
    ,@(tmhtml-src-args (cdr l))
    ,(blue "&gt;")))

(define (tmhtml-open-tag l)
  `(,(blue "&lt;\\")
    ,@(tmhtml (car l))
    ,@(tmhtml-src-args (cdr l))
    ,(blue "|")))

(define (tmhtml-middle-tag l)
  `(,@(tmhtml-src-args (cdr l))
    ,(blue "|")))

(define (tmhtml-close-tag l)
  `(,@(tmhtml-src-args (cdr l))
    ,(blue "&gt;")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Other primitives
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (tmhtml-label l)
  ;; WARNING: bad conversion if ID is not a string.
  `((h:a (@ (id ,(cork->html (force-string (car l))))))))

;(define (tmhtml-reference l)
;  (list 'ref (cork->html (force-string (car l)))))

;(define (tmhtml-pageref l)
;  (list 'pageref (cork->html (force-string (car l)))))

(define (tmhtml-suffix s)
  ;; Change .tm suffix to .xhtml suffix for local files for correct
  ;; conversion of entire web-sites. We might create an option
  ;; in order to disable this suffix change
  (let* ((sdir (string-rindex s #\/))
	 (sep (string-rindex s #\#)))
    (cond ((or (string-starts? s "http:")
               (string-starts? s "https:")
               (string-starts? s "ftp:")) s)
          ((and sep (or (not sdir) (< sdir sep)))
	   (string-append (tmhtml-suffix (substring s 0 sep))
			  (string-drop s sep)))
	  ((string-ends? s ".tm")
	   (string-append (string-drop-right s 3)
			  (if tmhtml-mathml? ".xhtml" ".html")))
	  ((string-ends? s ".texmacs")
	   (string-append (string-drop-right s 8) ".tm"))
	  (else s))))

(define (tmhtml-hyperlink l)
  ;; WARNING: bad conversion if URI is not a string.
  ;; TODO: change label at start of content into ID attribute, move other
  ;; labels out (A elements cannot be nested!).
  (let* ((body (tmhtml (first l)))
	 (to (tmhtml-safe-url (cork->html (force-string (second l))))))
    (if (string-starts? to "$")
	body ;; temporary fix for URLs like $TEXMACS_PATH/...
	`((h:a (@ (href ,(tmhtml-suffix to))) ,@body)))))

(define (tmhtml-specific l)
  ;; raw HTML: <less> and <gtr> give its markup, the other characters
  ;; (Cork or <name>) are written in UTF-8 like the rest of the page
  (cond ((== (car l) "html") (list (cork->utf8 (force-string (cadr l)))))
	((== (car l) "html*") (tmhtml (cadr l)))
	((== (car l) "image") (tmhtml-png (cadr l)))
	(else '())))

(define (tmhtml-action l)
  (if tmhtml-css?
      (tmhtml (car l))
      `((h:u ,@(tmhtml (car l))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Tables
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (map* fun l)
  (list-filter (map fun l) identity))

(define (html-css-attrs l)
  ;; l is a list of either key-value lists (XML) or strings (CSS)
  ;; we return a list with the corresponding @-style attribute
  (if (null? l) '()
      (receive (css html) (list-partition l string?)
	(if (nnull? css)
	    (with style (apply string-append (list-intersperse css "; "))
	      (set! html (cons `(style ,style) html))))
	`((@ ,@html)))))

;; the base color of a pattern (5th argument), opaque, under the image
(define (html-css-pattern-base x)
  (and (tm-func? x 'pattern 5)
       (with c (tmhtml-force-string (tm-ref x 4))
         (and (!= c "") (!= c "none")
              (with h (tmcolor->htmlcolor c)
                (string-append "background-color: "
                               (if (and (string-starts? h "#")
                                        (== (string-length h) 9))
                                   (substring h 0 7)
                                   h)))))))

(tm-define (tmhtml-css-url u)
  ;; the address in url('...'): no quote, parenthesis, backslash or ; in it,
  ;; except the ; of data:image/png;base64,
  (define data? (string-starts? (tmhtml-url-normalize u) "data:image/"))
  (apply string-append
         (map (lambda (c)
                (if (or (in? c '(#\' #\" #\( #\) #\\ #\< #\>))
                        (and (== c #\;) (not data?))
                        (< (char->integer c) 32))
                    (string-append "%" (number->string (char->integer c) 16))
                    (string c)))
              (string->list (tmhtml-safe-url u #t)))))

(define (html-css-pattern x)
  (let* ((u (tmhtml-css-url (tmhtml-force-string (cadr x))))
         (w (or (tmlength->htmllength (caddr x) #t) "100%"))
         (h (or (tmlength->htmllength (cadddr x) #t) "100%"))
         (im (string-append "background: url('" u "')"))
         (sz (string-append "background-size: " w " " h))
         (base (html-css-pattern-base x)))
    (string-append im "; " sz (if base (string-append "; " base) ""))))

(define (length-attr what x . opt)
  (with len (tmlength->htmllength x #t)
    (and len (apply string-append (cons* what ": " len opt)))))

(define (border-attr what x)
  (length-attr what x " solid"))

(define (tmhtml-make-cell-attr x all)
  (cond ((== (car x) "cell-width") (length-attr "width" (cadr x)))
	((== (car x) "cell-height") (length-attr "height" (cadr x)))
	((== x '("cell-halign" "l")) "text-align: left")
	((== x '("cell-halign" "c")) "text-align: center")
	((== x '("cell-halign" "r")) "text-align: right")
	((== x '("cell-valign" "t")) "vertical-align: top")
	((== x '("cell-valign" "c")) "vertical-align: middle")
	((== x '("cell-valign" "b")) "vertical-align: bottom")
	((== x '("cell-valign" "B")) "vertical-align: baseline")
	((== (car x) "cell-background")
         (cond ((not tmhtml-css?)
                `(bgcolor ,(tmcolor->htmlcolor (cadr x))))
               ((tm-atomic? (cadr x))
                (string-append "background-color: "
                               (tmbgcolor->htmlbgcolor (cadr x))))
               ((or (tm-func? (cadr x) 'pattern 3)
                    (tm-func? (cadr x) 'pattern 4)
                    (tm-func? (cadr x) 'pattern 5))
                (html-css-pattern (cadr x)))
               (else #f)))
	((== (car x) "cell-lborder") (border-attr "border-left" (cadr x)))
	((== (car x) "cell-rborder") (border-attr "border-right" (cadr x)))
	((== (car x) "cell-tborder") (border-attr "border-top" (cadr x)))
	((== (car x) "cell-bborder") (border-attr "border-bottom" (cadr x)))
	((== (car x) "cell-lsep") (length-attr "padding-left" (cadr x)))
	((== (car x) "cell-rsep") (length-attr "padding-right" (cadr x)))
	((== (car x) "cell-tsep") (length-attr "padding-top" (cadr x)))
	((== (car x) "cell-bsep") (length-attr "padding-bottom" (cadr x)))
	((== x '("cell-block" "no")) "white-space: nowrap")
	((== x '("cell-block" "yes")) #f)
	((== x '("cell-block" "auto"))
         (if (or (in? '("cell-hyphen" "t") all)
                 (in? '("cell-hyphen" "c") all)
                 (in? '("cell-hyphen" "b") all))
             #f
             "white-space: nowrap"))
	(else #f)))

(define (tmhtml-make-cell c cellf)
  (if (not (tm-func? c 'cell 1)) (set! c `(cell ,c)))
  (ahash-with tmhtml-env :left-margin 0
    (with make (lambda (attr) (tmhtml-make-cell-attr attr cellf))
      `(h:td ,@(html-css-attrs (map* make cellf))
             ,@(tmhtml (cadr c))))))

(define (tmhtml-make-cells-bis l cellf)
  (if (null? l) l
      (cons (tmhtml-make-cell (car l) (car cellf))
	    (tmhtml-make-cells-bis (cdr l) (cdr cellf)))))

(define (tmhtml-width-part attrl)
  (cond ((null? attrl) 0)
	((== (caar attrl) "cell-hpart") (string->number (cadar attrl)))
	(else (tmhtml-width-part (cdr attrl)))))

(define (tmhtml-width-replace attrl sum)
  (with part (tmhtml-width-part (reverse attrl))
    (if (== part 0) attrl
	(with l (list-filter attrl (lambda (x) (!= (car x) "cell-width")))
	  (with w (number->htmlstring (/ part sum))
	    (cons (list "cell-width" (string-append w "par")) l))))))

(define (tmhtml-make-cells l cellf)
  (let* ((partl (map tmhtml-width-part (map reverse cellf)))
	 (sum (apply + partl)))
    (if (!= sum 0) (set! cellf (map (cut tmhtml-width-replace <> sum) cellf)))
    (tmhtml-make-cells-bis l cellf)))

(define (tmhtml-make-row-attr x all)
  (tmhtml-make-cell-attr x all))

(define (tmhtml-make-row r rowf cellf)
  `(h:tr ,@(html-css-attrs (map* (cut tmhtml-make-row-attr <> rowf) rowf))
	 ,@(tmhtml-make-cells (cdr r) cellf)))

(define (tmhtml-make-rows l rowf cellf)
  (if (null? l) l
      (cons (tmhtml-make-row  (car l) (car rowf) (car cellf))
	    (tmhtml-make-rows (cdr l) (cdr rowf) (cdr cellf)))))

(define (tmhtml-make-column-attr x all)
  (tmhtml-make-cell-attr x all))

(define (tmhtml-make-col colf)
  `(h:col ,@(html-css-attrs (map* (cut tmhtml-make-column-attr <> colf) colf))))

(define (tmhtml-make-column-group colf)
  (if (list-every null? colf) '()
      `((h:colgroup ,@(map tmhtml-make-col colf)))))

(define (tmhtml-make-table-attr x)
  (cond ((== (car x) "table-width") (length-attr "width" (cadr x)))
	((== (car x) "table-height") (length-attr "height" (cadr x)))
	((== (car x) "table-lborder") (border-attr "border-left" (cadr x)))
	((== (car x) "table-rborder") (border-attr "border-right" (cadr x)))
	((== (car x) "table-tborder") (border-attr "border-top" (cadr x)))
	((== (car x) "table-bborder") (border-attr "border-bottom" (cadr x)))
	((== (car x) "table-lsep") (length-attr "padding-left" (cadr x)))
	((== (car x) "table-rsep") (length-attr "padding-right" (cadr x)))
	((== (car x) "table-tsep") (length-attr "padding-top" (cadr x)))
	((== (car x) "table-bsep") (length-attr "padding-bottom" (cadr x)))
	(else #f)))

(define (tmhtml-make-table t tablef colf rowf cellf)
  (let* ((attrs (map* tmhtml-make-table-attr tablef))
         (va "vertical-align: middle"))
    (if (not (list-find attrs (cut == <> "width: 100%")))
	(set! attrs (cons* "display: inline-table" va attrs)))
    `(h:table ,@(html-css-attrs attrs)
	      ,@(tmhtml-make-column-group colf)
	      (h:tbody ,@(tmhtml-make-rows (cdr t) rowf cellf)))))

(define (tmhtml-table l)
  (list (tmhtml-make-table (cons 'table l) '() '() '() '())))

(tm-define (tmhtml-tformat l)
  (with t (tmtable-normalize (cons 'tformat l))
    (receive (tablef colf rowf cellf) (tmtable-properties* t)
      (list (tmhtml-make-table (cAr t) tablef colf rowf cellf)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Pictures
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (tmhtml-collect-labels x)
  (cond ((nlist? x) '())
	((and (func? x 'label 1) (string? (cadr x))) `((id ,(cadr x))))
	(else (append-map tmhtml-collect-labels (cdr x)))))

(define (tmhtml-math-labels x)
  ;; labels inside formulas are lost by the MathML and MathJax conversions
  (map (lambda (id) `(h:a (@ (id ,(cork->html (cadr id))))))
       (tmhtml-collect-labels x)))

(define (tmhtml-image-names ext key)
  (set! tmhtml-image-serial (+ tmhtml-image-serial 1))
  (let* ((postfix (string-append
		   "-" (number->string tmhtml-image-serial) "." ext))
	 (name-url (url-glue tmhtml-image-root-url postfix))
	 (name-string (string-append tmhtml-image-root-string postfix)))
    (if tmhtml-image-loose?
	(loose-image-names key (string-append "." ext))
	(values name-url name-string))))

(define (magic-png-number)
  (let* ((css (get-preference "texmacs->html:css-stylesheet"))
         (s (url->string (url-tail css))))
    (if (string-starts? s "web-") 19000.0 20625.0)))

(define (tmhtml-formula-with? x)
  ;; a formula of the expansion of a buffer: a with whose first variable is
  ;; the number of the environment of the formula (see exec_with)
  (and (func? x 'with) (>= (length x) 4) (== (cadr x) "html-formula-env")))

(define (tmhtml-formula-body x)
  ;; the formula x without the number of its environment
  (cons 'with (cdddr x)))

(define (tmhtml-formula-env-of x)
  ;; the number of the environment of the formula x, under the with and the
  ;; documents around it, but not of a formula inside other markup of x
  (cond ((tmhtml-formula-with? x) (string->number (caddr x)))
        ((func? x 'with) (tmhtml-formula-env-of (cAr x)))
        ((func? x 'document 1) (tmhtml-formula-env-of (cadr x)))
        (else #f)))

(define (tmhtml-without-envs x)
  (cond ((nlist? x) x)
        ((tmhtml-formula-with? x)
         (tmhtml-without-envs (tmhtml-formula-body x)))
        (else (cons (car x) (map tmhtml-without-envs (cdr x))))))

(define (tmhtml-snippet-key x nr)
  ;; the key of x in the caches of the export, before it is typeset; the
  ;; same formula in the same environment without looking again
  (if (not nr) (object->string x)
      (let* ((k (object->string (cons nr x)))
             (id (or (ahash-ref tmhtml-snippet-ids k) (html-snippet-id x nr))))
        (and (>= id 0) (begin (ahash-set! tmhtml-snippet-ids k id) id)))))

(define (tmhtml-snippet-made x nr)
  ;; the key of x once it is typeset, or #f when its inputs are not known
  (if (not nr) (object->string x)
      (with id (html-snippet-last)
        (and (>= id 0)
             (begin
               (ahash-set! tmhtml-snippet-ids (object->string (cons nr x)) id)
               id)))))

(define (tmhtml-png y*)
  ;; converts generic markup that can be displayed on screen to png format;
  ;; a formula in its environment, at the size of the text around it
  (let* ((nr (or (tmhtml-formula-env-of y*) (ahash-ref tmhtml-env :static-env)))
         (y (if nr `(with "font-size" "1" ,(tmhtml-without-envs y*)) y*))
         (mag (ahash-ref tmhtml-env :mag))
	 (x (if (or tmhtml-css? (nstring? mag) (== mag "1")) y
                (with nmag `(times (value "magnification") ,mag)
                  `(with "magnification" ,nmag ,y))))
	 (l1 (tmhtml-collect-labels y))
	 (l2 (if (null? l1) l1 (list (car l1)))))
    ;; a formula by the number of what its typesetting read (equal formulas
    ;; whose environments differ in nothing which they read are the same),
    ;; something else as a string (the hash of Guile only sees the top of a
    ;; list)
    (with cached (ahash-ref tmhtml-image-cache (tmhtml-snippet-key x nr))
      (if (not cached)
	  (receive (name-url name-string) (tmhtml-image-names "png" x)
	    ;;(display* x " -> " name-url ", " name-string "\n")
	    (let* ((cleared? (image-target-clear name-url))
                   (extents (if nr (print-snippet-in name-url x nr)
                                (print-snippet name-url x #t)))
                   (written (image-target-written name-url cleared?))
                   (m (magic-png-number))
                   (unit (/ 6000.0 (* (ninth extents) (tenth extents) m)))
		   (x3 (* (first extents) unit))
		   (y3 (* (second extents) unit))
		   (x4 (* (third extents) unit))
		   (y4 (* (fourth extents) unit))
		   (x1 (* (fifth extents) unit))
		   (y1 (* (sixth extents) unit))
		   (x2 (* (seventh extents) unit))
		   (y2 (* (eighth extents) unit))
                   (lmar (number->htmlstring (- x3 x1)))
                   (bmar (number->htmlstring (- y3 y1)))
                   (rmar (number->htmlstring (- x2 x4)))
                   (tmar (number->htmlstring (- y2 y4)))
		   (valign (number->htmlstring y1))
		   (height (number->htmlstring (- y4 y3)))
		   (style (string-append "margin-left: " lmar "em; "
                                         "margin-bottom: " bmar "em; "
                                         "margin-right: " rmar "em; "
                                         "margin-top: " tmar "em; "
                                         "vertical-align: " valign "em; "
                                         "height: " height "em"))
                   (attrs (if tmhtml-css?
                              `((src ,name-string) (style ,style) ,@l2)
                              `((src ,name-string) ,@l2)))
                   (img (if (url-exists? name-url)
                            `((h:img (@ ,@attrs)))
                            `())))
	      ;;(display* x " -> " extents "\n")
	      (set! cached img)
	      (with key (tmhtml-snippet-made x nr)
                (when key (ahash-set! tmhtml-image-cache key cached)))))
	  cached))))

(define (tmhtml-graphics l)
  (tmhtml-png (cons 'graphics l)))

(define (tmhtml-draw-over l)
  (tmhtml-png (cons 'draw-over l)))

(define (tmhtml-draw-under l)
  (tmhtml-png (cons 'draw-under l)))

(define (tmhtml-image-suffix name)
  (cond ((string? name) (url-suffix name))
        ((and (func? name 'tuple 2) (string? (caddr name)))
         (if (== (url-suffix (caddr name)) "") name (url-suffix (caddr name))))
        (else "")))

(define (tmhtml-web-image-name name)
  ;; make sure image is accessible from html
  ;; FIXME: we should replace ~, environment variables, etc.
  (let* ((un  (unix->url name))
         (u (url-relative (save-target-url) un))
         (suf (url-suffix un)))
    (if (url-exists? u)
        name
      (with u1 (url-relative (current-buffer-url) un)
        (if (url-exists? u1)
          (receive (name-url name-string) (tmhtml-image-names suf name)
            (system-copy u1 name-url)
            name-string)
           name))))) ;; image does not exist, can't do much

(define (tmhtml-image l)
  ;; FIXME: Should also test that width and height are not magnifications.
  ;; Currently, magnifications make tmlength->htmllength return #f.
  (cond ((nin? (tmhtml-image-suffix (car l)) tmhtml-web-formats)
         ;; linked or embedded non-web image: convert & save to png  
         (tmhtml-png (cons 'image l)))
        ((and (func? (car l) 'tuple 2)
              (func? (cadar l) 'raw-data 1)
              (string? (cadr (cadar l)))
              (string? (caddar l)))
         ;; embedded web image, extract it    
         (receive (name-url name-string)
                  (tmhtml-image-names (url-suffix (cork->utf8 (caddar l)))
                                      (cadr (cadar l)))
           (with cleared? (image-target-clear name-url)
             (string-save (cadr (cadar l)) name-url)
             (image-target-written name-url cleared?))
           (tmhtml-image (cons name-string (cdr l)))))
        ((nstring? (first l))
         ;; treat complex images as generic markup
         (tmhtml-png (cons 'image l)))
        (else
          ;; web images
          (let* ((s (tmhtml-safe-url
                     (cork->html (tmhtml-web-image-name (first l))) #t))
                 (w (tmlength->htmllength (second l) #f))
                 (h (tmlength->htmllength (third l) #f)))
            `((h:img (@ (class "image")
                        (src ,s)
                        ,@(if w `((width ,w)) '())
                        ,@(if h `((height ,h)) '()))))))))

(define (tmhtml-ornament-get-env-style)
  (let* ((l0 (hash-map->list list tmhtml-env))
         (l1 (filter (lambda (x)
                       (and (list>0? (car x))
                            (cadr x)
                            (string-prefix? "#:ornament-"
                                            (object->string (caar x))))) l0))
         (l2   (map car l1))
         (args (map cadr l1))
         (funs (map cAr l2))
         (stys (map (lambda (x) (cdr (cDr x))) l2)))
    (apply
      string-append
      (list-intersperse
        (map (lambda (f arg sty)
               (with args (string-tokenize-by-char arg #\;)
                 (apply
                   string-append
                   (list-intersperse
                     (cond ((== (length args) (length sty))
                            (map (lambda (x y)
                                   (string-append x ":" (or (f y) "0"))) sty args))
                           ((>= 1 (length sty))
                            (map (lambda (y)
                                   (string-append (car sty) ":" (or (f y) "0"))) args))
                           (else '()))
                     ";"))))
             funs args stys) ";"))))

(define (contains-surround? l)
  (cond ((nlist? l) #f)
        ((func? l 'surround 3) #t)
        (else (with r #f
                (for-each (lambda (x)
                            (set! r (or r (contains-surround? x)))) l)
                r))))

(define (tmhtml-ornament l)
  (let* ((body (tmhtml (car l)))
         (styl (tmhtml-ornament-get-env-style))
         (styl (if (contains-surround? l)
                 (string-append styl ";display:block;") styl))
         (args (if (== styl "") '() `((style ,styl))))
         (tag  (if (stm-block-structure? (car l)) 'h:div 'h:span)))
    `((,tag (@ (class "ornament") ,@args) ,@body))))

(define (tmhtml-balloon l)
  (let* ((anch (tmhtml (car  l)))
         (body (tmhtml (cadr l)))
         (tag1 (if (stm-block-structure? (car  l)) 'h:div 'h:span))
         (tag2 (if (stm-block-structure? (cadr l)) 'h:div 'h:span)))
    `((,tag1 (@ (class "balloon-anchor")) ,@anch
             (,tag2 (@ (class "balloon-body") (hidden "hidden")) ,@body)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Standard markup
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (transform-item-post l)
  (if (not (tm-is? (car l) '!item))
      `(document ,@l)
      `(!item ,(cadar l) (document ,(caddar l) ,@(cdr l)))))

(define (item? x) (tm-in? x '(item item*)))

(define (transform-items x)
  (cond ((and (tm-is? x 'concat) (nnull? (cdr x))
              (item? (cadr x)))
         `(!item ,(cadr x) (concat ,@(cddr x))))
        ((and (tm-is? x 'concat) (nnull? (cdr x)) (nnull? (cddr x))
              (tm-is? (cadr x) 'assign) (item? (caddr x)))
         `(!item ,(caddr x) (concat ,@(cdddr x))))
        ((item? x)
         `(!item ,x (hspace "1pt")))
        ((tm-is? x 'with)
         `(,@(cDr x) ,(transform-items (cAr x))))
        ((tm-is? x 'document)
         (let* ((r  (map transform-items (cdr x)))
                (p? (lambda (i) (tm-is? i '!item)))
                (sr (list-scatter r p? #t))
                (fr (list-filter sr nnull?)))
           `(document ,@(map transform-item-post fr))))
        (else x)))

;; TODO: when the first data of the list is a label, it must be used to set the
;; ID attribute of the resulting xhtml element. When that is done, remove the
;; warning comment from htmltm-handler.

(define (tmhtml-static-bold thunk)
  ;; for the formulas which thunk writes: the tag is written in bold
  (ahash-with tmhtml-env :static-bold #t
    (thunk)))

(define (tmhtml-post-item args)
  (let* ((i (car args))
         (r (tmhtml (cadr args))))
    (if (or (tm-is? i 'item) (null? (cdr i)))
        `((h:li ,@r))
        (with desc? (ahash-ref tmhtml-env :description)
          `((h:dt ,@(if (and tmhtml-static-math? (not desc?))
                        ;; a name in bold is proper to description
                        '((@ (style "font-weight: normal"))) '())
                  ,@(if (and tmhtml-static-math? desc?)
                        (tmhtml-static-bold (lambda () (tmhtml (cadr i))))
                        (tmhtml (cadr i))))
            (h:dd ,@r))))))

(define (tmhtml-itemize args)
  (ahash-with tmhtml-env :description #f
    `((h:ul ,@(tmhtml (transform-items (car args)))))))

(define (tmhtml-enumerate args)
  (ahash-with tmhtml-env :description #f
    `((h:ol ,@(tmhtml (transform-items (car args)))))))

(define (tmhtml-description args)
  ;; the names of its items are bold, unlike those of itemize
  (ahash-with tmhtml-env :description #t
    `((h:dl ,@(tmhtml (transform-items (car args)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Verbatim
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (tmhtml-verbatim args)
  ;; Block-level verbatim environments should only contain inline elements.
  ;;
  ;; @args should be a single element list, we will call this element @body.
  ;;
  ;; If @body is a block structure, it will be either:
  ;; -- a simple DOCUMENT (normal case), and @(tmhtml body) will produce a list
  ;;    of h:p elements;
  ;; -- a block structure producing a single element (degenerate case).
  ;;
  ;; Verbatim structures which do not contain a DOCUMENT but are direct
  ;; children of a DOCUMENT (i.e. they occupy a whole paragraph) are degenerate
  ;; cases of block-level verbatim and must be exported as PRE.
  ;;
  ;; Inline verbatim has little special significance for display in TeXmacs. In
  ;; LaTeX it is used to escape special characters (and protect multiple inline
  ;; spaces, yuck!), but in TeXmacs there is no such problem.
  (with body (first args)
    (with pre? #f
      (when (func? body 'with 1)
      (set! body (cAr body)))
      (when (and (func? body 'with 3) (== (cadr body) "locus-color"))
        (set! body (cAr body)))
      (when (func? body 'action)
        (set! body (cadr body))
        (set! pre? (string? body)))
      (ahash-with tmhtml-env :verbatim #t
        (if (or (stm-block-structure? body) pre?)
            (verbatim-pre
             (ahash-with tmhtml-env :preformatted #t
               (tmhtml body)))
            (verbatim-tt (tmhtml body)))))))

(define (tmhtml-code* l)
  ;; Inline code: no ligatures (e.g. "--" must not become an en-dash)
  (ahash-with tmhtml-env :verbatim #t
    (tmhtml-post-simplify-element (cons 'h:code (tmhtml-list l)))))

(define (verbatim-tt content)
  `((h:tt (@ (class "verbatim")) ,@content)))

(define (verbatim-pre content)
  `((h:pre (@ (class "verbatim") (xml:space "preserve")) ,@content)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Additional tags
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (tmhtml-doc-title-block l)
  `((h:table (@ (class "title-block"))
	     (h:tr (h:td ,@(tmhtml (car l)))))))

(define (tmhtml-equation* l)
  (with first (simplify-document (car l))
    (with x `(with "mode" "math" (with "math-display" "true" ,first))
      (ahash-with tmhtml-env :math-display #t
        (if tmhtml-static-math?
            ;; boxes of formulas have no line height around them
            `((h:center (@ (style "margin: 0.5em 0"))
                        ,@(tmhtml (if (tmhtml-recorded? (car l)) (car l) x))))
            `((h:center ,@(tmhtml x))))))))

(define (tmhtml-recorded? x)
  ;; a formula of the expansion of a buffer (see tmhtml-display-body)
  (cond ((tmhtml-formula-with? x) #t)
        ((func? x 'with) (tmhtml-recorded? (cAr x)))
        ((func? x 'document 1) (tmhtml-recorded? (cadr x)))
        (else #f)))

(define (tmhtml-equation-lab l)
  (with first (simplify-document (car l))
    (with x (if (and tmhtml-static-math? (tmhtml-recorded? (car l))) (car l)
                `(with "mode" "math" (with "math-display" "true" ,first)))
      (ahash-with tmhtml-env :math-display #t
        `((h:table (@ (width "100%")
                      ,@(if tmhtml-static-math?
                            '((style "margin: 0.5em 0")) '()))
                   (h:tr (h:td (@ (align "center") (width "100%"))
                               ,@(tmhtml x))
                         (h:td (@ (align "right"))
                               "(" ,@(tmhtml (cadr l)) ")"))))))))

(define (tmhtml-mathjax-mode-after l text?)
  ;; is it text after the variables @l of a with, which end with its body?
  (cond ((or (null? l) (null? (cdr l))) text?)
        ((== (car l) "mode")
         (tmhtml-mathjax-mode-after (cddr l) (!= (cadr l) "math")))
        (else (tmhtml-mathjax-mode-after (cddr l) text?))))

(define (tmhtml-mathjax-no-text-phantoms x text?)
  ;; MathJax runs no macros in \text: the phantom would be shown as its source
  (cond ((or (nlist? x) (null? x)) x)
        ((and text? (in? (car x) '(phantom hphantom vphantom))) "")
        (else
         (let ((in-text?
                (cond ((tm-func? x 'with)
                       (tmhtml-mathjax-mode-after (cdr x) text?))
                      ((== (car x) 'text) #t)
                      ((== (car x) 'math) #f)
                      (else text?))))
           (cons (car x)
                 (map (cut tmhtml-mathjax-no-text-phantoms <> in-text?)
                      (cdr x)))))))

(define (tmhtml-mathjax-formula* x*)
  ;;(display* "x= " x* "\n")
  (let* ((opts (list (cons "texmacs->latex:mathjax" "on")))
         (x (tmhtml-mathjax-no-text-phantoms x* #f))
         (s (serialize-latex (texmacs->latex x opts)))
         (display? (ahash-ref tmhtml-env :math-display))
         (style (if display? "\\displaystyle " ""))
         (mj (string-append "\\(" style s "\\)")))
    (when (and (string-starts? mj "\\(\\displaystyle \\begin{array}{rcl}")
               (string-ends?   mj "\\end{array}\\)"))
      (set! mj (string-append "\\begin{eqnarray*}"
                              (substring mj 34 (- (string-length mj) 13))
                              "\\end{eqnarray*}")))
    (list mj)))

(define (tmhtml-mathjax-formula x)
  (cond ((tm-func? x 'with 1)
         (tmhtml-mathjax-formula (cadr x)))
        ((and (tm-func? x 'with 3)
              (== (cadr x) "math-display")
              (== (caddr x) "true"))
         (ahash-with tmhtml-env :math-display #t
           (tmhtml-mathjax-formula (cadddr x))))
        (else (tmhtml-mathjax-formula* x))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Static formulas: typeset here and written as SVG (Plugins/Html_math)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (tmhtml-static-defs)
  ;; the outlines of the glyphs used so far, once for the whole document
  (with defs (html-math-defs)
    (if (null? defs) '() (list `(*RAW* ,defs)))))

;; Tags kept as tags in HTML, which write the text in them in bold
(define tmhtml-static-bold-tags
  '(strong chapter-title section-title subsection-title subsubsection-title
    paragraph-title subparagraph-title))

(define (tmhtml-static-strip-labels x)
  ;; their anchors are written apart; typesetting them here would change the
  ;; references of the buffer
  (cond ((nlist? x) x)
        ((func? x 'label) "")
        ((null? (tmhtml-collect-labels x)) x)
        (else (cons (car x) (map tmhtml-static-strip-labels (cdr x))))))

(define (tmhtml-static-strip-numbering x)
  ;; the counters, which the import would set again
  (cond ((nlist? x) x)
        ((tm-in? x '(assign hidden-binding)) "")
        (else (cons (car x) (map tmhtml-static-strip-numbering (cdr x))))))

(define (tmhtml-static-tree x)
  ;; its size follows the HTML text around it, and so does bold
  `(with "mode" "math" "magnification" "1" "font-size" "1"
         ,@(if (ahash-ref tmhtml-env :static-bold)
               '("font-series" "bold" "math-font-series" "bold") '())
         ,(tmhtml-static-strip-labels x)))

(define (tmhtml-display-formula x)
  ;; the body of a displayed formula, see tmhtml-display-body
  (and (func? x 'with) (>= (length x) 4) (== (cadr x) "html-formula")
       (cons* 'with "mode" "math" (cdddr x))))

(define (tmhtml-with-formula-env val arg)
  ;; a formula, with the number of its environment (see exec_with)
  (ahash-with tmhtml-env :static-env (string->number val)
    (tmhtml (or (tmhtml-display-formula arg) arg))))

(define tmhtml-keep-envs? #f)

(define (tmhtml-keep-env x y)
  ;; y for the formula x of the expansion, with its environment if it is kept
  (if tmhtml-keep-envs?
      `(with "html-formula-env" ,(caddr x)
             ,@(if (func? y 'with) (cdr y) (list y)))
      y))

(define (tmhtml-unrecord x)
  ;; the expansion as for the other ways to write formulas: their tags, and
  ;; only for images the environments of formulas (see tmhtml-png)
  (cond ((nlist? x) x)
        ((tmhtml-formula-with? x)
         (tmhtml-keep-env x (tmhtml-unrecord (tmhtml-formula-body x))))
        ((func? x 'html-display)
         (cons 'equation* (map tmhtml-undisplay (cdr x))))
        ((func? x 'html-equations-base)
         (cons 'equations-base (map tmhtml-undisplay (cdr x))))
        ((func? x 'html-equation-lab)
         (cons 'equation-lab (map tmhtml-undisplay (cdr x))))
        (else (cons (car x) (map tmhtml-unrecord (cdr x))))))

(define (tmhtml-recorded-somewhere? x)
  (and (pair? x)
       (or (tmhtml-formula-with? x)
           (list-or (map tmhtml-recorded-somewhere? (cdr x))))))

(define (tmhtml-record x)
  ;; a tree which is not the expansion of a buffer is expanded as one, in the
  ;; current buffer, so that its formulas have their environment
  (cond ((tmhtml-recorded-somewhere? x) x)
        ((func? x '!file)
         `(!file ,(tree->stree (html-expand (stree->tree (cadr x))))
                 ,@(cddr x)))
        (else (tree->stree (html-expand (stree->tree x))))))

(define (tmhtml-undisplay x)
  ;; the formula of tmhtml-display-body, which equation* makes again; the
  ;; with of math-colored around it only serves the static formulas
  (or (tmhtml-undisplay-body x) (tmhtml-unrecord x)))

(define (tmhtml-undisplay-body x)
  (cond ((and (func? x 'with 5)
              (== (cdr (cDr x)) '("html-formula" "display"
                                  "math-display" "true")))
         (tmhtml-unrecord (cAr x)))
        ((tmhtml-formula-with? x)
         (with y (tmhtml-undisplay-body (tmhtml-formula-body x))
           (and y (tmhtml-keep-env x y))))
        ((and (func? x 'with) (pair? (cdr x))) (tmhtml-undisplay-body (cAr x)))
        (else #f)))

(define (tmhtml-static-links s)
  ;; the targets of the links, as those of the links of the text
  (with start "<a href=\""
    (let loop ((pos 0) (acc '()))
      (let ((i (string-search-forwards start pos s)))
        (if (< i 0)
            (apply string-append
                   (reverse (cons (substring s pos (string-length s)) acc)))
            (let* ((b (+ i (string-length start)))
                   (e (string-search-forwards "\"" b s)))
              (loop e (cons* (tmhtml-suffix (substring s b e))
                             (substring s pos b) acc))))))))

(define (tmhtml-static-svgs x)
  ;; several boxes for a long inline formula, for the browser to break it
  (let* ((nr (or (ahash-ref tmhtml-env :static-env) -1))
         (t (tmhtml-static-tree x))
         (key (tmhtml-snippet-key t nr))
         (known (and key (ahash-ref tmhtml-static-cache key))))
    (or known
        ;; a displayed formula is not cut: html_math_snippet sees it
        (let* ((r (html-math-snippet t nr #t))
               (l (map (lambda (s) `(*RAW* ,(tmhtml-static-links s)))
                       (if (string? r) (list r) r)))
               (made (tmhtml-snippet-made t nr)))
          (when made (ahash-set! tmhtml-static-cache made l))
          l))))

(define tmhtml-static-hidden
  (string-append "position:absolute;width:1px;height:1px;overflow:hidden;"
                 "clip:rect(0 0 0 0);-webkit-user-select:none;user-select:none"))

;; A formula is selected as a whole: Safari otherwise selects its hidden
;; MathML when a drag starts on it (it needs the -webkit- prefixes)
(define tmhtml-static-math
  "position:relative;-webkit-user-select:all;user-select:all")

;; The formula as LaTeX, as transparent text over it: it is found by the
;; search of the browser and copied with the text around it (the hidden
;; MathML, for screen readers, is not selected); the links of the formula
;; are below it
(define tmhtml-static-text
  (string-append "position:absolute;left:0;top:0;width:100%;height:100%;"
                 "overflow:hidden;white-space:nowrap;color:transparent;"
                 "pointer-events:none"))

(define (tmhtml-static-pictures? x)
  (and (pair? x)
       (or (in? (car x) '(graphics image draw-over draw-under tree))
           (and (== (car x) 'specific) (pair? (cdr x)) (== (cadr x) "image"))
           (list-or (map tmhtml-static-pictures? (cdr x))))))

(define (tmhtml-static-tex x)
  ;; the LaTeX of the formula x, or #f: none for a formula with pictures,
  ;; which the converter would write as files; a cell of a displayed table
  ;; is a formula of the text
  (let* ((display? (and (ahash-ref tmhtml-env :math-display)
                        (not (ahash-ref tmhtml-env :static-cell))))
         (key (object->string (cons display? x)))
         (known (ahash-ref tmhtml-static-texs key)))
    (if known (car known)
        (with r (and (not (tmhtml-static-pictures? x))
                     (tmhtml-static-tex* x (if display? "$$" "$")))
          (ahash-set! tmhtml-static-texs key (list r))
          r))))

(define (tmhtml-static-tex* x d)
  (catch #t
    (lambda ()
      (let* ((opts (list (cons "texmacs->latex:mathjax" "on")))
             (y (tmhtml-unrecord
                 (tmhtml-static-strip-numbering
                  (tmhtml-static-strip-labels x))))
             (z (tmhtml-mathjax-no-text-phantoms y #f))
             (s (serialize-latex (texmacs->latex z opts)))
             ;; (the lines of the LaTeX are one line of text)
             (t (and (string? s) (string-replace s "\n" " "))))
        (and t (not (string-null? (tm-string-trim-both t)))
             (cork->html (string-append d t d)))))
    (lambda args #f)))

(define (tmhtml-static-text-layer x)
  (with s (tmhtml-static-tex x)
    (if (not s) '()
        (list `(h:span (@ (class "tm-tex") (aria-hidden "true")
                          ,@(if tmhtml-in-file? '()
                                `((style ,tmhtml-static-text))))
                       ,s)))))

;; The MathML of a formula is written as one string, not as part of the SXML
;; tree of the document, so that its many nodes do not go through the later
;; passes on the tree and the writer of HTML one by one.
(define (tmhtml-markup x)
  (if (string? x) x
      (let* ((s (symbol->string (car x)))
             (n (if (string-starts? s "m:")
                    (substring s 2 (string-length s)) s))
             (at? (and (pair? (cdr x)) (func? (cadr x) '@)))
             (at (if at? (cdadr x) '()))
             (kids (if at? (cddr x) (cdr x))))
        (string-append
         "<" n
         (apply string-append
                (map (lambda (a)
                       (string-append " " (symbol->string (car a)) "=\""
                                      (tmmath-escape (cadr a) #t)
                                      "\""))
                     at))
         ">" (apply string-append (map tmhtml-markup kids)) "</" n ">"))))

(define (tmhtml-static-source x)
  ;; hidden MathML: for screen readers, and to read the formula back
  (catch #t
    (lambda ()
      (list `(*RAW* ,(tmhtml-markup
                      `(math (@ (xmlns "http://www.w3.org/1998/Math/MathML")
                                ,(if tmhtml-in-file?
                                     '(class "tm-src")
                                     `(style ,tmhtml-static-hidden)))
                             ,(texmacs->mathml x tmhtml-env))))))
    (lambda args '())))

;; Browsers break lines after an inline <svg>, even before a comma (and
;; a word joiner does not prevent it): the last box of a formula and the
;; punctuation after it go together. Outside the formula: on import it is
;; replaced by its source, and the punctuation remains.

(define (tmhtml-static-node? x)
  (and (pair? x) (== (car x) 'h:span)
       (pair? (cdr x)) (func? (cadr x) '@)
       (in? '(class "tm-math") (cdadr x))))

(define (tmhtml-static-box? x)
  ;; a box of a formula, not its glyphs nor its hidden MathML
  (and (func? x '*RAW*)
       (string-starts? (cadr x)
                       (string-append "<svg xmlns=\"http://www.w3.org/2000/"
                                      "svg\" aria-hidden=\"true\" fill="))))

(define (tmhtml-punctuation-end s)
  ;; the number of punctuation characters at the beginning of s
  (let loop ((i 0))
    (if (and (< i (string-length s))
             (in? (string-ref s i)
                  '(#\, #\. #\; #\: #\! #\? #\) #\])))
        (loop (+ i 1))
        i)))

(define (tmhtml-static-bind-punctuation l)
  (cond ((or (not tmhtml-static-math?) (null? l) (null? (cdr l))) l)
        ((and (tmhtml-static-node? (car l)) (string? (cadr l))
              (> (tmhtml-punctuation-end (cadr l)) 0))
         (let* ((n (tmhtml-punctuation-end (cadr l)))
                (rest (substring (cadr l) n (string-length (cadr l))))
                (kids (cddr (car l)))
                (boxes (list-filter kids tmhtml-static-box?))
                (others (list-filter kids (non tmhtml-static-box?))))
           (append
            ;; (a formula of one box goes with the punctuation as a whole,
            ;; with its text, see tmhtml-static-text)
            (if (null? (cDr boxes))
                (list `(h:span (@ (style "white-space:nowrap")) ,(car l)
                               ,(substring (cadr l) 0 n)))
                (list `(h:span ,(cadr (car l)) ,@(cDr boxes) ,@others)
                      `(h:span (@ (style "white-space:nowrap")) ,(cAr boxes)
                               ,(substring (cadr l) 0 n))))
            (if (string-null? rest) '() (list rest))
            (tmhtml-static-bind-punctuation (cddr l)))))
        (else (cons (car l) (tmhtml-static-bind-punctuation (cdr l))))))

(define (tmhtml-static-unwrap x vars)
  ;; the body of a displayed formula, and the variables of the with around it
  (cond ((and (func? x 'with) (even? (length x)))
         (tmhtml-static-unwrap (cAr x) (append vars (cDr (cdr x)))))
        ((func? x 'document 1) (tmhtml-static-unwrap (cadr x) vars))
        (else (cons vars x))))

(define (tmhtml-static-table-parts x formats)
  ;; the table of nested tformat, and their formats
  (cond ((func? x 'tformat)
         (tmhtml-static-table-parts (cAr x) (append formats (cDr (cdr x)))))
        ((func? x 'table) (cons formats x))
        ((func? x 'document 1) (tmhtml-static-table-parts (cadr x) formats))
        (else #f)))

(define (tmhtml-static-table-width formats)
  ;; the width of a table as wide as the paragraph, in HTML
  (let* ((l (list-filter formats
                         (lambda (f) (and (func? f 'twith 2)
                                          (== (cadr f) "table-width")))))
         (w (and (nnull? l) (caddr (cAr l)))))
    (and (string? w) (string-ends? w "par")
         (with n (string->number (substring w 0 (- (string-length w) 3)))
           (and n (string-append (number->string (* 100 n)) "%"))))))

(define (tmhtml-static-cell-format formats row rows col cols var)
  ;; the value of a variable of a cell, the last which the formats give
  (let loop ((l formats) (val #f))
    (if (null? l) val
        (let* ((f (car l))
               (index (lambda (s n)
                        (with i (and (string? s) (string->number s))
                          (if (and i (< i 0)) (+ n i 1) (or i 0)))))
               (hit? (and (func? f 'cwith 6) (== (list-ref f 5) var)
                          (<= (index (list-ref f 1) rows) row)
                          (>= (index (list-ref f 2) rows) row)
                          (<= (index (list-ref f 3) cols) col)
                          (>= (index (list-ref f 4) cols) col))))
          (loop (cdr l) (if hit? (list-ref f 6) val))))))

(define (tmhtml-static-text? y)
  ;; text, in any markup: a with which sets the mode to text
  (and (tm-func? y 'with)
       (or (let loop ((l (cDr (cdr y))))
             (and (pair? l) (pair? (cdr l))
                  (or (and (== (car l) "mode") (== (cadr l) "text"))
                      (loop (cddr l)))))
           (tmhtml-static-text? (cAr y)))))

(define (tmhtml-static-number? x)
  ;; what is after an htab of a displayed formula is its number (a label of
  ;; any form, as text) when it is text, besides counters and labels
  (with l (if (tm-func? x 'concat) (tm-children x) (list x))
    (and (list-or (map tmhtml-static-text? l))
         (list-and (map (lambda (y)
                          (or (== y "") (tmhtml-static-text? y)
                              (tm-in? y '(assign hidden-binding label))))
                        l)))))

(define (tmhtml-static-cell vars x align width)
  `(h:td (@ (align ,align) ,@(if width `((width ,width)) '()))
         ,@(if (== x "") '()
               (ahash-with tmhtml-env :math #t
                 (ahash-with tmhtml-env :static-cell #t
                   (tmhtml-static-formula*
                    (if (null? vars) x `(with ,@vars ,x))))))))

(define (tmhtml-static-number-cell x)
  `(h:td (@ (align "right"))
         ,@(ahash-with tmhtml-env :math #f
             (ahash-with tmhtml-env :math-display #f
               (tmhtml x)))))

(define (tmhtml-static-cell-body c)
  (if (func? c 'cell 1) (cadr c) c))

(define (tmhtml-static-htab? x)
  (list-or (map (lambda (y) (tm-func? y 'htab)) (tm-concat-children x))))

(define (tmhtml-static-row vars row r rows cols formats numbers?)
  (let* ((cells (map tmhtml-static-cell-body (cdr row)))
         (last (split-htab (cAr cells)))
         (number? (tmhtml-static-number? (cadr last)))
         (cells* (if number? (append (cDr cells) (list (car last))) cells))
         (parts (map (lambda (c)
                       (or (string->number
                            (or (tmhtml-static-cell-format formats 1 rows c cols
                                                           "cell-hpart")
                                "0"))
                           0))
                     (... 1 cols)))
         (sum (apply + parts))
         (cell (lambda (c col)
                 (let* ((h (tmhtml-static-cell-format formats r rows col cols
                                                      "cell-halign"))
                        (p (if (<= col cols) (list-ref parts (- col 1)) 0)))
                   (tmhtml-static-cell
                    vars c
                    ;; the default of TeXmacs is left
                    (cond ((== h "c") "center") ((== h "r") "right")
                          (else "left"))
                    (and (> sum 0) (> p 0)
                         (string-append
                          (number->string (inexact->exact
                                           (round (/ (* 100 p) sum))))
                          "%")))))))
    `(h:tr ,@(map cell cells* (... 1 (length cells*)))
           ,@(if numbers?
                 (list (tmhtml-static-number-cell (if number? (cadr last) "")))
                 '()))))

(define (tmhtml-static-split-htabs x)
  ;; the parts of x between its htab
  (with s (split-htab x)
    (if (tmhtml-static-htab? (cadr s))
        (cons (car s) (tmhtml-static-split-htabs (cadr s)))
        (list (car s) (cadr s)))))

(define (tmhtml-static-display-table x)
  ;; a displayed formula which is laid out on the page, in a table of HTML:
  ;; its parts are formulas and its numbers text; or #f
  (let* ((u (tmhtml-static-unwrap x '()))
         (vars (car u))
         (body (cdr u))
         (tab (tmhtml-static-table-parts body '())))
    (cond (tab
           (let* ((rows (list-filter (cddr tab) (cut func? <> 'row)))
                  (cols (if (null? rows) 0 (length (cdar rows))))
                  (number-of (lambda (r)
                               (cadr (split-htab
                                      (tmhtml-static-cell-body (cAr r))))))
                  (numbers? (list-or (map (lambda (r)
                                            (tmhtml-static-number?
                                             (number-of r)))
                                          rows))))
             (and (> cols 0)
                  `((h:table (@ ,@(with w (tmhtml-static-table-width (car tab))
                                    (if w `((width ,w)) '())))
                             ,@(map (lambda (row r)
                                      (tmhtml-static-row
                                       vars row r (length rows)
                                       cols (car tab) numbers?))
                                    rows (... 1 (length rows))))))))
          ((and (tm-func? body 'concat) (tmhtml-static-htab? body))
           (let* ((parts (tmhtml-static-split-htabs body))
                  (number? (tmhtml-static-number? (cAr parts)))
                  (fs (if number? (cDr parts) parts))
                  (n (length fs))
                  ;; the space of the htab, shared
                  (cell (lambda (f i)
                          (tmhtml-static-cell
                           vars f "center"
                           (string-append
                            (number->string (quotient 100 n)) "%")))))
             `((h:table (@ (width "100%"))
                        (h:tr ,@(map cell fs (... 1 n))
                              ,@(if number?
                                    (list (tmhtml-static-number-cell
                                           (cAr parts)))
                                    '()))))))
          (else #f))))

(define (tmhtml-static-source-tree x)
  ;; the formula, with the tags of the expansion of a buffer as in TeXmacs
  (with y (if (func? x 'with 1) (cadr x) x)
    (cork->html (object->string
                 (tmhtml-unrecord
                  (tmhtml-static-strip-numbering
                   (tmhtml-static-strip-labels y)))))))

(define (tmhtml-static-formula x)
  (or (and (ahash-ref tmhtml-env :math-display)
           (tmhtml-static-display-table x))
      (tmhtml-static-formula* x)))

(define (tmhtml-static-formula* x)
  (with svgs (tmhtml-static-svgs x)
    (cond ((nnull? svgs)
           (append (tmhtml-math-labels x)
                   (list `(h:span (@ (class "tm-math")
                                     ,@(if tmhtml-in-file? '()
                                           `((style ,tmhtml-static-math)))
                                     ;; to read it back
                                     (data-tm ,(tmhtml-static-source-tree x)))
                                  ,@(tmhtml-static-text-layer x)
                                  ,@svgs
                                  ,@(tmhtml-static-source x)))))
          ((!= tmhtml-image-root-string "image")
           (tmhtml-png `(with "mode" "math" ,x)))
          (else (append (tmhtml-math-labels x)
                        (list `(m:math
                                (@ (xmlns "http://www.w3.org/1998/Math/MathML"))
                                ,(texmacs->mathml x tmhtml-env))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Equation arrays
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (tm-concat-children t)
  (if (tm-func? t 'concat)
      (append-map tm-concat-children (tm-children t))
      (list t)))

(define (split-htab r)
  (let* ((a (tm-concat-children r))
         (f (list-find-index a (lambda (x) (tm-func? x 'htab))))
         (al (if f (sublist a 0 f) a))
         (ar (if f (sublist a (+ f 1) (length a)) (list)))
         (rl (apply tmconcat al))
         (rr (apply tmconcat ar)))
    (list rl rr)))

(define (rewrite-eqnarray* t)
  (cond ((tm-atomic? t) t)
        ((tm-func? t 'row 3)
         (let* ((l (tm-ref t 0 0))
                (c (tm-ref t 1 0))
                (r (split-htab (tm-ref t 2 0))))
           `(row (cell (big-math ,l))
                 (cell (big-math ,c))
                 (cell (big-math ,(car r)))
                 (cell ,(cadr r)))))
        (else (cons (tm-label t) (map rewrite-eqnarray* (tm-children t))))))

(tm-define (ext-tmhtml-eqnarray* body)
  (:secure #t)
  (if (tm-func? body 'document 1)
      `(document ,(ext-tmhtml-eqnarray* (tm-ref body 0)))
      (cond ((null? (tm-search body (lambda (x) (tm-func? x 'htab))))
             `(equation* (rcl-table ,body)))
            ((and (tm-func? body 'tformat)
                  (tm-func? (tm-ref body :last) 'table 1)
                  (tm-func? (tm-ref body :last 0) 'row 3))
             (let* ((row (tm-ref body :last 0))
                    (l (tm-ref row 0 0))
                    (c (tm-ref row 1 0))
                    (r (split-htab (tm-ref row 2 0)))
                    (row1 `(row (cell ,l) (cell ,c) (cell ,(car r))))
                    (rcl `(rcl-table (tformat (table ,row1))))
                    (row2 `(row (cell (big-math ,rcl)) (cell ,(cadr r))))
                    (res `(cx-table (tformat (table ,row2)))))
               res))
            (else
              `(rclx-table ,(rewrite-eqnarray* body))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Tags for customized html generation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (tmhtml-append-attribute-sub l var val)
  (cond ((null? l) (list (list var val)))
	((and (list-2? (car l)) (== (caar l) var) (string? (cadar l)))
	 (cons (list var (string-append (cadar l) " " val)) (cdr l)))
	(else (cons (car l) (tmhtml-append-attribute-sub (cdr l) var val)))))

(define (tmhtml-append-attribute t var val)
  (cond ((and (func? t 'h:img) (== var 'style))
         `(class (@ (,var ,val)) ,t))
        ((and (func? t 'h:p 1) (func? (cadr t) 'h:p))
         (tmhtml-append-attribute (cadr t) var val))
        ((and (pair? t) (pair? (cdr t)) (list? t)
              (pair? (cadr t)) (== (caadr t) '@) (list? (cadr t)))
         (with l (tmhtml-append-attribute-sub (cdadr t) var val)
           `(,(car t) (@ ,@l) ,@(cddr t))))
        ((and (pair? t) (list? t))
         `(,(car t) (@ (,var ,val)) ,@(cdr t)))
        ((== var 'class) `(font (@ (,var ,val)) ,t))
        (else `(class (@ (,var ,val)) ,t))))

(define (tmhtml-html-tag l)
  (let* ((s (tmhtml-force-string (car l)))
         (r (tmhtml (cadr l))))
    (list `(,(string->symbol s) ,@r))))

(define (tmhtml-html-attr l)
  (let* ((a (string->symbol (tmhtml-force-string (car l))))
         (v (tmhtml-force-string (cadr l)))
         (r (tmhtml (caddr l))))
    (map (cut tmhtml-append-attribute <> a v) r)))

(define (tmhtml-html-style l)
  (let* ((s (tmhtml-force-string (car l)))
         (r (tmhtml (cadr l))))
    (map (cut tmhtml-append-attribute <> 'style s) r)))

(define (tmhtml-html-class l)
  (let* ((s (tmhtml-force-string (car l)))
         (r (tmhtml (cadr l))))
    (map (cut tmhtml-append-attribute <> 'class s) r)))

(define (tmhtml-html-div-style l)
  (list `(h:div (@ (style ,(tmhtml-force-string (car l))))
		,@(tmhtml (cadr l)))))

(define (tmhtml-html-div-class l)
  (list `(h:div (@ (class ,(tmhtml-force-string (car l))))
		,@(tmhtml (cadr l)))))

(define (tmhtml-html-javascript l)
  (list `(h:script (@ (language "javascript"))
		   ,(tmhtml-force-string (car l)))))

(define (tmhtml-html-javascript-src l)
  (list `(h:script (@ (language "javascript")
		      (src ,(tmhtml-force-string (car l)))))))

(define (tmhtml-html-video l)
  (let* ((dest (tmhtml-safe-url (cork->html (force-string (car l)))))
         (mp4 (string-append dest ".mp4"))
         (ogg (string-append dest ".ogg"))
         (webm (string-append dest ".webm"))
         (swf (string-append dest ".swf"))
         (width (force-string (cadr l)))
         (height (force-string (caddr l))))
    (list `(h:video (@ (width ,width) (height ,height) (controls "controls"))
             (h:source (@ (src ,mp4) (type "video/mp4")))
             (h:source (@ (src ,ogg) (type "video/ogg")))
             (h:source (@ (src ,webm) (type "video/webm")))
             (h:object (@ (data ,mp4) (width ,width) (height ,height))
               (h:embed (@ (src ,swf) (width ,width) (height ,height))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Tmdoc tags
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (tmhtml-make-block content)
  (let* ((l '(h:td
	      (@ (align "left"))
	      (h:img (@ (src "https://www.texmacs.org/Images/tm_gnu1b.png")))))
	 (c `(h:td
	      (@ (align "center") (width "100%"))
	      ,@(tmhtml content)))
	 (r '(h:td
	      (@ (align "right"))
	      (h:img (@ (src "https://www.texmacs.org/Images/tm_gnu2b.png")))))
	 (row `(h:tr ,l ,c ,r)))
    `(h:table (@ (width "100%") (cellspacing "0") (cellpadding "3")) ,row)))

(define (tmhtml-tmdoc-title l)
  (list `(h:div (@ (class "tmdoc-title-1"))
		,(tmhtml-make-block (car l)))))

(define (tmhtml-tmdoc-title* l)
  (list `(h:div (@ (class "tmdoc-title-2"))
		,(tmhtml-make-block (car l)))
	`(h:div (@ (class "tmdoc-navbar")) ,@(tmhtml (cadr l)))))

(define (tmhtml-tmdoc-title** l)
  (list `(h:div (@ (class "tmdoc-navbar")) ,@(tmhtml (car l)))
	`(h:div (@ (class "tmdoc-title-3")) ,(tmhtml-make-block (cadr l)))
	`(h:div (@ (class "tmdoc-navbar")) ,@(tmhtml (caddr l)))))

(define (tmhtml-tmdoc-flag l)
  ;(tmhtml (car l)))
  (list `(h:div (@ (class "tmdoc-flag")) ,@(tmhtml (car l)))))

(define (tmhtml-tmdoc-copyright* l)
  (if (null? l) l
      `(", " ,@(tmhtml (car l)) ,@(tmhtml-tmdoc-copyright* (cdr l)))))

(define (tmhtml-tmdoc-copyright l)
  (with content
      `("&copy;" " " ,@(tmhtml (car l))
	" " ,@(tmhtml (cadr l))
	,@(tmhtml-tmdoc-copyright* (cddr l)))
    (list `(h:div (@ (class "tmdoc-copyright")) ,@content))))

(define (tmhtml-tmdoc-license l)
  (list `(h:div (@ (class "tmdoc-license")) ,@(tmhtml (car l)))))

(define (tmhtml-key l)
  ;; `((h:u (h:tt ,@(tmhtml (tm->stree (tmdoc-key (car l))))))))
  `((h:u (h:tt ,@(tmhtml (car l))))))

(define (tmhtml-tmdoc-bar? y)
  (or (func? y 'h:h1)
      (func? y 'h:h2)
      (and (func? y 'h:div)
	   (nnull? (cdr y))
	   (func? (cadr y) '@ 1)
	   (== (first (cadadr y)) 'class)
	   (string-starts? (second (cadadr y)) "tmdoc"))))

(define (tmhtml-tmdoc-post-sub x)
  ;; FIXME: these rewritings are quite hacky;
  ;; better simplification would be nice...
  (cond ((and (func? x 'h:p) (list-find (cdr x) tmhtml-tmdoc-bar?)) (cdr x))
	((func? x 'h:p)
	 (with r (append-map tmhtml-tmdoc-post-sub (cdr x))
	   (if (== (cdr x) r) (list x) r)))
	(else (list x))))

(define (tmhtml-tmdoc-post body)
  (with r (append-map tmhtml-tmdoc-post-sub body)
    `((h:div (@ (class "tmdoc-body")) ,@r))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Css finalization (breaking around images)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (tmhtml-breaks-post l)
  (cond ((or (null? l) (null? (cdr l))) l)
        ((and (func? (car l) 'h:img)
              (string? (cadr l))
              (not (string-starts? (cadr l) " ")))
         (let* ((s (cadr l))
                (i (string-index s #\space))
                (s1 (if i (substring s 0 i) s))
                (s2 (and i (substring s i (string-length s))))
                (nb `(h:span (@ (class "no-breaks")) ,(car l) ,s1))
                (t (if s2 (cons s2 (cddr l)) (cddr l))))
           (cons nb (tmhtml-breaks-post t))))
        (else (cons (car l) (tmhtml-breaks-post (cdr l))))))

(define (tmhtml-breaks-post* l)
  (let* ((n (length l))
         (h (quotient n 2)))
    (if (<= n 10000) (tmhtml-breaks-post l)
        (append (tmhtml-breaks-post* (sublist l 0 h))
                (tmhtml-breaks-post* (sublist l h n))))))

(define (tmhtml-css-post body)
  (if (pair? body)
      `(,(car body) ,@(tmhtml-breaks-post* (map tmhtml-css-post (cdr body))))
      body))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Main conversion routines
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (tmhtml-list l)
  (append-map tmhtml l))

(define (tmhtml-dispatch htable l)
  (let ((x (logic-ref ,htable (car l))))
    (cond ((not x) #f)
	  ((procedure? x) (x (cdr l)))
	  (else (tmhtml-post-simplify-element
		 (append x (tmhtml-list (cdr l))))))))

(define (tmhtml-implicit-compound l)
  (if (and tmhtml-static-math? (in? (car l) tmhtml-static-bold-tags))
      (tmhtml-static-bold (lambda () (tmhtml-implicit-compound* l)))
      (tmhtml-implicit-compound* l)))

(define (tmhtml-implicit-compound* l)
  (or (tmhtml-dispatch 'tmhtml-stdmarkup% l)
      (and (!= tmhtml-site-version "2")
           (tmhtml-dispatch 'tmhtml-tmdocmarkup% l))
      '()))

(tm-define (tmhtml-root x)
  (ahash-with tmhtml-env :mag "1"
    (ahash-with tmhtml-env :math #f
      (ahash-with tmhtml-env :math-display #f
        (ahash-with tmhtml-env :preformatted #f
          (ahash-with tmhtml-env :verbatim #f
            (ahash-with tmhtml-env :left-margin 0
              (ahash-with tmhtml-env :right-margin 0
                (tmhtml x)))))))))

(define (tmhtml x)
  ;; Main conversion function.
  ;; Takes a TeXmacs tree in Scheme notation and produce a SXML node-set.
  ;; All handler functions have a similar prototype.
  (cond ((and tmhtml-static-math? (ahash-ref tmhtml-env :math))
         (tmhtml-static-formula x))
        ((and tmhtml-mathjax? (ahash-ref tmhtml-env :math))
         (append (tmhtml-math-labels x)
                 (tmhtml-mathjax-formula x)))
        ((and tmhtml-mathml? (ahash-ref tmhtml-env :math))
	 `(,@(tmhtml-math-labels x)
           (m:math (@ (xmlns "http://www.w3.org/1998/Math/MathML"))
		   ,(texmacs->mathml x tmhtml-env))))
	((and tmhtml-images? (ahash-ref tmhtml-env :math)
              (!= tmhtml-image-root-string "image"))
         (tmhtml-png `(with "mode" "math" ,x)))
	((string? x)
	 (if (string-null? x) '() (tmhtml-text x))) ; non-verbatim string nodes
	(else (or (tmhtml-dispatch 'tmhtml-primitives% x)
		  (tmhtml-implicit-compound x)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Dispatching
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(logic-dispatcher tmhtml-primitives%
  (document tmhtml-document)
  (para tmhtml-paragraph)
  (surround tmhtml-surround)
  (concat tmhtml-concat)
  (rigid tmhtml-id)
  (hgroup tmhtml-id)
  (hidden tmhtml-hidden)
  (freeze tmhtml-id)
  (unfreeze tmhtml-id)
  (hspace tmhtml-hspace)
  (vspace* tmhtml-vspace)
  (vspace tmhtml-vspace)
  (space tmhtml-hspace)
  (htab tmhtml-hspace)
  (split tmhtml-noop)
  (move tmhtml-move)
  (shift tmhtml-move)
  (resize tmhtml-resize)
  (clipped tmhtml-resize)
  (repeat tmhtml-repeat)
  (repeat* tmhtml-repeat)
  (float tmhtml-float)
  (underline tmhtml-underline)
  (overline tmhtml-overline)
  (strike-through tmhtml-strike-through)
  (deleted tmhtml-deleted)
  (marked tmhtml-marked)
  (fill-out tmhtml-fill-out)
  (datoms tmhtml-datoms)
  (dlines tmhtml-datoms)
  (dpages tmhtml-datoms)
  (dbox tmhtml-datoms)
  
  (with-limits tmhtml-noop)
  (line-break tmhtml-noop)
  (new-line tmhtml-new-line)
  (line-sep tmhtml-noop)
  (next-line tmhtml-next-line)
  (no-break tmhtml-noop)
  (no-indent tmhtml-noop)
  (yes-indent tmhtml-noop)
  (no-indent* tmhtml-noop)
  (yes-indent* tmhtml-noop)
  (page-break* tmhtml-noop)
  (page-break tmhtml-noop)
  (no-page-break* tmhtml-noop)
  (no-page-break tmhtml-noop)
  (no-break-here* tmhtml-noop)
  (no-break-here tmhtml-noop)
  (no-break-start tmhtml-noop)
  (no-break-end tmhtml-noop)
  (new-page* tmhtml-noop)
  (new-page tmhtml-noop)
  (new-dpage* tmhtml-noop)
  (new-dpage tmhtml-noop)

  ((:or around around* big-around) tmhtml-concat)
  (left tmhtml-id)
  (mid tmhtml-id)
  (right tmhtml-id)
  (big tmhtml-big)
  (lprime tmhtml-id)
  (rprime tmhtml-id)
  (below tmhtml-below)
  (above tmhtml-above)
  (lsub tmhtml-sub)
  (lsup tmhtml-sup)
  (rsub tmhtml-sub)
  (rsup tmhtml-sup)
  (rsubsup tmhtml-subsup)
  (frac tmhtml-frac)
  (sqrt tmhtml-sqrt)
  (wide tmhtml-wide)
  (neg tmhtml-neg)
  (tree tmhtml-tree)
  (syntax tmhtml-syntax)
  ((:or old-matrix old-table old-mosaic old-mosaic-item)
   tmhtml-noop)
  (table tmhtml-table)
  (tformat tmhtml-tformat)
  ((:or twith cwith tmarker row cell sub-table) tmhtml-noop)

  (assign tmhtml-noop)
  (with tmhtml-with)
  (provides tmhtml-noop)
  ((:or value quote-value) tmhtml-compound)
  ((:or macro drd-props arg quote-arg) tmhtml-noop)
  (compound tmhtml-compound)
  ((:or xmacro get-label get-arity map-args eval-args) tmhtml-noop)
  (mark tmhtml-mark)
  (eval tmhtml-noop)
  ((:or if if* case while for-each extern) tmhtml-noop)
  (include tmhtml-include)
  (use-package tmhtml-noop)

  ((:or or xor and not plus minus times over div mod merge length range
	number date translate is-tuple look-up equal unequal less lesseq
	greater greatereq if case while extern authorize)
   tmhtml-noop)

  ((:or style-with style-with* style-only style-only*
	active active* inactive inactive* rewrite-inactive) tmhtml-noop)
  (inline-tag tmhtml-inline-tag)
  (open-tag tmhtml-open-tag)
  (middle-tag tmhtml-middle-tag)
  (close-tag tmhtml-close-tag)
  (symbol tmhtml-noop)
  (latex tmhtml-noop)
  (hybrid tmhtml-noop)

  (locus tmhtml-datoms)
  (label tmhtml-label)
  (reference tmhtml-noop)
  (pageref tmhtml-noop)
  (hlink tmhtml-hyperlink)
  (action tmhtml-action)
  (write tmhtml-noop)
  
  ((:or tuple collection associate) tmhtml-noop)
  (specific tmhtml-specific)

  (graphics tmhtml-graphics)
  ((:or point line arc bezier) tmhtml-noop)
  (image tmhtml-image)

  (ornament tmhtml-ornament)

  (format tmhtml-noop)
  ((:or tag meaning) tmhtml-noop)

  (vgroup tmhtml-id)
  ((:or switch fold exclusive progressive superposed) tmhtml-noop)
  ((:or mouse-over-balloon mouse-over-balloon*
        hover-balloon hover-balloon*
        popup-balloon popup-balloon*) tmhtml-balloon)
  
  (!file tmhtml-file))

(logic-table tmhtml-stdmarkup%
  ;; special auxiliary tags
  (!left ,tmhtml-align-left)
  (!middle ,tmhtml-align-middle)
  (!right ,tmhtml-align-right)
  ;; Sectioning
  (chapter-title (h:h1))
  (section-title (h:h2))
  (subsection-title (h:h3))
  (subsubsection-title (h:h4))
  (paragraph-title (h:h5))
  (subparagraph-title (h:h6))
  ;; Lists
  ((:or itemize itemize-minus itemize-dot itemize-arrow)
   ,tmhtml-itemize)
  ((:or enumerate enumerate-numeric enumerate-roman enumerate-Roman
	enumerate-alpha enumerate-Alpha)
   ,tmhtml-enumerate)
  ((:or description description-compact description-dash
	description-aligned description-long description-paragraphs)
   ,tmhtml-description)
  (item* (h:dt))
  (!item ,tmhtml-post-item)
  ;; Phrase elements
  (strong (h:strong))
  (em (h:em))
  (dfn (h:dfn))
  (code* ,tmhtml-code*)
  (samp (h:samp)) ; WARNING: semantic documentation does not match HTML4
  (kbd (h:kbd))
  (var (h:var))
  (abbr (h:abbr))
  (acronym (h:acronym))
  (verbatim ,tmhtml-verbatim)
  (code ,tmhtml-verbatim)
  (nbsp ,(lambda x '("&nbsp;")))
  ;; Presentation
  (tt (h:tt))
  (hrule (h:hr))
  ;; Names
  (TeXmacs ,(lambda x '("TeXmacs")))
  (TeX ,(lambda x '("TeX")))
  (LaTeX ,(lambda x '("LaTeX")))
  ;; additional tags
  (shown ,tmhtml-shown)
  (hidden-title ,tmhtml-noop)
  (doc-title-block ,tmhtml-doc-title-block)
  (equation* ,tmhtml-equation*)
  (equation-lab ,tmhtml-equation-lab)
  (html-equation-lab ,tmhtml-equation-lab)
  (equations-base ,tmhtml-equation*)
  (html-display ,tmhtml-equation*)
  (html-equations-base ,tmhtml-equation*)
  (wide-float ,tmhtml-float)
  (draw-over ,tmhtml-draw-over)
  (draw-under ,tmhtml-draw-under)
  ;; tags for customized html generation
  (html-tag ,tmhtml-html-tag)
  (html-attr ,tmhtml-html-attr)
  (html-div-style ,tmhtml-html-div-style)
  (html-div-class ,tmhtml-html-div-class)
  (html-style ,tmhtml-html-style)
  (html-class ,tmhtml-html-class)
  (html-javascript ,tmhtml-html-javascript)
  (html-javascript-src ,tmhtml-html-javascript-src)
  (html-video ,tmhtml-html-video)
  ;; tmdoc tags
  (web-title ,tmhtml-noop)
  (hyper-link ,tmhtml-hyperlink))

(logic-table tmhtml-tmdocmarkup%
  ;; deprecated direct exports of tmdoc tags
  (tmdoc-title ,tmhtml-tmdoc-title)
  (tmdoc-title* ,tmhtml-tmdoc-title*)
  (tmdoc-title** ,tmhtml-tmdoc-title**)
  (tmdoc-flag ,tmhtml-tmdoc-flag)
  (tmdoc-copyright ,tmhtml-tmdoc-copyright)
  (tmdoc-license ,tmhtml-tmdoc-license))

;; (name (h:name)) ; not in HTML4
;; (person (h:person)))) ; not in HTML4

(logic-table tmhtml-with-cmd%
  ("mode" ,tmhtml-with-mode)
  ("math-display" ,tmhtml-with-math-display)
  ("color" ,tmhtml-with-color)
  ("font-size" ,tmhtml-with-font-size)
  ("par-left" ,tmhtml-with-par-left)
  ("par-right" ,tmhtml-with-par-right)
  ("par-first" ,tmhtml-with-par-first)
  ("par-par-sep" ,tmhtml-with-par-par-sep)
  (("ornament-hpadding")      (:ornament-hpadding
                                "padding-left" "padding-right"
                                ,tmlength->htmllength))
  (("ornament-vpadding")      (:ornament-vpadding
                                "padding-top"  "padding-bottom"
                                ,tmlength->htmllength))
  (("ornament-border")        (:ornament-border
                                "border-width"     ,tmlength->htmllength))
  (("ornament-shape")         (:ornament-shape
                                "border-radius"    ,tmshape->htmllength))
  (("ornament-color")         (:ornament-color
                                "background-color" ,tmcolor->htmlcolor))
  (("ornament-shadow-color")  (:ornament-shadow-color
                                "border-bottom-color" "border-right-color"
                                ,tmcolor->htmlcolor))
  (("ornament-sunny-color")   (:ornament-sunny-color
                                "border-left-color" "border-top-color"
                                ,tmcolor->htmlcolor))
  ;;(("ornament-extra-color")   :ornament-extra-color ""  ,tmcolor->htmlcolor))
  ;;(("ornament-swell")        :ornament-swell "" ,identity))
  ;;(("ornament-title-style")   :ornament-title-style "" ,identity))
  (("font-family" "tt") (h:tt))
  (("font-family" "ss") (h:class (@ (style "font-family: sans-serif"))))
  (("font-series" "bold") (h:b))
  (("font-shape" "italic") (h:i))
  (("font" "roman") (h:class (@ (style "font-family: Times New Roman"))))
  (("font" "times") (h:class (@ (style "font-family: Times New Roman"))))
  (("font" "helvetica") (h:class (@ (style "font-family: Helvetica"))))
  (("font" "courier") (h:class (@ (style "font-family: Coutier"))))
  (("math-font" "cal") (h:class (@ (style "font-family: Flemish Script"))))
  (("math-font" "frak") (h:class (@ (style "font-family: Bernhard Modern"))))
  (("font-series" "medium") (h:class (@ (style "font-weight: normal"))))
  (("font-shape" "right") (h:class (@ (style "font-style: normal"))))
  (("font-shape" "small-caps")
   (h:class (@ (style "font-variant: small-caps")))))

(logic-table tmhtml-with-cmd% ; deprecated
  (("par-mode" "left") (h:div (@ (align "left"))))
  (("par-mode" "justify") (h:div (@ (align "justify"))))
  (("par-mode" "center") (h:center)))

(logic-table tmhtml-with-cmd% ; netscape4
  (("par-columns" "1") (h:multicol (@ (cols "1"))))
  (("par-columns" "2") (h:multicol (@ (cols "2"))))
  (("par-columns" "3") (h:multicol (@ (cols "3"))))
  (("par-columns" "4") (h:multicol (@ (cols "4"))))
  (("par-columns" "5") (h:multicol (@ (cols "5")))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Interface
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (texmacs->html x opts)
  (if (tmfile? x)
      (let* ((body (tmfile-extract x 'body))
	     (style* (tmfile-extract x 'style))
	     (style (if (list? style*) style* (list style*)))
	     (lan (tmfile-language x))
	     (doc (list '!file body style lan
                        (url->string (get-texmacs-path)))))
	(texmacs->html doc opts))
      (begin
	(tmhtml-initialize opts)
	(set! tmhtml-keep-envs?
	      (and tmhtml-images? (not tmhtml-mathjax?) (not tmhtml-mathml?)))
	(set! x (if tmhtml-static-math? (tmhtml-record x) (tmhtml-unrecord x)))
	(if (func? x '!file)
	    (tmhtml-finalize-document (tmhtml-root x))
	    (with l (tmhtml-root x)
	      ;; the glyphs of static formulas once, after them
	      (tmhtml-finalize-selection
	       (if tmhtml-static-math? (append l (tmhtml-static-defs)) l)))))))
