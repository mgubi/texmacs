
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : csl-fixtures.scm
;; DESCRIPTION : running the fixtures of the test suite of CSL
;; COPYRIGHT   : (C) 2026  Massimiliano Gubinelli
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; The fixtures are the files processor-tests/humans/*.txt of
;; https://github.com/citation-style-language/test-suite. Each of them holds
;; a style, items in CSL-JSON, a mode (citation or bibliography) and the
;; expected result. They are not distributed with TeXmacs.

(texmacs-module (csl csl-fixtures)
  (:use (csl csl-utils) (csl csl-style) (csl csl-data) (csl csl-process)
        (csl csl-output)))

(define (section-name line)
  ;; ">>===== MODE =====>>" -> "MODE"
  (apply string-append
         (list-filter (map string (string->list line))
                      (lambda (c) (nin? c '(">" "<" "=" " "))))))

(define (parse-fixture s)
  ;; returns an association list from the names of the sections to texts
  (let loop ((l (string-tokenize-by-char s #\newline)) (name #f) (acc '())
             (r '()))
    (cond ((null? l) r)
          ((and (not name) (string-starts? (car l) ">>="))
           (loop (cdr l) (section-name (car l)) '() r))
          ((and name (string-starts? (car l) "<<="))
           (loop (cdr l) #f '()
                 (cons (cons name (string-recompose (reverse acc) "\n")) r)))
          (name (loop (cdr l) name (cons (car l) acc) r))
          (else (loop (cdr l) name acc r)))))

(define (json-attrs t)
  (if (not (func? t 'attr)) '()
      (let loop ((l (cdr t)) (r '()))
        (if (or (null? l) (null? (cdr l))) (reverse r)
            (loop (cddr l) (cons (cons (car l) (cadr l)) r))))))

(define (json->cite t)
  (let* ((a (json-attrs t))
         (get (lambda (k) (with p (assoc k a)
                            (and p (string? (cdr p)) (!= (cdr p) "")
                                 (cdr p)))))
         (flag (lambda (k) (in? (get k) '("true" "1"))))
         (pos (get "position")))
    (list-filter
     (list (cons 'id (utf8->cork (or (get "id") "")))
           (and (get "locator") (cons 'locator (utf8->cork (get "locator"))))
           (and (get "label") (cons 'label (get "label")))
           (and (get "prefix")
                (cons 'prefix (csl-parse-markup (get "prefix"))))
           (and (get "suffix")
                (cons 'suffix (csl-parse-markup (get "suffix"))))
           (and (flag "suppress-author") (cons 'suppress-author #t))
           (and (flag "author-only") (cons 'author-only #t))
           (and (flag "near-note") (cons 'near-note #t))
           (and pos (cons 'position
                          (cond ((== pos "1") 'subsequent)
                                ((== pos "2") 'ibid)
                                ((== pos "3") 'ibid-with-locator)
                                (else 'first)))))
     identity)))

(define (squeeze s)
  ;; the blanks around tags and at the ends of lines do not matter
  (let* ((lines (map string-trim-both (string-tokenize-by-char s #\newline)))
         (lines* (list-filter lines (lambda (x) (!= x "")))))
    (string-recompose lines* "")))

(define (run-bibliography proc)
  (let* ((locale (csl-processor-locale proc))
         (entry (lambda (e)
                  (string-append
                   "<div class=\"csl-entry\">"
                   (if (caddr e)
                       (string-append
                        "<div class=\"csl-left-margin\">"
                        (csl->html (caddr e) locale)
                        "</div><div class=\"csl-right-inline\">"
                        (csl->html (cadddr e) locale) "</div>")
                       (csl->html (cadddr e) locale))
                   "</div>"))))
    (string-append "<div class=\"csl-bib-body\">"
                   (apply string-append (map entry (csl-bibliography proc)))
                   "</div>")))

(define (run-citations proc clusters)
  (with locale (csl-processor-locale proc)
    (string-recompose
     (map (lambda (cites) (csl->html (csl-citation proc cites) locale))
          clusters)
     "\n")))

(tm-define (csl-run-fixture u)
  (:synopsis "Run the fixture in the file @u: pass, skip or (fail want got)")
  (let* ((sections (parse-fixture (string-load u)))
         (get (lambda (k) (with p (assoc k sections) (and p (cdr p)))))
         (mode (string-trim-both (or (get "MODE") "")))
         (style (and (get "CSL") (csl-style-from-string "fixture" (get "CSL"))))
         (items (and (get "INPUT") (csl-json->items (get "INPUT")))))
    (cond ((or (not style) (not items)) 'skip)
          ((or (get "CITATIONS") (get "BIBENTRIES") (get "BIBSECTION")
               (get "ABBREVIATIONS"))
           'skip)
          ((nin? mode '("citation" "bibliography")) 'skip)
          (else
            (csl-forget-styles)
            (let* ((proc (csl-make-processor style #f items))
                   (clusters
                    (if (get "CITATION-ITEMS")
                        (with t (tree->stree (json->tree (get "CITATION-ITEMS")))
                          (map (lambda (c) (map json->cite (cdr c))) (cdr t)))
                        (list (map (lambda (i) (list (cons 'id (csl-item-id i))))
                                   items))))
                   (want (get "RESULT"))
                   (got (if (== mode "citation") (run-citations proc clusters)
                            (run-bibliography proc)))
                   (same? (if (== mode "citation")
                              (== (string-trim-both want)
                                  (string-trim-both got))
                              (== (squeeze want) (squeeze got)))))
              (if same? 'pass (list 'fail want got)))))))

(tm-define (csl-run-fixtures dir report)
  (:synopsis "Run the fixtures of @dir and write the results to @report")
  (let* ((names (map url->string
                     (url-read-directory dir "*.txt")))
         (pass '()) (fail '()) (skip '()) (err '()) (out '()))
    (for (name (csl-sort names string<?))
      (let* ((u (string->url name))
             (short (url->string (url-tail u)))
             (r (catch #t (lambda () (csl-run-fixture u))
                       (lambda args (list 'error args)))))
        (cond ((== r 'pass) (set! pass (cons short pass)))
              ((== r 'skip) (set! skip (cons short skip)))
              ((== (car r) 'error)
               (set! err (cons short err))
               (set! out (cons (string-append
                                "ERROR " short "\n"
                                (object->string (cadr r)) "\n\n")
                               out)))
              (else
                (set! fail (cons short fail))
                (set! out (cons (string-append
                                 "FAIL " short "\n--- want\n" (cadr r)
                                 "\n--- got\n" (caddr r) "\n\n")
                                out))))))
    (string-save
     (string-append
      "pass " (number->string (length pass))
      ", fail " (number->string (length fail))
      ", error " (number->string (length err))
      ", skip " (number->string (length skip)) "\n\n"
      "PASSED\n" (string-recompose (reverse pass) "\n") "\n\n"
      (apply string-append (reverse out)))
     report)
    (list (length pass) (length fail) (length err) (length skip))))
