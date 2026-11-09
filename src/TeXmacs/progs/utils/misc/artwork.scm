
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : artwork.scm
;; DESCRIPTION : automatically obtain artwork from the web or a local thumbnail
;; COPYRIGHT   : (C) 2018  Joris van der Hoeven
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (utils misc artwork))

;;(define up-to-date-artwork? #t)
;;(define (delayed-update-artwork)
;;  (when up-to-date-artwork?
;;    (set! up-to-date-artwork? #f)
;;    (delayed
;;      (:idle 10000)
;;      (set! up-to-date-artwork? #t)
;;      (picture-cache-reset)
;;      (update-all-buffers))))

(tmfs-format-handler (artwork name)
  (url-format name))

(define (artwork-page-site)
  ;; in the browser: the folder artwork next to the page, where the site
  ;; has a copy of the artwork (misc/wasm/Makefile): texmacs.org does not
  ;; let a page read its pictures (CORS, #322); "" elsewhere (and in the
  ;; build for node, which has no page and fetches with the programs)
  (if (not (defined? 'web-javascript)) ""
      (with r (web-javascript
               (string-append
                "typeof location !== 'undefined' && "
                "/^https?:$/.test (location.protocol) ? "
                "new URL ('artwork', location.href).href : ''"))
        (if (string-starts? r "http") r ""))))

(tmfs-load-handler (artwork name)
  (let* ((s (string-append "thumbnail-" (url->string (url-tail name))))
	 (t (url-append (url-head name) s))
	 (thumbnail (url-append "$TEXMACS_PATH/misc" t))
         (site (artwork-page-site))
	 (src (if (== site "")
                  (url-append "https://www.texmacs.org/artwork" name)
                  (url-append site name)))
	 (dest (url-append "$TEXMACS_HOME_PATH/misc" name)))
    (cond ((url-exists? dest)
           (string-load dest))
          ((!= site "")
           ;; (not kept in the home directory, which the browser stores and
           ;; backs up: the browser keeps the picture in its cache)
           (with tmp (url-concretize* src)
             (cond ((not (url-none? tmp)) (string-load tmp))
                   ((url-exists? thumbnail) (string-load thumbnail))
                   (else ""))))
          (else
           (with tmp (url-concretize* src)
             (cond ((not (url-none? tmp))
                    (system-mkdir (url-head dest))
                    (system-move tmp dest)
                    (string-load dest))
                   ((url-exists? thumbnail)
                    (string-load thumbnail))
                   (else "")))))))
