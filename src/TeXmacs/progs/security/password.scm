
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : password.scm
;; DESCRIPTION : TeXmacs
;; COPYRIGHT   : (C) 2025 Robin Wils
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (security password))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Generation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define lower-charset "abcdefghijklmnopqrstuvwxyz")
(define upper-charset "ABCDEFGHIJKLMNOPQRSTUVWXYZ")
(define digits-charset "0123456789")
(define symbols-charset "!@#$%^&*()_+-=[]{}|;:,.?")
(define all-charset (string-append lower-charset upper-charset digits-charset symbols-charset))

(define (urandom-number n)
  ;; Uniform random integer in [0..n-1] from /dev/urandom, or #f
  (and (url-exists? "/dev/urandom")
       (catch #t
         (lambda ()
           (call-with-input-file "/dev/urandom"
             (lambda (p)
               ;; Read raw bytes: Guile 1.8 and S7 ports are byte ports,
               ;; Guile 2+ ports decode text unless set to latin-1
               (when (defined? 'set-port-encoding!)
                 (set-port-encoding! p "ISO-8859-1"))
               (let* ((m 4294967296)
                      (limit (- m (modulo m n)))
                      (b (lambda ()
                           (with c (char->integer (read-char p))
                             (if (< c 256) c (error "not a byte"))))))
                 (let loop ()
                   (let ((x (+ (b) (* 256 (+ (b) (* 256 (+ (b) (* 256 (b)))))))))
                     (if (< x limit) (modulo x n) (loop))))))))
         (lambda args #f))))

(define (rnd n)
  (if (supports-gnutls?)
      (gnutls-random-number n)
      (or (urandom-number n) (random n))))

(define (string-shuffle! s)
  (let ((n (string-length s)))
    (let loop ((i (- n 1)))
      (if (<= i 0)
          s
          (let* ((j (rnd (+ i 1)))        ; j in [0..i]
                 (ci (string-ref s i))
                 (cj (string-ref s j)))
            (string-set! s i cj)
            (string-set! s j ci)
            (loop (- i 1)))))))

(define (random-char charset)
  (let* ((n (string-length charset))
         (i (rnd n)))
    (string-ref charset i)))

(define (generate-password-default n)
  "Generate password with at least 1 lower & upper char + digit + symbol"
  (let* ((pwd
           (string-append
             (string (random-char lower-charset))
             (string (random-char upper-charset))
             (string (random-char digits-charset))
             (string (random-char symbols-charset))
             (with s ""
                   (do ((i 0 (+ i 1)))
                     ((= i (- n 4)) s)
                     (set! s (string-append
                               s (string (random-char all-charset)))))))))
    (string-shuffle! pwd)
    pwd))

(tm-define (generate-password n) (generate-password-default n))
(tm-define (generate-salt)       (gnutls-generate-salt))
