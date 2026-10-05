
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : web-wallet.scm
;; DESCRIPTION : the wallet in a web browser
;; COPYRIGHT   : (C) 2026  Massimiliano Gubinelli
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; In a browser the wallet cannot be encrypted by GnuPG: the cryptography of
;; the browser does it (misc/wasm/wallet.js, tmWallet), with a passphrase or
;; a passkey. The table of the wallet is here while the wallet is on; the
;; file holds it encrypted. The work of the browser is asynchronous: the
;; functions which open or change the wallet take a procedure, called with
;; #t or #f and a text (the reason of a failure) when it is done.

(texmacs-module (security wallet web-wallet))

(tm-define (web-wallet?)
  (defined? 'web-javascript))

(define (js-quote s)
  (string-append
   "\""
   (string-replace
    (string-replace
     (string-replace
      (string-replace s "\\" "\\\\")
      "\"" "\\\"")
     "\n" "\\n")
    "\r" "\\r")
   "\""))

(define (wallet-js . l)
  (web-javascript (apply string-append "tmWallet." l)))

(define web-wallet-path-set? #f)
(define (web-wallet-js . l)
  (when (not web-wallet-path-set?)
    (set! web-wallet-path-set? #t)
    (wallet-js "setPath ("
               (js-quote (string-append
                          (url->system (url-concretize "$TEXMACS_HOME_PATH"))
                          "/system/wallet/browser-wallet.json"))
               ")"))
  (apply wallet-js l))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The answers of the browser
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define web-wallet-requests (make-ahash-table))
(define web-wallet-next 0)

(define (web-wallet-request cb call . args)
  (set! web-wallet-next (+ web-wallet-next 1))
  (ahash-set! web-wallet-requests web-wallet-next cb)
  (web-wallet-js call " (" (number->string web-wallet-next)
                 (apply string-append
                        (map (lambda (a) (string-append ", " (js-quote a)))
                             args))
                 ")"))

(tm-define (web-wallet-answer id ok? text)
  (with cb (ahash-ref web-wallet-requests id)
    (ahash-remove! web-wallet-requests id)
    (when cb (cb ok? text))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; State
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define web-wallet-table "") ; an ahash table while the wallet is on

(tm-define (web-wallet-supported?)
  (== (web-wallet-js "supported ()") "true"))

(tm-define (web-wallet-passkey-supported?)
  (== (web-wallet-js "passkeySupported ()") "true"))

(tm-define (web-wallet-initialized?)
  (!= (web-wallet-js "status ()") "none"))

(tm-define (web-wallet-has-passkey?)
  (string-ends? (web-wallet-js "status ()") "passkey"))

(tm-define (web-wallet-on?)
  (nstring? web-wallet-table))

(define (web-wallet-opened text)
  (with l (catch #t (lambda () (string->object text)) (lambda args '()))
    (set! web-wallet-table (list->ahash-table (if (list? l) l '())))))

(define (web-wallet-save)
  (web-wallet-js "save ("
                 (js-quote (object->string
                            (ahash-table->list web-wallet-table)))
                 ")"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Opening, closing, changing
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (opening cb)
  (lambda (ok? text)
    (when ok? (web-wallet-opened text))
    (cb ok? (if ok? "" text))))

(tm-define (web-wallet-create passphrase cb)
  (web-wallet-request (opening cb) "create" passphrase "()"))

(tm-define (web-wallet-unlock passphrase cb)
  (web-wallet-request (opening cb) "unlock" passphrase))

(tm-define (web-wallet-unlock-passkey cb)
  (web-wallet-request (opening cb) "unlockPasskey"))

(tm-define (web-wallet-lock)
  (set! web-wallet-table "")
  (web-wallet-js "lock ()")
  #t)

(tm-define (web-wallet-change-passphrase passphrase cb)
  (web-wallet-request cb "changePassphrase" passphrase))

(tm-define (web-wallet-add-passkey cb)
  (web-wallet-request cb "addPasskey"))

(tm-define (web-wallet-remove-passkey)
  (web-wallet-js "removePasskey ()")
  #t)

(tm-define (web-wallet-destroy)
  (set! web-wallet-table "")
  (web-wallet-js "destroy ()")
  #t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Entries
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (web-wallet-set key val)
  (and (web-wallet-on?)
       (begin
         (ahash-set! web-wallet-table key val)
         (web-wallet-save)
         #t)))

(tm-define (web-wallet-get key)
  (and (web-wallet-on?)
       (ahash-ref web-wallet-table key)))

(tm-define (web-wallet-delete key)
  (and (web-wallet-on?)
       (begin
         (ahash-remove! web-wallet-table key)
         (web-wallet-save)
         #t)))

(tm-define (web-wallet-entries)
  (and (web-wallet-on?)
       (ahash-table->list web-wallet-table)))
