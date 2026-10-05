
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : wallet-base.scm
;; DESCRIPTION : wallet
;; COPYRIGHT   : (C) 2015  Gregoire Lecerf
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (security wallet wallet-base)
  (:use (security gpg gpg-wallet) (security wallet web-wallet)))

;; In a web browser the wallet is that of web-wallet.scm (the cryptography of
;; the browser instead of GnuPG); its opening and its changes are
;; asynchronous, and are done by the dialogues of wallet-menu.scm, so that
;; wallet-turn-on, wallet-initialize, wallet-reinitialize and
;; wallet-correct-passphrase? are not used there.

;; So far portable implementation is based on GnuPG
(tm-define (supports-wallet?)
  (:synopsis "Tells if the platform provides a wallet implementation")
  (if (web-wallet?) (web-wallet-supported?) (supports-gpg?)))

;; What wants to know when the wallet is turned on (the plug-ins which take
;; their keys there)
(define wallet-on-hooks (list))

(tm-define (wallet-add-on-hook f)
  (:synopsis "Call @f each time the wallet is turned on")
  (set! wallet-on-hooks (append wallet-on-hooks (list f))))

(tm-define (wallet-notify-on)
  (for-each (lambda (f) (f)) wallet-on-hooks))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Initialization
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (wallet-initialized?)
  (:synopsis "Tells if wallet has been initialized") 
  (if (web-wallet?) (web-wallet-initialized?) (gpg-wallet-initialized?)))

(tm-define (wallet-initialize passphrase)
  (:synopsis "Initialize a new wallet") 
  (:interactive #t)
  (:argument passphrase "password" "Wallet passphrase")
  (and (supports-wallet?) (gpg-wallet-initialize passphrase)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Reinitialize
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (wallet-reinitialize old-passphrase new-passphrase)
  (:synopsis "Reinitialize wallet") 
  (:interactive #t)
  (:argument old-passphrase "password" "Current wallet passphrase")
  (:argument new-passphrase "password" "New wallet passphrase")
  (gpg-wallet-reinitialize old-passphrase new-passphrase))

(tm-define (wallet-destroy)
  (:synopsis "Destroy wallet") 
  (if (web-wallet?) (web-wallet-destroy) (gpg-wallet-destroy)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Status
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (wallet-on?)
  (:synopsis "Tells if wallet is turned on") 
  (if (web-wallet?) (web-wallet-on?) (gpg-wallet-on?)))

(tm-define (wallet-off?)
  (:synopsis "Tells if wallet is turned off") 
  (not (wallet-on?)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Persistent wallet status
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define wallet-persistent-status "off")

(define (notify-wallet-persistent-status var val)
  (set! wallet-persistent-status val))

(define-preferences
  ("wallet persistent status" "off" notify-wallet-persistent-status))

(tm-define (wallet-persistent-status-on?)
  (== (get-preference "wallet persistent status") "on"))

(tm-define (wallet-persistent-status-off?)
  (not (wallet-persistent-status-on?)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Check passphrase
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (wallet-correct-passphrase? passphrase)
  (:synopsis "Tells if @passphrase is correct") 
  (gpg-wallet-correct-passphrase? passphrase))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Turn on/off
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (wallet-turn-on passphrase)
  (:synopsis "Turn wallet on using @passphrase") 
  (:interactive #t)
  (:argument passphrase "password" "Wallet passphrase")
  (and (gpg-wallet-turn-on passphrase)
       (begin
         (set-preference "wallet persistent status" "on")
         (wallet-notify-on)
         #t)))

(tm-define (wallet-turn-off)
  (:synopsis "Turn wallet off") 
  (if (web-wallet?) (web-wallet-lock) (gpg-wallet-turn-off))
  (set-preference "wallet persistent status" "off"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Set, get, delete entries
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (wallet-set key val)
  (:synopsis "Insert binding @key ~> @val into wallet")
  (if (web-wallet?) (web-wallet-set key val) (gpg-wallet-set key val)))

(tm-define (wallet-get key)
  (:synopsis "Get value for @key from the wallet")
  (if (web-wallet?) (web-wallet-get key) (gpg-wallet-get key)))

(tm-define (wallet-delete key)
  (:synopsis "Delete binding for @key in the wallet")
  (if (web-wallet?) (web-wallet-delete key) (gpg-wallet-delete key)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; List entries
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (wallet-entries)
  (:synopsis "List all entries of the wallet")
  (if (web-wallet?) (web-wallet-entries) (gpg-wallet-entries)))
