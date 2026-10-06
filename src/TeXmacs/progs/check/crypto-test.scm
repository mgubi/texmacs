;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : crypto-test.scm
;; DESCRIPTION : tests of hashes, random numbers, passwords and encryption
;; COPYRIGHT   : (C) 2026  Massimiliano Gubinelli
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; The cryptography of TeXmacs:
;;
;;   - the glue: encode-base64 and decode-base64 (src/Data/String/base64.cpp),
;;     tree-hash, the 64 bit hash which names the entries of the tree cache
;;     (src/Data/Tree/tree_cache.cpp; not a cryptographic hash), and, when
;;     TeXmacs is built with GnuTLS (supports-gnutls?), gnutls-random-number,
;;     gnutls-generate-salt and hash-password-pbkdf2 (PBKDF2-HMAC-SHA256,
;;     600000 iterations, src/Plugins/Gnutls/gnutls.cpp). There is no SHA or
;;     MD5 function in the glue;
;;   - security/password.scm: generate-password and generate-salt;
;;   - server/server-authentication.scm: the passwords which the server
;;     stores, in clear, as sha256 or sha512 crypt strings (made by the
;;     openssl command) or as PBKDF2 hashes (GnuTLS);
;;   - security/gpg: GnuPG through the gpg command: key listing, encryption
;;     to recipients, encryption with a passphrase, encrypted files, the
;;     encrypted blocks of documents (gpg-encrypted, gpg-passphrase-encrypted
;;     and their -block versions) and encrypted documents (the encryption
;;     variable of the initial environment, tree-export-encrypted). There is
;;     no signing.
;;
;; The functions of gpg-base.scm take an optional GnuPG home directory: the
;; checks give them a temporary one, made for the suite with a throwaway key
;; without passphrase, and removed at the end; the keyring of the user and
;; GNUPGHOME are never used. The gpg-agent.conf of that directory disables
;; the passphrase cache and replaces pinentry by a program which fails, so
;; that a passphrase which gpg would ask for makes the command fail instead
;; of hanging. The functions of gpg-edit.scm use the GnuPG directory of the
;; TeXmacs home (TEXMACS_HOME_PATH/users/<user>/gnupg), which the checks
;; use only when the TeXmacs home is not ~/.TeXmacs (tests/scheme/check.sh
;; gives a scratch home), with the same gpg-agent.conf.
;;
;; Left out because they need a dialog: the decryption of key encrypted
;; blocks (tm-gpg-dialogue-decrypt asks the passphrase of the key), the
;; dialogues which ask a new passphrase, the key manager, the wallet, the
;; certificates and the TLS connections. An error of a gpg command opens a
;; dialog window (report-system-error), which does not block.
;;
;; What is missing is skipped with a message: GnuTLS when it is not compiled
;; in, the openssl password encodings when openssl cannot make them, GnuPG
;; when there is no gpg command (or a gpg too old for --quick-gen-key).

(texmacs-module (check crypto-test)
  (:use (check check-lib)
        (security password)
        (security gpg gpg-edit)
        (server server-authentication)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Helpers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define crypto-dir (url-append (url-temp-dir) "crypto-test"))

(define (tmp name) (url-append crypto-dir name))

;; the sockets of gpg-agent are made in its home directory, and the name of
;; a Unix socket has at most 104 bytes on macOS (108 on Linux, where gpg puts
;; them in /run/user instead): the GnuPG homes of the checks are in /tmp
(define gpg-socket-max 80)

(define (gpg-tmp name)
  (if (or (os-mingw?) (os-win32?)) (tmp name)
      (system->url (string-append "/tmp/tm-" name "-"
                                  (url->string (url-tail (url-temp-dir)))))))

(define (skip what why)
  (display* "  SKIP " what ": " why "\n")
  (force-output))

(define (contains? s what)
  (and (string? s) (>= (string-search-forwards what 0 s) 0)))

(define (armored? s what)
  (and (string? s)
       (string-starts? s (string-append "-----BEGIN PGP " what "-----"))))

(define (h x) (tree-hash (stree->tree x)))

(define (all-bytes)
  (list->string (map integer->char (iota 256))))

(define (guarded name thunk)
  ;; an error in a group is counted as a failure, and the suite goes on
  (with r (check-run thunk)
    (when (and (pair? r) (== (car r) 'error))
      (check-report #f name (object->string r)))))

(define (hex-string? s)
  (and (string? s)
       (list-and (map (lambda (c) (in? c (string->list "0123456789ABCDEF")))
                      (string->list s)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Base64
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define all-bytes-base64
  (string-append
   "AAECAwQFBgcICQoLDA0ODxAREhMUFRYXGBkaGxwdHh8gISIjJCUmJygpKissLS4vMDEy"
   "MzQ1Njc4OTo7PD0+P0BBQkNERUZHSElKS0xNTk9QUVJTVFVWV1hZWltcXV5fYGFiY2Rl"
   "ZmdoaWprbG1ub3BxcnN0dXZ3eHl6e3x9fn+AgYKDhIWGh4iJiouMjY6PkJGSk5SVlpeY"
   "mZqbnJ2en6ChoqOkpaanqKmqq6ytrq+wsbKztLW2t7i5uru8vb6/wMHCw8TFxsfIycrL"
   "zM3Oz9DR0tPU1dbX2Nna29zd3t/g4eLj5OXm5+jp6uvs7e7v8PHy8/T19vf4+fr7/P3+"
   "/w=="))

;; Base64 carries the salts and the binary data of documents: the vectors
;; of RFC 4648, every byte value (against Python's base64), the line
;; breaks after 80 characters, and decoding, which skips what is not in
;; the alphabet.
(define (test-base64)
  (check-group "base64")
  (check= (encode-base64 "") "")
  (check= (encode-base64 "f") "Zg==")
  (check= (encode-base64 "fo") "Zm8=")
  (check= (encode-base64 "foo") "Zm9v")
  (check= (encode-base64 "foob") "Zm9vYg==")
  (check= (encode-base64 "fooba") "Zm9vYmE=")
  (check= (encode-base64 "foobar") "Zm9vYmFy")
  (check= (decode-base64 "") "")
  (check= (decode-base64 "Zg==") "f")
  (check= (decode-base64 "Zm8=") "fo")
  (check= (decode-base64 "Zm9vYg==") "foob")
  (check= (decode-base64 "Zm9vYmE=") "fooba")
  (check= (decode-base64 "Zm9vYmFy") "foobar")
  ;; every byte value; the output is cut in lines of 80 characters
  (with enc (encode-base64 (all-bytes))
    (check= (map string-length (string-decompose enc "\n"))
            '(80 80 80 80 24))
    (check= (apply string-append (string-decompose enc "\n"))
            all-bytes-base64))
  (check= (decode-base64 (encode-base64 (all-bytes))) (all-bytes))
  (check= (decode-base64 all-bytes-base64) (all-bytes))
  ;; a long input
  (with s (apply string-append (map (lambda (i) (all-bytes)) (iota 20)))
    (check= (string-length (decode-base64 (encode-base64 s))) 5120)
    (check-true (== (decode-base64 (encode-base64 s)) s)))
  ;; white space and line breaks are skipped
  (check= (decode-base64 "Zm9v\nYmFy") "foobar")
  (check= (decode-base64 "Zm9v YmFy") "foobar")
  (check= (decode-base64 "Zm9v\r\nYmFy\n") "foobar")
  ;; FIXME: decode_base64 indexes its table with a signed char
  ;; (src/Data/String/base64.cpp:87 and :66-69), so a byte above 127 reads
  ;; before the table: (decode-base64 "QU\xffJD") gives "A@\t", not "ABC".
  ;; FIXME: decode-base64 crashes TeXmacs (segmentation fault) when a "="
  ;; follows a complete group, as in (decode-base64 "QUJD="): the pending
  ;; group is empty and decode_base64 (array<int>) reads ac[0], ac[1]
  ;; (src/Data/String/base64.cpp:66-67, called from :95-96).
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; tree-hash
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; tree-hash names the files of the tree cache: 16 upper case hexadecimal
;; digits, the same for equal trees and different for different ones. The
;; vectors of strings were computed by a separate implementation of hash64
;; (ASCII only: hash64 xors a char, so that bytes above 127 hash
;; differently where char is unsigned). The 256 strings of one byte have
;; 256 hashes, and the hash depends on the labels and on the way a string
;; is cut.
(define (test-tree-hash)
  (check-group "tree-hash")
  (check= (h "") "9341CA263702A9E6")
  (check= (h "abc") "DD7287E37DD89047")
  (check= (h (make-string 1000 #\a)) "7F872DACB8AD3634")
  (check= (h (apply string-append
                    (map (lambda (i)
                           "The quick brown fox jumps over the lazy dog")
                         (iota 10))))
          "E989389B2276E91D")
  (check= (string-length (h '(document "a" (em "b")))) 16)
  (check-true (hex-string? (h '(document "a" (em "b")))))
  (check-true (hex-string? (h (all-bytes))))
  (check= (h '(document "a" (em "b"))) (h '(document "a" (em "b"))))
  (check= (h (all-bytes)) (h (all-bytes)))
  (check= (length (list-remove-duplicates
                   (map (lambda (i) (h (string (integer->char i))))
                        (iota 256))))
          256)
  (with l (map h (list "x" "y" '(em "x") '(strong "x") '(em "y")
                       '(concat "ab") '(concat "a" "b") '(concat "ab" "")
                       '(concat "" "ab") '(document "x") '(document "x" "")
                       '(frac "a" "b") '(frac "b" "a")))
    (check= (length (list-remove-duplicates l)) (length l)))
  ;; the hash of a tree is not the one of a string, and its label is
  ;; separated from its children (#176)
  (check-false (== (h '(frac)) (h "frac")))
  (check-false (== (h '(em "x")) (h (string-append "em" (h "x")))))
  (check-false (== (h '(em "x")) (h (string-append "1:em" (h "x")))))
  (check-false (== (h (list (string->symbol (string-append "em" (h "x")))))
                   (h '(em "x")))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Random numbers and salts
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (draws n limit)
  (map (lambda (i) (gnutls-random-number limit)) (iota n)))

;; With GnuTLS, gnutls-random-number n is uniform in [0, n): the range, a
;; sequence which is not constant, and the counts of 6000 draws in 6 boxes
;; (1000 expected, 29 of standard deviation, 200 allowed). A salt is 32
;; random bytes in base64. Without GnuTLS the stubs give 0 and "".
(define (test-random)
  (check-group "random numbers")
  (if (not (supports-gnutls?))
      (begin
        (skip "random numbers" "TeXmacs is built without GnuTLS")
        (check= (gnutls-random-number 10) 0)
        (check= (gnutls-generate-salt) ""))
      (begin
        (check= (gnutls-random-number 0) 0)
        (check= (list-remove-duplicates (draws 50 1)) '(0))
        (for (limit '(2 3 10 1000 1000000 2147483647))
          (check-true (list-and (map (lambda (x) (and (>= x 0) (< x limit)))
                                     (draws 200 limit)))))
        (check= (sort (list-remove-duplicates (draws 200 2)) <) '(0 1))
        (check-true (> (length (list-remove-duplicates (draws 100 1000000)))
                       90))
        (let* ((l (draws 6000 6))
               (counts (map (lambda (k) (length (filter (cut == <> k) l)))
                            (iota 6))))
          (check-true (list-and (map (lambda (c) (and (> c 800) (< c 1200)))
                                     counts))))
        (let ((s1 (gnutls-generate-salt))
              (s2 (gnutls-generate-salt)))
          (check= (string-length s1) 44)
          (check= (string-length (decode-base64 s1)) 32)
          (check= (encode-base64 (decode-base64 s1)) s1)
          (check-false (== s1 s2))
          (check-false (== (generate-salt) (generate-salt)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Generated passwords
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define password-charset
  (string-append "abcdefghijklmnopqrstuvwxyz" "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
                 "0123456789" "!@#$%^&*()_+-=[]{}|;:,.?"))

(define (has-one-of? s chars)
  (list-or (map (lambda (c) (in? c (string->list chars))) (string->list s))))

;; generate-password n (the server makes the admin password with it): n
;; characters of the charset, at least one lower case letter, upper case
;; letter, digit and symbol, so that the server accepts it as strong.
(define (test-generate-password)
  (check-group "generate-password")
  (let ((l (map (lambda (i) (generate-password 20)) (iota 30))))
    (check= (list-remove-duplicates (map string-length l)) '(20))
    (for (chars (list "abcdefghijklmnopqrstuvwxyz" "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
                      "0123456789" "!@#$%^&*()_+-=[]{}|;:,.?"))
      (check-true (list-and (map (lambda (p) (has-one-of? p chars)) l))))
    (check-true (list-and
                 (map (lambda (p)
                        (list-and (map (lambda (c)
                                         (in? c (string->list
                                                 password-charset)))
                                       (string->list p))))
                      l)))
    (check-true (list-and (map server-strong-password? l)))
    (check= (length (list-remove-duplicates l)) 30))
  (check= (string-length (generate-password 4)) 4)
  ;; FIXME (#66, item 10): the characters come from the ordinary random
  ;; without GnuTLS (security/password.scm:26) and are always shuffled with
  ;; it (:33). *random-state* is seeded only when server-base.scm is loaded
  ;; (with the time in seconds), so that without GnuTLS (generate-password
  ;; 16) gives the same password in every new session ("%YUN}-t4.h*[PXmT"
  ;; on macOS); generate-password should use a cryptographic source or fail.
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Server passwords
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; A salt of 32 bytes ("a" 32 times), and PBKDF2-HMAC-SHA256 with 600000
;; iterations of three passwords, computed with Python's hashlib.
(define test-salt "YWFhYWFhYWFhYWFhYWFhYWFhYWFhYWFhYWFhYWFhYWE=")
(define pbkdf2-vectors
  '(("password" "JXx1KaRcr8wd3BTp5pjGzTFqBvAWZvm70ARhugD6O6E=")
    ("Tr0ub4dor&3" "qgFivnFVZAI8vmRb79rl4IZ/7N+0VtHosscst+Ra3zM=")))
(define test-salt-2 "AAECAwQFBgcICQoLDA0ODxAREhMUFRYXGBkaGxwdHh8=")
(define pbkdf2-vector-2 "7IoOytGEs2gtMCd22IHTihWzQ2J8cm26fZ28OGTRzVo=")

;; The server keeps (password type ...) lists: each supported encoding
;; accepts the password it encoded and rejects another one; malformed
;; lists are rejected; PBKDF2 gives the known hashes. Without GnuTLS and
;; with an openssl which cannot make sha256 or sha512 crypt strings (as the
;; LibreSSL of macOS), "clear" is the only encoding, and the server keeps
;; the passwords in clear.
(define (test-server-passwords)
  (check-group "server passwords")
  (with l (server-supported-password-encodings)
    (check-true (in? "clear" l))
    (check= (cAr l) "clear")
    (for (type '("sha256" "sha512" "pbkdf2"))
      (when (not (in? type l))
        (skip (string-append "password encoding " type)
              (if (== type "pbkdf2") "TeXmacs is built without GnuTLS"
                  "openssl passwd cannot make it"))))
    (for (type l)
      (with hidden (server-password-encode "Correct-Horse-9!" test-salt type)
        (check= (list (car hidden) (cadr hidden)) (list 'password type))
        (check-true (server-password-correct? "Correct-Horse-9!" hidden))
        (check-false (server-password-correct? "Correct-Horse-9?" hidden))
        (check-false (server-password-correct? "" hidden))
        (when (!= type "clear")
          (check-false (in? "Correct-Horse-9!" hidden))))))
  (check= (server-password-encode "secret" test-salt "clear")
          '(password "clear" "secret"))
  (check-false (server-password-encode "secret" test-salt "rot13"))
  (check-false (server-password-correct? "x" "x"))
  (check-false (server-password-correct? "x" '(password)))
  (check-false (server-password-correct? "x" '(password "clear")))
  (check-false (server-password-correct? "x" '(token "clear" "x")))
  (check-false (server-password-correct? "x" '(password "rot13" "x")))
  (check= (server-credentials-normalize "abc") '((password "clear" "abc")))
  (check= (server-add-salt '((password "p") (other "a" "b")) "s")
          '((password "p" "s") (other "a" "b")))
  ;; the credentials as the server stores and checks them
  (let* ((creds (server-add-salt '((tls-password "Pass-word-42")) test-salt))
         (hiddens (server-hide-credentials creds)))
    (check= (length hiddens) 1)
    (check-true (server-password-authentified? "u" "Pass-word-42" hiddens))
    (check-false (server-password-authentified? "u" "Pass-word-43" hiddens))
    (check-false (server-password-authentified? "u" "Pass-word-42" '())))
  (check-true (server-strong-password? "Abcdefgh1!"))
  (check-false (server-strong-password? "abcdefgh1!"))
  (check-false (server-strong-password? "ABCDEFGH1!"))
  (check-false (server-strong-password? "Abcdefghi!"))
  (check-false (server-strong-password? "Abcdefghi1"))
  (check-false (server-strong-password? "Abc1!"))
  (if (not (supports-gnutls?))
      (check= (hash-password-pbkdf2 "password" test-salt) "")
      (begin
        (for (v pbkdf2-vectors)
          (check= (hash-password-pbkdf2 (car v) test-salt)
                  (string-append "$7$600000$" test-salt "$" (cadr v)))
          (check= (server-password-encode (car v) test-salt "pbkdf2")
                  `(password "pbkdf2" ,test-salt ,(cadr v) "600000")))
        (check= (hash-password-pbkdf2 "password" test-salt-2)
                (string-append "$7$600000$" test-salt-2 "$" pbkdf2-vector-2))
        ;; a salt shorter than 32 bytes or not in base64 is refused
        (check= (hash-password-pbkdf2 "password" "YWFh") "")
        (check= (hash-password-pbkdf2 "password" "") "")
        (with s (generate-salt)
          (check-false (== (server-password-encode "pw-X1!" s "pbkdf2")
                           (server-password-encode "pw-X1!" test-salt
                                                   "pbkdf2"))))))
  ;; checking a password does not print it, since the output of a server
  ;; without a window is its log (#176); other lines may be printed (on
  ;; Windows, every process started is reported there)
  (for (type '("clear" "sha256" "sha512" "pbkdf2"))
    (check-false (contains? (begin
                              (cout-buffer)
                              (check-run
                               (lambda ()
                                 (server-password-correct?
                                  "Logged-pw-7!"
                                  `(password ,type ,test-salt "x"))))
                              (cout-unbuffer))
                            "Logged-pw-7!"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The format of encrypted blocks and documents
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; An encrypted block keeps the armored GnuPG message of the serialized
;; content (serialize-texmacs, read back by parse-texmacs-snippet) followed,
;; for the key encrypted ones, by the fingerprints of the recipients:
;; (gpg-encrypted-block msg fpr...), (gpg-passphrase-encrypted-block msg),
;; and the inline gpg-encrypted and gpg-passphrase-encrypted. An encrypted
;; document has a body made of (gpg-passphrase-encrypted-buffer msg).
(define (test-format)
  (check-group "format")
  (check-true (tm-gpg-encrypted? (stree->tree '(gpg-encrypted-block "m" "F"))))
  (check-true (tm-gpg-encrypted? (stree->tree '(gpg-encrypted "m" "F"))))
  (check-false (tm-gpg-encrypted? (stree->tree '(gpg-decrypted "m" "F"))))
  (check-false (tm-gpg-encrypted? (stree->tree "m")))
  (check-true (tm-gpg-decrypted? (stree->tree '(gpg-decrypted-block "m"))))
  (check-true (tm-gpg-passphrase-encrypted?
               (stree->tree '(gpg-passphrase-encrypted-block "m"))))
  (check-true (tm-gpg-passphrase-encrypted?
               (stree->tree '(gpg-passphrase-encrypted "m"))))
  (check-false (tm-gpg-passphrase-encrypted?
                (stree->tree '(gpg-encrypted-block "m"))))
  (check-true (tm-gpg-passphrase-decrypted?
               (stree->tree '(gpg-passphrase-decrypted "m"))))
  (check-true (tm-gpg-symbol-encrypted? 'gpg-encrypted-block))
  (check-false (tm-gpg-symbol-encrypted? 'gpg-passphrase-encrypted-block))
  (check-true (tm-gpg-symbol-passphrase-encrypted? 'gpg-passphrase-encrypted))
  (check-true (tm-gpg-symbol-decrypted? 'gpg-decrypted))
  (check-true (tm-gpg-symbol-passphrase-decrypted?
               'gpg-passphrase-decrypted-block))
  ;; the plain text which is encrypted, and read back after decryption;
  ;; parse-texmacs-snippet makes a document of what is not one
  (for (x '((document "hidden text") (document (theorem (document "x")) "y")))
    (check= (tree->stree (parse-texmacs-snippet
                          (serialize-texmacs (stree->tree x))))
            x))
  ;; FIXME: so an inline region comes back as a document:
  ;; tm-gpg-passphrase-decrypt (security/gpg/gpg-edit.scm:332-336) and
  ;; tm-gpg-dialogue-decrypt (:290) put (parse-texmacs-snippet dec) in the
  ;; inline tag, and (gpg-passphrase-decrypted (concat "a" (em "b"))),
  ;; encrypted and decrypted, would become (gpg-passphrase-decrypted
  ;; (document (concat "a" (em "b")))). Not run with gpg.
  (check= (tree->stree (parse-texmacs-snippet
                        (serialize-texmacs
                         (stree->tree '(concat "a" (em "b"))))))
          '(document (concat "a" (em "b"))))
  (check= (tree->stree (parse-texmacs-snippet "plain")) '(document "plain"))
  ;; encrypted documents
  (with enc '(gpg-passphrase-encrypted-buffer "m")
    (check-true (encrypted-buffer?
                 (stree->tree `(document (TeXmacs "2.1")
                                 (body (document ,enc))))))
    (check-false (encrypted-buffer?
                  (stree->tree `(document (TeXmacs "2.1")
                                  (body (document "a" ,enc)))))))
  (check-false (encrypted-buffer?
                (stree->tree '(document (TeXmacs "2.1")
                                (body (document "a"))))))
  (check-false (encrypted-buffer? (stree->tree '(document (TeXmacs "2.1")))))
  ;; FIXME (#66, item 5): tm-gpg-dialogue-passphrase-decrypt, the decryption
  ;; of a passphrase encrypted block, calls gpg-ask-ask-standalone-passphrase
  ;; (security/gpg/gpg-edit.scm:362), which is not defined: it raises
  ;; unbound-variable instead of asking the passphrase.
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Passphrases of encrypted documents
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; The passphrase of an encrypted document is kept in memory for its name
;; and the name of its autosave file; Save as copies it, a move moves it.
;; The wallet is off in the scratch home, so nothing is written.
(define (test-buffer-passphrases)
  (check-group "document passphrases")
  (let ((u (tmp "pass-a.tm"))
        (v (tmp "pass-b.tm"))
        (w (tmp "pass-c.tm")))
    (check-false (gpg-get-buffer-passphrase u))
    (gpg-set-buffer-passphrase u "pass-one")
    (check= (gpg-get-buffer-passphrase u) "pass-one")
    (check= (gpg-get-buffer-passphrase (url-autosave u "~")) "pass-one")
    (gpg-copy-buffer-passphrase u v)
    (check= (gpg-get-buffer-passphrase v) "pass-one")
    (check= (gpg-get-buffer-passphrase u) "pass-one")
    (gpg-move-buffer-passphrase v w)
    (check= (gpg-get-buffer-passphrase w) "pass-one")
    (check-false (gpg-get-buffer-passphrase v))
    (gpg-set-buffer-passphrase u "pass-two")
    (check= (gpg-get-buffer-passphrase u) "pass-two")
    (gpg-delete-buffer-passphrase u)
    (gpg-delete-buffer-passphrase w)
    (check-false (gpg-get-buffer-passphrase u))
    (check-false (gpg-get-buffer-passphrase w))
    ;; the passphrases of the autosave files are deleted too (#176)
    (check-false (gpg-get-buffer-passphrase (url-autosave u "~")))
    (check-false (gpg-get-buffer-passphrase (url-autosave u "#")))
    (check-false (gpg-get-buffer-passphrase (url-autosave w "~")))
    (check-false (gpg-get-buffer-passphrase (url-autosave w "#")))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Untrusted documents
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; The scripts of an untrusted document may call the :secure functions only:
;; the gpg functions which delete keys or decrypt are not among them.
(define (test-secure)
  (check-group "secure gpg functions")
  (check-false (secure? '(gpg-delete-public-key "F")))
  (check-false (secure? '(gpg-delete-secret-and-public-key "F")))
  (check-false (secure? '(gpg-decrypt "m" "p")))
  (check-false (secure? '(gpg-export-secret-keys (list "F"))))
  (check-false (secure? '(gpg-set-buffer-passphrase "a.tm" "p")))
  (check-false (secure? '(tree-export-encrypted "a.tm" "x")))
  ;; FIXME (#66, item 2): gpg-set-default-key-fingerprint, which sets a
  ;; preference (security/gpg/gpg-widgets.scm:51), and
  ;; tm-gpg-collect-public-keys-from-buffer, which writes a file
  ;; (security/gpg/gpg-base.scm:225), are :secure: (secure?
  ;; '(gpg-set-default-key-fingerprint "F")) and (secure?
  ;; '(tm-gpg-collect-public-keys-from-buffer)) are #t.
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Without GnuPG
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; When gpg is missing, every operation fails cleanly (with #f, an empty
;; list, or an unchanged block); its error dialog does not block.
(define (test-without-gpg)
  (check-group "without gpg")
  (check-false (supports-gpg?))
  (check-false (gpg-passphrase-encrypt "data" "pw"))
  (check-false (gpg-passphrase-decrypt "data" "pw"))
  (check-false (gpg-encrypt "data" (list "F")))
  (check-false (gpg-decryptable? "data" "pw"))
  (check= (gpg-public-keys) '())
  (check= (gpg-secret-keys) '())
  (with t (stree->tree '(gpg-passphrase-decrypted-block (document "a")))
    (tm-gpg-passphrase-encrypt t "pw")
    (check= (tree->stree t) '(gpg-passphrase-decrypted-block (document "a"))))
  (with t (stree->tree '(gpg-passphrase-encrypted-block "msg"))
    (tm-gpg-passphrase-decrypt t "pw")
    (check= (tree->stree t) '(gpg-passphrase-encrypted-block "msg")))
  ;; a document without the encryption variable is saved as it is
  (let ((u (tmp "plain.tm"))
        (doc '(document (TeXmacs "2.1") (style (tuple "generic"))
                        (body (document "PLAINTEXT")))))
    (check-false (tree-export (stree->tree doc) u "texmacs"))
    (check-true (contains? (string-load u) "PLAINTEXT"))
    (check-false (contains? (string-load u) "gpg-passphrase-encrypted")))
  ;; a document to be encrypted which cannot be is not saved: without a
  ;; passphrase, or when gpg fails (#66, item 3)
  (let ((u (tmp "secret.tm"))
        (doc '(document (TeXmacs "2.1") (style (tuple "generic"))
                        (initial (collection
                                  (associate "encryption" "gpg-passphrase")))
                        (body (document "SECRETTEXT")))))
    (when (url-exists? u) (system-remove u))
    (check-true (tree-export (stree->tree doc) u "texmacs"))
    (check-false (and (url-exists? u) (contains? (string-load u) "SECRETTEXT")))
    ;; with a passphrase, gpg is missing here
    (gpg-set-buffer-passphrase u "pw")
    (check-true (tree-export (stree->tree doc) u "texmacs"))
    (check-false (and (url-exists? u)
                      (contains? (string-load u) "SECRETTEXT")))
    (gpg-delete-buffer-passphrase u)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; GnuPG setup
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define gpg-agent-conf
  (string-append "pinentry-program /usr/bin/false\n"
                 "default-cache-ttl 0\n"
                 "max-cache-ttl 0\n"))

(define (gpg-run . args)
  ;; (status output error) of gpg with @args
  (evaluate-system (cons (gpg-get-executable) args) '() '() '(1 2)))

(define (gpg-make-test-home dir)
  ;; a GnuPG home directory which never asks a passphrase
  (when (not (url-exists? dir)) (system-mkdir dir))
  (when (not (or (os-mingw?) (os-win32?)))
    (system-1 "chmod 700" dir))
  (string-save gpg-agent-conf (url-append dir "gpg-agent.conf")))

(define (gpg-kill-agent dir)
  (when (url-exists-in-path? "gpgconf")
    (evaluate-system (list "gpgconf" "--homedir" (url->system dir)
                           "--kill" "gpg-agent")
                     '() '() '(1 2))))

(define (gpg-generate-test-key dir)
  ;; the fingerprint of a new key without passphrase, or a string which
  ;; says why it could not be made
  (with r (gpg-run "--homedir" (url->system dir) "--batch" "--no-tty"
                   "--pinentry-mode" "loopback" "--passphrase" ""
                   "--quick-gen-key"
                   "TeXmacs Test <texmacs-test@example.invalid>"
                   "default" "default" "never")
    (if (!= (car r) "0")
        (list 'error (caddr r))
        (with l (gpg-secret-key-fingerprints dir)
          (if (and (list? l) (= (length l) 1)) (car l)
              (list 'error "no key after --quick-gen-key"))))))

(define (gpg-available)
  ;; #t or the reason why GnuPG cannot be used
  (cond ((not (or (url-exists-in-path? "gpg") (url-exists-in-path? "gpg2")))
         "no gpg command in the path")
        ((not (gpg-valid-executable? (gpg-get-executable)))
         "the gpg executable of TeXmacs is not set")
        (else #t)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; GnuPG with keys
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define secret-text "First line of the secret.\nSecond line.\n")

;; The key of the temporary home is listed with its fingerprint and user
;; id; a message encrypted to it is armored, does not show the text, is
;; different each time, and decrypts to the text; a message for an unknown
;; recipient, garbage, or the message in a home without the key fail with
;; #f. Encrypted strings and objects saved to files are read back.
(define (test-gpg-keys dir other fpr)
  (check-group "gpg keys")
  (check= (string-length fpr) 40)
  (check-true (hex-string? fpr))
  (check= (gpg-public-key-fingerprints dir) (list fpr))
  (check= (gpg-secret-key-fingerprints dir) (list fpr))
  (check-true (gpg-public-key-fingerprint? fpr dir))
  (check-true (gpg-secret-key-fingerprint? fpr dir))
  (check-false (gpg-public-key-fingerprint? (make-string 40 #\0) dir))
  (check= (gpg-secret-key-fingerprints other) '())
  (with key (gpg-search-key-by-fingerprint fpr (gpg-public-keys dir))
    (check-true key)
    ;; (the keys are listed as Cork strings, for the widgets: < is <less>)
    (check= (and key (gpg-get-key-user-id key))
            (utf8->cork "TeXmacs Test <texmacs-test@example.invalid>")))
  (check-true (armored? (gpg-export-public-keys (list fpr) dir)
                        "PUBLIC KEY BLOCK"))
  (check-group "gpg encryption")
  (let ((e1 (gpg-encrypt secret-text (list fpr) dir))
        (e2 (gpg-encrypt secret-text (list fpr) dir)))
    (check-true (armored? e1 "MESSAGE"))
    (check-false (contains? e1 "secret"))
    (check-false (== e1 e2))
    (check= (gpg-decrypt e1 "" dir) secret-text)
    (check= (gpg-decrypt e2 "" dir) secret-text)
    (check-true (gpg-decryptable? e1 "" dir))
    (check-false (gpg-decrypt e1 "" other))
    (check-false (gpg-decryptable? e1 "" other)))
  (check= (gpg-decrypt (gpg-encrypt (all-bytes) (list fpr) dir) "" dir)
          (all-bytes))
  (check-false (gpg-encrypt secret-text (list (make-string 40 #\0)) dir))
  (check-false (gpg-decrypt "not a message" "" dir))
  (check-false (gpg-decryptable? "not a message" "" dir))
  (check-group "gpg files")
  (let ((f (tmp "secret.gpg"))
        (g (tmp "object.gpg"))
        (o '(a "b" 3 (c "d"))))
    (check-true (gpg-string-encrypt-save secret-text f fpr dir))
    (check-true (url-exists? f))
    (check-false (contains? (string-load f) "secret"))
    (check= (gpg-string-load-decrypt f "" dir) secret-text)
    (check-false (gpg-string-load-decrypt f "" other))
    (gpg-encrypt-save-object g o fpr dir)
    (check= (gpg-load-decrypt-object g "" dir) o)
    ;; a long object comes back whole (with S7, write prints only the first
    ;; 40 elements of a list: the GnuPG wallet saves its table this way)
    (with long (map (lambda (i) (list i (number->string i))) (iota 100))
      (gpg-encrypt-save-object g long fpr dir)
      (check= (gpg-load-decrypt-object g "" dir) long)))
  ;; FIXME (#66, item 4): gpg-delete-public-key runs
  ;; --delete-secret-and-public-key (security/gpg/gpg-base.scm:429), so
  ;; that deleting a public key also deletes the secret key.
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; GnuPG with a passphrase
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Symmetric encryption with the cipher of the preferences (AES256 by
;; default): the message decrypts with the passphrase and not with another
;; one (the passphrase cache of the agent is disabled).
(define (test-gpg-passphrase dir)
  (check-group "gpg passphrase")
  (check-true (in? (gpg-get-cipher-algorithm) '("AES192" "AES256")))
  (with e (gpg-passphrase-encrypt secret-text "pass-one" dir)
    (check-true (armored? e "MESSAGE"))
    (check-false (contains? e "secret"))
    (check= (gpg-passphrase-decrypt e "pass-one" dir) secret-text)
    (check-true (gpg-decryptable? e "pass-one" dir))
    (check-false (gpg-passphrase-decrypt e "pass-two" dir))
    (check-false (gpg-decryptable? e "pass-two" dir))
    (check-false (gpg-passphrase-decrypt e "" dir)))
  (check-false (== (gpg-passphrase-encrypt "x" "p" dir)
                   (gpg-passphrase-encrypt "x" "p" dir))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; GnuPG in documents
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (in-buffer doc thunk)
  ;; run @thunk with the current buffer holding @doc, then close it
  (let* ((old (current-buffer))
         (u (new-buffer)))
    (buffer-set-body u (stree->tree doc))
    (guarded "the buffer" thunk)
    (buffer-close u)
    (when (buffer-exists? old) (switch-to-buffer old))))

;; The commands of gpg-edit.scm use the GnuPG home of TeXmacs. A passphrase
;; block of a buffer is encrypted and decrypted in place; a key block is
;; encrypted for its recipients (the decryption asks the passphrase of the
;; key in a dialog, so the message is decrypted with gpg-decrypt). A
;; document with a passphrase is saved encrypted and read back. The commands
;; replace the block by a new tree (tree-set! of another label), so the
;; block is looked up again in the buffer after each of them.
(define (first-block) (tree-ref (buffer-tree) 0))

(define (test-gpg-documents dir fpr)
  (check-group "gpg blocks")
  (in-buffer '(document (gpg-passphrase-decrypted-block
                         (document "hidden text")))
    (lambda ()
      (tm-gpg-passphrase-encrypt (first-block) "block-pass")
      (with t (first-block)
        (check= (tree-label t) 'gpg-passphrase-encrypted-block)
        (check= (tree-arity t) 1)
        (check-true (armored? (tree->string (tree-ref t 0)) "MESSAGE"))
        (check-false (contains? (tree->string (tree-ref t 0)) "hidden")))
      (with enc (tree->stree (first-block))
        (tm-gpg-passphrase-decrypt (first-block) "wrong-pass")
        (check= (tree->stree (first-block)) enc))
      (tm-gpg-passphrase-decrypt (first-block) "block-pass")
      (check= (tree->stree (first-block))
              '(gpg-passphrase-decrypted-block (document "hidden text")))))
  ;; the public key goes to the GnuPG home of TeXmacs
  (check-true (gpg-import-public-keys (gpg-export-public-keys (list fpr) dir)))
  (in-buffer `(document (gpg-decrypted-block (document "for the key") ,fpr))
    (lambda ()
      (tm-gpg-encrypt (first-block))
      (with t (first-block)
        (check= (tree-label t) 'gpg-encrypted-block)
        (check= (tree->string (tree-ref t 1)) fpr)
        (with enc (tree->string (tree-ref t 0))
          (check-true (armored? enc "MESSAGE"))
          (check= (tree->stree
                   (parse-texmacs-snippet (or (gpg-decrypt enc "" dir) "")))
                  '(document "for the key"))))))
  (check-group "gpg documents")
  (let ((u (tmp "encrypted.tm"))
        (doc '(document (TeXmacs "2.1") (style (tuple "generic"))
                        (body (document "TOPSECRET"))
                        (initial (collection
                                  (associate "encryption" "gpg-passphrase"))))))
    (gpg-set-buffer-passphrase u "doc-pass")
    (check-false (tree-export (stree->tree doc) u "texmacs"))
    (with s (string-load u)
      (check-false (contains? s "TOPSECRET"))
      (check-true (contains? s "gpg-passphrase-encrypted-buffer")))
    (with t (tree-import u "texmacs")
      (check-true (encrypted-buffer? t))
      (with enc (tree->string (tree-ref (tmfile-get t 'body) 0 0))
        (check-false (gpg-passphrase-decrypt enc "wrong-pass"))
        (with dec (gpg-passphrase-decrypt enc "doc-pass")
          (check-true (contains? dec "TOPSECRET")))))
    (gpg-delete-buffer-passphrase u)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; GnuPG
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (scratch-home?)
  ;; the GnuPG home of TeXmacs may be used: TeXmacs does not run with the
  ;; home of the user
  (let ((home (url->system (url-concretize "$TEXMACS_HOME_PATH")))
        (user (url->system (url-concretize "~/.TeXmacs"))))
    (!= home user)))

(define (test-gpg)
  (with ok (gpg-available)
    (if (!= ok #t)
        (begin
          (check-group "gpg")
          (skip "GnuPG" ok)
          (test-without-gpg))
        (let ((dir (gpg-tmp "gnupg"))
              (other (gpg-tmp "gnupg-other")))
          (dynamic-wind
            (lambda ()
              (gpg-make-test-home dir)
              (gpg-make-test-home other))
            (lambda ()
              (with fpr (gpg-generate-test-key dir)
                (if (not (string? fpr))
                    (skip "GnuPG"
                          (string-append "cannot generate a test key: "
                                         (object->string fpr)))
                    (begin
                      (guarded "gpg keys"
                               (lambda () (test-gpg-keys dir other fpr)))
                      (guarded "gpg passphrase"
                               (lambda () (test-gpg-passphrase dir)))
                      (cond
                        ((not (scratch-home?))
                         (skip "GnuPG in documents"
                               "TeXmacs runs with the home of the user"))
                        ((> (string-length (url->system (gpg-homedir)))
                            gpg-socket-max)
                         (skip "GnuPG in documents"
                               (string-append "the GnuPG home of TeXmacs "
                                 "is too long for the sockets of gpg-agent "
                                 "(use a shorter TM_TEST_HOME): "
                                 (url->system (gpg-homedir)))))
                        (else
                          (let* ((home (gpg-homedir))
                                 (made? (not (url-exists? home))))
                            (when made?
                              (when (not (url-exists? (gpg-userdir)))
                                (system-mkdir (gpg-userdir)))
                              (gpg-make-test-home home))
                            (guarded "gpg documents"
                                     (lambda ()
                                       (test-gpg-documents dir fpr)))
                            (gpg-kill-agent home)
                            (when made? (system-rmdir-recursive home)))))))))
            (lambda ()
              (gpg-kill-agent dir)
              (gpg-kill-agent other)
              (system-rmdir-recursive dir)
              (system-rmdir-recursive other)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The suite
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (crypto-test-failures)
  (check-suite "crypto")
  (when (not (url-exists? crypto-dir)) (system-mkdir crypto-dir))
  (dynamic-wind
    (lambda () #t)
    (lambda ()
      (guarded "base64" test-base64)
      (guarded "tree-hash" test-tree-hash)
      (guarded "random numbers" test-random)
      (guarded "generate-password" test-generate-password)
      (guarded "server passwords" test-server-passwords)
      (guarded "format" test-format)
      (guarded "document passphrases" test-buffer-passphrases)
      (guarded "secure gpg functions" test-secure)
      (guarded "gpg" test-gpg))
    (lambda ()
      (system-rmdir-recursive crypto-dir)))
  (check-end))
