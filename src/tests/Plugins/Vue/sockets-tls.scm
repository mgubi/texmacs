;; the TeXmacs server and a TLS client in the same instance (needs a build
;; with --with-gnutls). A self-signed certificate is generated in
;; $TEXMACS_SERVER_CERT_DIR when there is none: run with that variable
;; pointing to a scratch directory and with -tls-no-verify (self-signed).
(use-modules (server server-base) (client client-base) (client client-authentication))
(define tls-cert (string->url "$TEXMACS_SERVER_CERT_DIR/cert.pem"))
(define tls-key  (string->url "$TEXMACS_SERVER_CERT_DIR/key.pem"))
(display* "SOCKETS-TLS gnutls: " (supports-gnutls?) " cert dir: " (url->string tls-cert) "\n")
;; the server reads the tls-server preference from C++: its default ("on")
;; is only registered when the server modules are loaded, set it explicitly
;; for the duration of the test
(define sockets-old-tls (get-preference "tls-server"))
(set-preference "tls-server" "on")
(when (not (url-exists? tls-cert))
  (display* "SOCKETS-TLS certificate generated: "
            (generate-self-signed-certificate '(("cn" "localhost")) tls-cert tls-key) "\n"))
(delayed (:pause 3000)
  (server-start)
  (display* "SOCKETS-TLS server started: " (server-started?) "\n")
  (delayed (:pause 1500)
    (with fd (anonymous-client-start "localhost" "6561" 'tls)
      (display* "SOCKETS-TLS client fd: " fd "\n")
      (when (> fd 0)
        (client-remote-eval fd '(remote-public-preferences)
          (lambda (r) (display* "SOCKETS-TLS roundtrip: " r "\n"))
          (lambda (e) (display* "SOCKETS-TLS roundtrip error: " e "\n"))))
      (delayed (:pause 5000)
        (when (> fd 0) (client-stop fd))
        (server-stop)
        (display* "SOCKETS-TLS stopped, server running: " (server-started?) "\n")
        (if (== sockets-old-tls "default")
            (reset-preference "tls-server")
            (set-preference "tls-server" sockets-old-tls))))))
