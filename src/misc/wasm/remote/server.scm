;; a TeXmacs server for the tests of the clients: admin/secret123
;; TLS for the other clients, as by default (the home of the test is a copy),
;; with a self-signed certificate
(let ((cert (string->url "$TEXMACS_SERVER_CERT_DIR/cert.pem"))
      (key (string->url "$TEXMACS_SERVER_CERT_DIR/key.pem")))
  (unless (url-exists? cert)
    (display* "certificate: "
              (generate-self-signed-certificate '(("cn" "localhost")) cert key)
              "\n")
    (when (server-started?) (server-stop))))
(when (!= (get-preference "tls-server") "on")
  (set-preference "tls-server" "on")
  (when (server-started?) (server-stop)))
(unless (server-started?) (server-start))
(let* ((credentials (server-add-salt (list (list 'tls-password "secret123"))
                                     (generate-salt)))
       (hiddens (server-hide-credentials credentials))
       (info (server-get-user-info "admin")))
  (server-set-user-info #f "admin" "admin" hiddens (fourth info) #t))
;; a second user, for what takes two (sharing, messages, chat): bob/secret456
(unless (server-find-user "bob")
  (let* ((credentials (server-add-salt (list (list 'tls-password "secret456"))
                                       (generate-salt)))
         (hiddens (server-hide-credentials credentials)))
    (server-set-user-info #f "bob" "Bob" hiddens "bob@localhost" #f)))
(display* "test server ready, websocket: " (get-preference "server websocket") "\n")
