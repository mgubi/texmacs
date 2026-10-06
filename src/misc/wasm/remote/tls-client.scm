;; a desktop client (TCP, TLS) of the test server
(use-modules (client client-base))
(client-login-then "localhost" "6561" "admin" '(tls-password "secret123")
  (lambda (server . ret)
    (display* "tls client login: " server " " ret "\n")))
