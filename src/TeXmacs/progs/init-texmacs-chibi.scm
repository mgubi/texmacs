
(define boot-start (texmacs-time))
(define remote-client-list (list))

(define developer-mode?
(equal? (cpp-get-preference "developer tool" "off") "on"))

