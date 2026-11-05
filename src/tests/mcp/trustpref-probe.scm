;; What the person remembers (the preference agent-trust) with names and paths that hold quotes,
;; backslashes, newlines and non-ASCII characters; a bad entry does not drop the others; the limit
;; of 200 entries. Two processes with the same home: PHASE write, then read.
(define (show-ok name ok) (display* "CHECK " name " " (if ok "PASS" "FAIL") "\n"))
(define phase (getenv "TRUST_PHASE"))
(define nl (string (integer->char 10)))
(define uni (string (integer->char 233) (integer->char 195) (integer->char 169)))
(define paths (list "/tmp/a\"b.tm" "/tmp/back\\slash.tm" (string-append "/tmp/new" nl "line.tm")
                    (string-append "/tmp/uni" uni ".tm") "/tmp/a b:c.tm"))
(define names (list "quote\"name" "back\\name" (string-append "nl" nl "name") (string-append "uni" uni) "plain" "a b:c"))
(catch #t
  (lambda ()
    (if (== phase "write")
        (begin
          (for (p paths)
            (for (n names)
              (shared-trust-remember! p n "style")))
          (shared-trust-remember! (car paths) "plain" "links")
          ;; (an entry that does not read, and one with a level that cannot be remembered)
          (set-preference "agent-trust" (string-append (get-preference "agent-trust") " bad:entry zzzz:zzzz:style 0041:0041:code"))
          (show-ok "written entries read back (quotes, backslashes, newlines, non-ASCII)"
                   (list-and (map (lambda (p) (list-and (map (lambda (n) (shared-trust-remembered? p n "style")) names))) paths)))
          (show-ok "and the links entry" (shared-trust-remembered? (car paths) "plain" "links"))
          (show-ok "a bad entry does not drop the others" (shared-trust-remembered? (cadr paths) "back\\name" "style"))
          (show-ok "level code is never remembered" (not (shared-trust-remember! "/x" "n" "code")))
          (show-ok "level redefinition is never remembered" (not (shared-trust-remember! "/x" "n" "redefinition"))))
        (begin
          (show-ok "after a restart the entries are there"
                   (list-and (map (lambda (p) (list-and (map (lambda (n) (shared-trust-remembered? p n "style")) names))) paths)))
          (shared-trust-forget! (car paths) "quote\"name" "style")
          (show-ok "revoked one: it is gone, the others stay"
                   (and (not (shared-trust-remembered? (car paths) "quote\"name" "style"))
                        (shared-trust-remembered? (car paths) "plain" "style")))
          (let loop ((i 0))
            (when (< i 250) (shared-trust-remember! "/limit" (string-append "n" (number->string i)) "style") (loop (+ i 1))))
          (show-ok "the limit of 200 entries" (not (shared-trust-remember! "/limit" "one-more" "style")))
          (show-ok "entries before the limit are kept" (shared-trust-remembered? "/limit" "n0" "style"))
          (show-ok "the list is full" (shared-trust-full?))
          ;; entries that were corrupted by hand: each is skipped, nothing throws, the good one stays
          (set-preference "agent-trust"
                          (string-append "d800:0041:style ffff:0041:style 0100:0041:style -041:0041:style +041:0041:style "
                                         "0.50:0041:style 1/10:0041:style 1e+1:0041:style #x41:0041:style 0041:004:style "
                                         "::style ::: 00zz:0041:style 0041:0041:style"))
          (show-ok "corrupt entries are skipped, the good one is read, nothing throws"
                   (and (shared-trust-remembered? "A" "A" "style")
                        (= (length (shared-trust-all)) 1)))
          (show-ok "and the list is not full any more" (not (shared-trust-full?)))
          ;; forgetting: the grants of files that no longer exist, then all
          (shared-trust-remember! "/" "keeper" "style")
          (shared-trust-remember! "/no/such/folder/file.tm" "ghost" "style")
          (show-ok "forget the grants of missing files: one is forgotten, the others stay"
                   (and (= (shared-trust-forget-missing!) 2)
                        (shared-trust-remembered? "/" "keeper" "style")
                        (not (shared-trust-remembered? "/no/such/folder/file.tm" "ghost" "style"))))
          (shared-trust-forget-all!)
          (show-ok "forget all" (null? (shared-trust-all))))))
  (lambda args (display* "CHECK exception " args " FAIL\n")))
(display* "TRUSTPREF-DONE\n")
(quit-TeXmacs)
