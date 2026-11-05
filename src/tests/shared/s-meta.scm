;; The style and the initial environment are shared as the body is: a change
;; made in one process (style, packages, language, page and font settings) is
;; in the other; concurrent changes of settings and of the body merge; a
;; style change during a handover is kept.
(load (string-append (getenv "SH_DIR") "/lib.scm"))

(define (both . steps) steps)

(define (join-steps)
  (if (== ROLE "A")
      (list (act "load" (load-buffer FILE)) (act "go" (sync-set! "a-loaded"))
            (wait-sync "b-joined"))
      (list (wait-sync "a-loaded") (act "load" (load-buffer FILE))
            (act "go" (sync-set! "b-joined")))))

(define (A-steps)
  (list
   ;; 1. A changes the style, a package, the language and a font setting
   (act "A changes" (init-style "article") (add-style-package "number-europe")
        (init-env "language" "french") (init-env "font-base-size" "14")
        (sync-set! "a-changed"))
   (wait-sync "b-saw-1")
   ;; 2. B changes a setting and removes one
   (wait-sync "b-changed")
   (step "A sees B's changes" (and (== (meta-var "font-base-size") "11")
                                   (not (meta-var "language"))))
   (act "check 2" (check "the change of a setting reaches the host" (== (meta-var "font-base-size") "11"))
        (check "a setting removed in B is removed in A" (not (meta-var "language")))
        (check "the editor of A has it" (== (get-init "font-base-size") "11"))
        (sync-set! "a-saw-2"))))

(define (B-steps)
  (list
   (wait-sync "a-changed")
   (step "B sees A's changes" (and (== (meta-var "language") "french")
                                   (== (meta-var "font-base-size") "14")
                                   (== (cadr (meta-now)) '(tuple "article" "number-europe"))))
   (act "check 1" (check "style and package reach the participant" (== (cadr (meta-now)) '(tuple "article" "number-europe")))
        (check "settings reach the participant" (and (== (meta-var "language") "french") (== (meta-var "font-base-size") "14")))
        (check "the editor of B has them" (and (== (get-init "language") "french")
                                               (in? "number-europe" (get-style-list))))
        (sync-set! "b-saw-1"))
   (act "B changes" (init-env "font-base-size" "11") (init-default "language")
        (sync-set! "b-changed"))
   (wait-sync "a-saw-2")))

(define (steps)
  (append
   (join-steps)
   (if (== ROLE "A") (A-steps) (B-steps))
   ;; 3. at once: A changes a setting, B another setting and the body
   (round-steps 3
                (lambda () (init-env "page-type" "a5"))
                (lambda () (init-env "par-first" "2fn") (insert-text! 1 5 "BB")))
   (list (act "check 3"
              (check "both settings and the body merged"
                     (and (== (meta-var "page-type") "a5") (== (meta-var "par-first") "2fn")
                          (== (text-of 1) "HelloBB world.")))
              (check "nothing undone" (== (shared-stat 'lost) 0))))
   ;; 4. at once: both change the same setting; they end equal
   (round-steps 4
                (lambda () (init-env "font-base-size" "12"))
                (lambda () (init-env "font-base-size" "10")))
   (list (act "settle" (publish-body!)
              (sync-set! (string-append (string-downcase ROLE) "-fin5")))
         (wait-sync (if (== ROLE "A") "b-fin5" "a-fin5")))
   ;; 5. A changes the style and closes: B takes over with it; A joins again
   (if (== ROLE "A")
       (list (act "style, close" (add-style-package "number-us") (buffer-close (the-name))
                  (sync-set! "a-closed"))
             (wait-sync "b-checked")
             (act "reload" (load-buffer FILE))
             (act "rejoined" (check "the host's style comes with the join"
                                    (and (== (role-of) 'participant)
                                         (== (cadr (meta-now)) '(tuple "article" "number-europe" "number-us"))
                                         (== (meta-var "page-type") "a5")))
                  (sync-set! "a-rejoined"))
             (wait-sync "b-done"))
       (list (wait-sync "a-closed")
             (step "B hosts" (== (role-of) 'host))
             (act "check 5" (check "the style change survived the handover"
                                   (== (cadr (meta-now)) '(tuple "article" "number-europe" "number-us")))
                  (sync-set! "b-checked"))
             (wait-sync "a-rejoined")
             (act "done" (sync-set! "b-done"))))
   (list (act "out" (publish-body!)))))

(run-steps (steps))
