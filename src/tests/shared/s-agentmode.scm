;; The mode the person chose for a file is kept in the person's process: the
;; file is handed to the agent's process (the person closes it) and comes back
;; (the person opens it again): the mode is the one chosen, not the default.
(load (string-append (getenv "SH_DIR") "/lib.scm"))

(define (A-steps)
  (list
   (act "interactive" (shared-set-interactive! #t))
   (act "load" (load-buffer FILE))
   (act "mode" (check "the default mode is propose" (eq? (shared-agent-mode (the-name)) 'propose))
        (shared-set-agent-mode! (the-name) 'read-only)
        (sync-set! "a-loaded"))
   (wait-sync "b-joined")
   (act "close" (buffer-close (the-name)) (sync-set! "a-closed"))
   (wait-sync "b-hosts")
   (act "reload" (load-buffer FILE))
   (step "hosts again" (== (role-of) 'host))
   (act "check" (check "the mode chosen is kept" (eq? (shared-agent-mode (the-name)) 'read-only))
        (sync-set! "a-back"))
   (wait-sync "b-done")
   (act "out" (publish-body!) (sync-set! "a-done"))))

(define (B-steps)
  (list
   (wait-sync "a-loaded")
   (act "agent" (shared-set-agent! #t))
   (act "load" (load-buffer FILE))
   (step "mode told" (eq? (shared-agent-mode (the-name)) 'read-only))
   (act "sig" (sync-set! "b-joined"))
   (wait-sync "a-closed")
   (step "B hosts" (== (role-of) 'host))
   (act "sig" (sync-set! "b-hosts"))
   (wait-sync "a-back")
   (step "participant again" (== (role-of) 'participant))
   (step "told again" (eq? (shared-agent-mode (the-name)) 'read-only))
   (act "check" (check "the agent is told the mode again" #t) (sync-set! "b-done"))
   (wait-sync "a-done")
   (act "out" (publish-body!))))

(run-steps (if (== ROLE "A") (A-steps) (B-steps)))
