;; For tests/mcp/check.sh (scenario static): prints, for the document in the
;; environment variable SAFE_FILE, the macros of the table of standard macros
;; (safe-macros in agents/mcp-server.scm) that its style defines, with the
;; places the static analysis finds in them.
(use-modules (agents mcp-server))
(define f (getenv "SAFE_FILE"))
(load-buffer f)
(let ((r (mcp-safe-report f)))
  (for (e (car r))
    (display* "SAFE " (car e) " " (cadr e) " " (object->string (caddr e)) "\n"))
  (for (n (cadr r)) (display* "ALLOW " n "\n")))
(display* "SAFE-END\n")
(quit-TeXmacs)
