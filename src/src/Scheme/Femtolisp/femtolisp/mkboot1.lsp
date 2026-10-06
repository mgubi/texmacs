; -*- scheme -*-

(load "system.lsp")
(load "compiler.lsp")

; TeXmacs: the functions of system.lsp and compiler.lsp call each other
; through private names, %fl:name, so that a program may redefine a public
; name (TeXmacs has its own make-label, print...) without breaking the
; compiler. The functions are compiled again with resolve-global mapping
; their names to the private names, then the public names get the same
; functions. resolve-global stays public: the embedding redefines it.
(let ((private (table)))
  (for-each (lambda (s)
	      (if (and (bound? s) (not (constant? s))
		       (function? (top-level-value s))
		       (not (memq s '(resolve-global compile-unknown-call)))
		       (not (string.find (string s) "%fl:")))
		  (put! private s (symbol (string "%fl:" s)))))
	    (environment))
  ; the private names start with the functions of the first compilation
  (table.foldl (lambda (s p z) (set-top-level-value! p (top-level-value s)) z)
	       #t private)
  (set! resolve-global (lambda (s) (get private s s)))
  (load "system.lsp")
  (load "compiler.lsp")
  (set! resolve-global (lambda (s) s))
  (table.foldl (lambda (s p z) (set-top-level-value! s (top-level-value p)) z)
	       #t private))

(make-system-image "flisp.boot")
