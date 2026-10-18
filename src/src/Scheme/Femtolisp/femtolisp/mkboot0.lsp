; -*- scheme -*-

(if (not (bound? 'top-level-value)) (set! top-level-value %eval))
(if (not (bound? 'set-top-level-value!)) (set! set-top-level-value! set))
(if (not (bound? 'eof-object?)) (set! eof-object? (lambda (x) #f)))

;(load "compiler.lsp")

(define (compile-file inf)
  (let ((in  (file inf :read)))
    (let next ((E (read in)))
      (if (not (io.eof? in))
	  (begin (print (compile-thunk (expand E)))
		 (princ "\n")
		 (next (read in)))))
    (io.close in)))

; TeXmacs: with --compiler new-compiler.lsp first, the forms of that file are
; all compiled by the compiler of the image, then run, and the new compiler
; compiles the files. A cross step for a change of the bytecode or of the
; functions of the compiler (Julia's femtolisp did it with f21780ccb0): the
; old flisp and its boot image make the image of the new flisp,
;   old/flisp mkboot0.lsp --compiler compiler.lsp system.lsp compiler.lsp
(define (load-compiled-at-once inf)
  (let ((in  (file inf :read)))
    (let next ((E (read in)) (thunks ()))
      (if (io.eof? in)
	  (begin (io.close in)
		 (for-each (lambda (th) (th)) (reverse! thunks)))
	  (next (read in) (cons (compile-thunk (expand E)) thunks))))))

(let ((files (cdr *argv*)))
  (if (and (pair? files) (equal? (car files) "--compiler"))
      (begin (load-compiled-at-once (cadr files))
	     (set! files (cddr files))))
  (for-each (lambda (file)
	      (compile-file file))
	    files))
