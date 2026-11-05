;; For tests/mcp/check.sh (scenario static): reads the file EXTERN_LIST, lines
;; "document-path<TAB>name,name,...", loads each document (its style defines
;; the macros) and prints, for each name, DEF or UNDEF and the places that the
;; static analysis finds in the macro. Also the list of the functions of extern
;; whose arguments are data.
(use-modules (agents mcp-server))
(define lines (string-split (string-load (getenv "EXTERN_LIST")) #\newline))
(display* "REFUSED " (object->string (mcp-refused-style-macros)) "\n")
(display* "ALLOWED " (object->string (mcp-extern-allowlist)) "\n")
(for (l lines)
  (when (> (string-length l) 0)
    (let* ((i (string-index l #\tab))
           (f (substring l 0 i))
           (names (string-split (substring l (+ i 1) (string-length l)) #\,)))
      (load-buffer f)
      (display* "DEFINED " (object->string (mcp-defined-names f names)) "\n")
      (for (e (mcp-dangerous-names f names))
        (display* "DANGER " (car e) " " (object->string (cdr e)) "\n"))
      (display* "DONE " f "\n"))))
(display* "EXTERN-END\n")
(quit-TeXmacs)
