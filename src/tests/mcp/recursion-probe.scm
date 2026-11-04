;; A macro that calls itself, in a block and inline: typesetting and export
;; give an error box, not a stack overflow (RECURSION_FILE, RECURSION_PDF).
(load-buffer (getenv "RECURSION_FILE"))
(update-forced)
(display* "TYPESET-OK\n")
(export-buffer (getenv "RECURSION_PDF"))
(display* "EXPORT-OK\n")
(quit-TeXmacs)
