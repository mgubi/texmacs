; The outline of JSON (the keys of the first two levels) for TeXmacs (progs/treesit/ts-edit.scm): ours, the
; grammar has no tags.scm.

(document (object (pair key: (string (string_content) @name)) @definition.key))
(document (object (pair value: (object
  (pair key: (string (string_content) @name)) @definition.key))))
