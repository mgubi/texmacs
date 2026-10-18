; The outline of TOML for TeXmacs (progs/treesit/ts-edit.scm): ours, the
; grammar has no tags.scm.

(table [(bare_key) (dotted_key) (quoted_key)] @name) @definition.table
(table_array_element [(bare_key) (dotted_key) (quoted_key)] @name) @definition.table
