; The outline of CSS for TeXmacs (progs/treesit/ts-edit.scm): ours, the
; grammar has no tags.scm.

(rule_set (selectors) @name) @definition.rule
(media_statement . (_) @name) @definition.media
(keyframes_statement (keyframes_name) @name) @definition.keyframes
