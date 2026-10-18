; Definitions of Lean, for the outline of TeXmacs (written for TeXmacs:
; tree-sitter-lean has no tags.scm)

(def name: (identifier) @name) @definition.function
(abbrev name: (identifier) @name) @definition.function
(theorem name: (identifier) @name) @definition.theorem
(instance name: (identifier) @name) @definition.instance
(structure name: (identifier) @name) @definition.class
(inductive name: (identifier) @name) @definition.type
