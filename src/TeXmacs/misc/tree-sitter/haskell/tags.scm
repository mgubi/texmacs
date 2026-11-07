; The definitions of a Haskell module, for the outline of TeXmacs
; (progs/treesit/ts-edit.scm): ours, the grammar has no tags.scm.

(declarations (function name: (variable) @name) @definition.function)
(declarations (bind name: (variable) @name) @definition.function)
(data_type name: (name) @name) @definition.class
(newtype name: (name) @name) @definition.class
(class name: (name) @name) @definition.interface
(type_synonym name: (name) @name) @definition.type
