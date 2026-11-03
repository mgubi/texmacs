; Definitions of Julia, for the outline of TeXmacs (written for TeXmacs:
; tree-sitter-julia has no tags.scm)

(function_definition
  (signature (call_expression . (identifier) @name))) @definition.function

(function_definition
  (signature (identifier) @name)) @definition.function

(assignment
  . (call_expression . (identifier) @name)) @definition.function

(macro_definition
  (signature (call_expression . (identifier) @name))) @definition.macro

(struct_definition
  (type_head (identifier) @name)) @definition.class

(abstract_definition
  (type_head (identifier) @name)) @definition.type

(module_definition
  name: (identifier) @name) @definition.module
