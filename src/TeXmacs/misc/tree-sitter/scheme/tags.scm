; Definitions of Scheme and of TeXmacs, for the outline of TeXmacs (written
; for TeXmacs: tree-sitter-scheme has no tags.scm)

(list
  . (symbol) @_definer
  . (list . (symbol) @name)
  (#any-of? @_definer "define" "define-public" "define-macro"
            "define-public-macro" "tm-define" "tm-define-macro" "tm-menu"
            "tm-widget" "define*" "define-macro*")) @definition.function

(list
  . (symbol) @_definer
  . (symbol) @name
  (#any-of? @_definer "define" "define-public" "tm-define" "menu-bind"
            "define-group")) @definition.variable
