; The outline of HTML (the headings and the elements with an id) for TeXmacs (progs/treesit/ts-edit.scm): ours, the
; grammar has no tags.scm.

((element (start_tag (tag_name) @_tag) (text) @name) @definition.heading
 (#match? @_tag "^[hH][1-6]$"))
((element (start_tag (attribute (attribute_name) @_attr
                                (quoted_attribute_value (attribute_value) @name)))) @definition.element
 (#eq? @_attr "id"))
