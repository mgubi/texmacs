; Highlighting of LaTeX for TeXmacs (written for TeXmacs: tree-sitter-latex
; has no queries); the capture names are those of tree-sitter

[(line_comment) (block_comment) (comment_environment)] @comment

(command_name) @function
(todo_command_name) @function

[(begin) (end)] @keyword
(begin name: (curly_group_text (text) @type))
(end name: (curly_group_text (text) @type))

[(chapter) (part) (section) (subsection) (subsubsection) (paragraph)
 (subparagraph)] @keyword
(section text: (curly_group (text) @string.special))
(subsection text: (curly_group (text) @string.special))
(chapter text: (curly_group (text) @string.special))

[(package_include) (class_include) (latex_include) (import_include)
 (bibtex_include) (biblatex_include) (graphics_include)] @keyword.directive
[(curly_group_path) (curly_group_path_list)] @string

[(new_command_definition) (old_command_definition) (let_command_definition)
 (environment_definition) (theorem_definition)] @keyword
(new_command_definition declaration: (curly_group_command_name (command_name) @function))

[(label_definition) (label_reference) (label_reference_range) (citation)]
 @keyword
[(curly_group_label) (curly_group_label_list)] @label

[(inline_formula) (displayed_equation) (math_environment)] @string.special
(math_delimiter) @punctuation.bracket
(subscript) @operator
(superscript) @operator

[(curly_group_uri) (uri)] @string
[(verbatim_environment) (listing_environment) (minted_environment)] @string

["[" "]" "{" "}"] @punctuation.bracket
(delimiter) @punctuation.delimiter
