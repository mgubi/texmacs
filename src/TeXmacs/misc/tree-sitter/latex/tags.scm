; The outline of LaTeX (the sectioning commands, labels and new commands) for TeXmacs (progs/treesit/ts-edit.scm): ours, the
; grammar has no tags.scm.

(part text: (curly_group (text) @name)) @definition.part
(chapter text: (curly_group (text) @name)) @definition.chapter
(section text: (curly_group (text) @name)) @definition.section
(subsection text: (curly_group (text) @name)) @definition.subsection
(subsubsection text: (curly_group (text) @name)) @definition.subsubsection
(paragraph text: (curly_group (text) @name)) @definition.paragraph
(label_definition name: (curly_group_label (label) @name)) @definition.label
(new_command_definition
  declaration: (curly_group_command_name (command_name) @name)) @definition.macro
