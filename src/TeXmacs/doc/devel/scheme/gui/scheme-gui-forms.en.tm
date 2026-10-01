<TeXmacs|1.99.8>

<style|<tuple|tmdoc|old-spacing>>

<\body>
  <tmdoc-title|Forms>

  As explained in \P<hlink|Dialogs and composite
  widgets|scheme-gui-dialogs.en.tm>\Q the available widgets can be used to
  compose dialog windows which perform one simple task. But sometimes one
  needs to read complex input from the user and forms provide one mechanism
  to do this. They allow you to define multiple named fields of several
  types, whose values are stored in a hash table. The contents of this hash
  can be retrieved when the user clicks a button using the functions
  <scm|form-fields> and <scm|form-values>.

  In the following example you can see that the syntax is pretty much the
  same as for regular widgets, but you must prefix the keywords with
  <scm|form-> :

  <\session|scheme|default>
    <\folded-io|Scheme] >
      (tm-widget (form3 cmd)

      \ \ (resize "500px" "500px"

      \ \ \ \ (padded

      \ \ \ \ \ \ (form "Test"

      \ \ \ \ \ \ \ \ (aligned

      \ \ \ \ \ \ \ \ \ \ (item (text "Input:")

      \ \ \ \ \ \ \ \ \ \ \ \ (form-input "fieldname1" "string" '("one")
      "1w"))

      \ \ \ \ \ \ \ \ \ \ (item === ===)

      \ \ \ \ \ \ \ \ \ \ (item (text "Enum:")

      \ \ \ \ \ \ \ \ \ \ \ \ (form-enum "fieldname2" '("one" "two" "three")
      "two" "1w"))

      \ \ \ \ \ \ \ \ \ \ (item === ===)

      \ \ \ \ \ \ \ \ \ \ (item (text "Choice:")

      \ \ \ \ \ \ \ \ \ \ \ \ (form-choice "fieldname3" '("one" "two"
      "three") "one"))

      \ \ \ \ \ \ \ \ \ \ (item === ===)

      \ \ \ \ \ \ \ \ \ \ (item (text "Choices:")

      \ \ \ \ \ \ \ \ \ \ \ \ (form-choices "fieldname4"\ 

      \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ '("one" "two"
      "three")\ 

      \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ '("one" "two"))))

      \ \ \ \ \ \ \ \ (bottom-buttons

      \ \ \ \ \ \ \ \ \ \ ("Cancel" (cmd "cancel")) \<gtr\>\<gtr\>

      \ \ \ \ \ \ \ \ \ \ ("Ok"

      \ \ \ \ \ \ \ \ \ \ \ (display* (form-fields) " -\<gtr\> "
      (form-values) "\\n")

      \ \ \ \ \ \ \ \ \ \ \ (cmd "ok")))))))
    <|folded-io>
      \;
    </folded-io>

    <\input|Scheme] >
      (dialogue-window form3 (lambda (x) (display* x "\\n")) "Test of form3")
    </input>
  </session>

  The available form fields are <scm|(form-input <scm-arg|field>
  <scm-arg|type> <scm-arg|proposals> <scm-arg|width>)>, <scm|(form-enum
  <scm-arg|field> <scm-arg|vals> <scm-arg|val> <scm-arg|width>)>,
  <scm|(form-choice <scm-arg|field> <scm-arg|vals> <scm-arg|val>)>,
  <scm|(form-choices <scm-arg|field> <scm-arg|vals> <scm-arg|selected>)>
  and <scm|(form-toggle <scm-arg|field> <scm-arg|on?>)>. They take the same
  arguments as <scm|input>, <scm|enum>, <scm|choice>, <scm|choices> and
  <scm|toggle>, except that the command is replaced by the name of the
  field. Other widgets can be freely mixed with the fields inside a form.

  Inside the form, <scm|(form-fields)> returns the list of field names,
  <scm|(form-values)> the list of their current values, <scm|(form-ref
  <scm-arg|field>)> the value of one field and <scm|(form-set
  <scm-arg|field> <scm-arg|value>)> changes it. The values are stored in a
  global table under the name of the form and of the field; when the form
  is built, each field is initialized with its default value (the first
  proposal, <abbr|resp.> the selected value), except for toggles, whose
  value is only set when they are clicked. These macros are defined
  in <hlink|<verbatim|gui-markup.scm>|$TEXMACS_PATH/progs/kernel/gui/gui-markup.scm>.

  <tmdoc-copyright|2012|the <TeXmacs> team.>

  <tmdoc-license|Permission is granted to copy, distribute and/or modify
  this\ndocument under the terms of the GNU Free Documentation License,
  Version 1.1 or\nany later version published by the Free Software
  Foundation; with no Invariant\nSections, with no Front-Cover Texts, and
  with no Back-Cover Texts. A copy of\nthe license is included in the section
  entitled "GNU Free Documentation License".>
</body>

<\initial>
  <\collection>
    <associate|preamble|false>
  </collection>
</initial>