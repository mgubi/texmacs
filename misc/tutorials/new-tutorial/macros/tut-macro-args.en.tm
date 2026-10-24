<TeXmacs|2.1.4>

<style|<tuple|tmdoc|english>>

<\body>
  <tmdoc-title|Macros with arguments and the preamble>

  <paragraph*|The preamble>

  The macros which you define are stored in the <em|preamble> of your
  document, a hidden part at its beginning. You may look at it with
  <menu|Tools|Macros|Edit preamble>, which only shows the preamble, and
  shows it as <em|source code>: instead of their result, you see the
  definitions themselves. After defining the macro <markup|hi> of the
  previous section, the preamble contains

  <\tm-fragment>
    <inactive*|<assign|hi|<macro|Hello world!>>>
  </tm-fragment>

  This reads: <em|assign> to the name <verbatim|hi> the <em|macro> whose
  body is <verbatim|Hello world!>. Select <menu|Tools|Macros|Edit preamble>
  again in order to come back to your text.

  <paragraph*|A macro with an argument>

  Let us now define a macro <markup|love>, such that
  <inactive*|<love|free software>> produces the text \PI love free software
  <with|color|red|<math|\<heartsuit\>>>\Q. The text after \PI love\Q varies,
  so it becomes an <em|argument> of the macro, which we call <src-arg|name>.
  The definition reads

  <\tm-fragment>
    <inactive*|<assign|love|<macro|name|I love <arg|name>
    <with|color|red|<math|\<heartsuit\>>>>>>
  </tm-fragment>

  In the macro editor (<menu|Tools|Macros|New macro>), you would enter the
  name <verbatim|love> of the macro, then give it an argument and type the
  body. You may also type the definition directly in the preamble, in the
  same way as the source code of any document:

  <\enumerate>
    <item>Choose <menu|Tools|Macros|Edit preamble> and put the cursor at the
    end of the preamble.

    <item>Type <key|\\ a s s i g n> and <key|return>: an empty
    <markup|assign> appears. Type the name <verbatim|love> in its first
    field, and move to the second field with <key*|right>.

    <item>Type <key|\\ m a c r o> and <key|return>, then the name of the
    argument, <verbatim|name>. Press <key|tab>: a new field is inserted
    after it, for the body of the macro.

    <item>In the body, type \PI love \Q, and then the name of the argument
    after the <key|\\> key, <key|\\ n a m e>: the status bar says that
    <key|return> will insert the argument <verbatim|name>. Press
    <key|return>: the value of the argument will appear at this place. Type a space
    and finally add the heart: select the red color in
    <menu|Format|Color>, press <key|$> and type <key|\\ h e a r t s u i t>
    and <key|return>.

    <item>Leave the preamble with <menu|Tools|Macros|Edit preamble>.
  </enumerate>

  When you now type <key|\\ l o v e> and <key|return> in your text, a box
  with one field appears, in which you type the argument:

  <\big-envbox>
    I love <small-focus|free software<text-cursor>>
    <with|color|red|<math|\<heartsuit\>>>
  </big-envbox>

  The document <verbatim|examples/love.tm> contains the macros of this
  chapter and some applications of them.

  <tmdoc-copyright|1998\U2026|Joris van der Hoeven>

  <tmdoc-license|Permission is granted to copy, distribute and/or modify this
  document under the terms of the GNU Free Documentation License, Version 1.1
  or any later version published by the Free Software Foundation; with no
  Invariant Sections, with no Front-Cover Texts, and with no Back-Cover
  Texts. A copy of the license is included in the section entitled "GNU Free
  Documentation License".>
</body>

<initial|<\collection>
</collection>>
