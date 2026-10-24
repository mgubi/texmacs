<TeXmacs|2.1.4>

<style|<tuple|tmdoc|english>>

<\body>
  <tmdoc-title|A first macro>

  Let us define a macro <markup|hi> which produces the text \PHello
  world!\Q.

  <paragraph*|Defining the macro>

  The simplest way to define a macro is to start from an example. Type the
  text <verbatim|Hello world!> in your document, select it, and choose
  <menu|Tools|Macros|New macro>. The <em|macro editor> appears: it shows the
  body of the new macro, which is the selected text, below a field for the
  name of the macro:

  <\big-envbox>
    <inline-tag|<with|color|red|font-shape|italic|enter-name>>
    <math|\<assign\>>

    Hello world!
  </big-envbox>

  Click on the field <with|color|red|font-shape|italic|enter-name> and type
  the name <verbatim|hi>. You may still change the body, which can contain
  any kind of content: formatted text, formulas, tables, pictures... Then
  click on <menu|Ok>. The macro <markup|hi> is now defined in your document.

  If you open the macro editor without a selection, both the name and the
  body are empty, and you type them in the same way.

  <paragraph*|Using the macro>

  Now, wherever you want the text \PHello world!\Q to appear, type
  <key|\\ h i> and <key|return>. While you type the name, the status bar
  confirms that a macro <markup|hi> exists. You may also choose
  <menu|Insert|Macro|hi>: the <menu|Insert|Macro> menu lists all the macros
  of your document.

  When the cursor is next to the text produced by the macro, the status bar
  shows <verbatim|hi>: <TeXmacs> remembers that this text is an application
  of the macro, and not ordinary text. In particular, if you later change
  the definition of <markup|hi>, for instance into \PHello everybody!\Q, the
  text changes at once everywhere in your document. In order to change the
  definition, put the cursor next to an application of the macro and choose
  <menu|Focus|Preferences|Edit macro>, or use
  <menu|Tools|Macros|Edit macros>.

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
