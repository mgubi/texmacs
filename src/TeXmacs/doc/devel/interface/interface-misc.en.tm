<TeXmacs|2.1.4>

<style|<tuple|tmdoc|english>>

<\body>
  <tmdoc-title|Miscellaneous features>

  Several other features are supported in order to write interfaces between
  <TeXmacs> and extern applications. Some of these are very hairy or quite
  specific. Let us briefly describe a few miscellaneous features:

  <paragraph*|Interrupts>

  The \Pstop\Q icon (or the <menu|Interrupt execution> entry of the
  session menus) can be used in order to interrupt the evaluation of some
  input. When pressing this button, <TeXmacs> will just send a
  <verbatim|SIGINT> signal to the process group of your application (this
  is not implemented under <name|Windows>). It expects your application to
  finish the output as usual. In particular, you should close all open
  <render-key|DATA_BEGIN>-blocks.

  The <menu|Close session> entry terminates the application by sending
  <verbatim|SIGTERM> to its process group, followed by <verbatim|SIGKILL>
  two seconds later.

  <paragraph*|Testing whether the input is complete>

  Some systems start a multiline input mode as soon as you start to define a
  function or when you enter an opening bracket without a matching closing
  bracket. <TeXmacs> allows your application to implement a special predicate
  for testing whether the input is complete. First of all, this requires you
  to specify the configuration option

  <\scm-code>
    (:test-input-done #t)
  </scm-code>

  As soon as you will press <shortcut|(kbd-return)> in your input, <TeXmacs> will
  then send the command

  <\quotation>
    <\framed-fragment>
      <\verbatim>
        <render-key|DATA_COMMAND>(input-done? <em|input-string>)<shortcut|(kbd-return)>
      </verbatim>
    </framed-fragment>
  </quotation>

  Your application should reply with a message of the form

  <\quotation>
    <\framed-fragment>
      <verbatim|<render-key|DATA_BEGIN>scheme:<em|done><render-key|DATA_END>>
    </framed-fragment>
  </quotation>

  where <verbatim|<em|done>> is either <scm|#t> or <scm|#f>. The
  <verbatim|<em|input-string>> is a quoted <scheme> string, obtained by
  serializing the input with the serializer of the plug-in (without the
  final newline), and the command is sent using the same mechanism as
  tab-completion requests (so that it can be customized using the
  <scm|:commander> option). The
  <verbatim|multiline> plug-in provides an example of this mechanism (see in
  particular the file <example-plugin-link|multiline/src/multiline.cpp>).

  <tmdoc-copyright|1998--2002|Joris van der Hoeven>

  <tmdoc-copyright|2026|the <TeXmacs> team>

  <tmdoc-license|Permission is granted to copy, distribute and/or modify this
  document under the terms of the GNU Free Documentation License, Version 1.1
  or any later version published by the Free Software Foundation; with no
  Invariant Sections, with no Front-Cover Texts, and with no Back-Cover
  Texts. A copy of the license is included in the section entitled "GNU Free
  Documentation License".>
</body>

<initial|<\collection>
</collection>>