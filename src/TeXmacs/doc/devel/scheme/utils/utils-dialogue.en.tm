<TeXmacs|2.1.4>

<style|<tuple|tmdoc|english>>

<\body>
  <tmdoc-title|Interactive dialogues>

  The routines on this page allow <scheme> programs to ask simple questions
  to the user and to display messages. They are defined in
  <source-link|kernel/texmacs/tm-dialogue.scm|TeXmacs/progs/kernel/texmacs/tm-dialogue.scm> and
  <source-link|kernel/gui/menu-widget.scm|TeXmacs/progs/kernel/gui/menu-widget.scm>. Since the graphical user interface
  is event driven, the answers are passed to continuations rather than
  returned. More complex dialogues can be built using widgets, as explained
  in <hlink|dialogs and composite
  widgets|../gui/scheme-gui-dialogs.en.tm>.

  <\explain>
    <scm|(user-ask <scm-arg|prompt> <scm-arg|cont>)><explain-synopsis|ask a
    question>
  <|explain>
    Prompt the user with the string <scm-arg|prompt> (in the footer or in a
    dialogue window, depending on the preferences) and call
    <scm-arg|cont> with the answer (a string). Instead of a string,
    <scm-arg|prompt> may also be a list <scm|(<scm-arg|prompt>
    <scm-arg|type> <scm-arg|proposal> ...)>, where <scm-arg|type> is as in
    the <scm|:argument> option of <scm|tm-define> (see <hlink|function
    definitions|utils-overload.en.tm>) and the proposals are the suggested
    answers.
  </explain>

  <\explain>
    <scm|(user-confirm <scm-arg|prompt> <scm-arg|default>
    <scm-arg|cont>)><explain-synopsis|ask for confirmation>
  <|explain>
    Ask a yes/no question and call <scm-arg|cont> with a boolean. The
    boolean <scm-arg|default> determines whether <scm|"yes"> or <scm|"no">
    is proposed first. For instance:

    <\scm-code>
      (user-confirm "Really close the document?" #f

      \ \ (lambda (answ) (when answ (buffer-close (current-buffer)))))
    </scm-code>
  </explain>

  <\explain>
    <scm|(user-url <scm-arg|prompt> <scm-arg|type>
    <scm-arg|cont>)><explain-synopsis|ask for a file name>
  <|explain>
    Open a file chooser with title <scm-arg|prompt> and call
    <scm-arg|cont> with the chosen <abbr|URL>. The <scm-arg|type> can for
    instance be <scm|"texmacs">, <scm|"image"> or <scm|"directory">; see
    <scm|choose-file> in <source-link|kernel/boot/abbrevs.scm|TeXmacs/progs/kernel/boot/abbrevs.scm>.
  </explain>

  <\explain>
    <scm|(interactive <scm-arg|fun>)>

    <scm|(interactive <scm-arg|fun> <scm-arg|arg-1> ...
    <scm-arg|arg-n>)><explain-synopsis|call a function interactively>
  <|explain>
    Prompt the user for the arguments of the function <scm-arg|fun> and
    then call it. In the first form, the prompts, types and default values
    of the arguments are taken from the <scm|:argument>, <scm|:default> and
    <scm|:proposals> options of the definition of <scm-arg|fun>. In the
    second form, each <scm-arg|arg-i> is a prompt string or a list
    <scm|(<scm-arg|prompt> <scm-arg|type> <scm-arg|proposal> ...)>. Answers
    are learned, so that they can be proposed again next time (see also
    <scm|learn-interactive> and <scm|forget-interactive>). This is the
    usual way to bind commands which require arguments in menus, as in
    <scm|(interactive load-buffer)>.
  </explain>

  <\explain>
    <scm|(set-message <scm-arg|left> <scm-arg|right>)>

    <scm|(set-temporary-message <scm-arg|left> <scm-arg|right>
    <scm-arg|ms>)><explain-synopsis|messages in the footer>
  <|explain>
    Display the message <scm-arg|left> in the footer, together with the
    information <scm-arg|right> (which usually describes the action that
    caused the message). The temporary variant restores the previous
    message after <scm-arg|ms> milliseconds of inactivity. The messages may
    be strings or <TeXmacs> content.
  </explain>

  <\explain>
    <scm|(show-message <scm-arg|msg> <scm-arg|title>)>

    <scm|(notify-now <scm-arg|msg>)><explain-synopsis|message windows>
  <|explain>
    Show the message <scm-arg|msg> in a small window with an <verbatim|Ok>
    button. The routine <scm|notify-now> uses the title
    <verbatim|Notification> and opens the window after the current event
    has been processed.
  </explain>

  <\explain>
    <scm|(dialogue-window <scm-arg|widget> <scm-arg|cmd>
    <scm-arg|title>)><explain-synopsis|dialogue windows>
  <|explain>
    Open a new window with the widget <scm-arg|widget> (defined using
    <scm|tm-widget>) and the given <scm-arg|title>. When the widget calls
    its continuation with some arguments, then <scm-arg|cmd> is applied to
    these arguments and the window is closed. See <hlink|dialogs and
    composite widgets|../gui/scheme-gui-dialogs.en.tm> for details and
    examples.
  </explain>

  Many dialogues need to be executed after the current event has been
  processed or after some delay. This can be done using the following
  macro.

  <\explain>
    <scm|(delayed <scm-arg|option> ... <scm-arg|body> ...)><explain-synopsis|delayed
    execution>
  <|explain>
    Schedule the evaluation of <scm-arg|body> after the current event has
    been processed. The following options modify when and how often
    <scm-arg|body> is evaluated:

    <\description>
      <item*|<scm|(:idle <scm-arg|ms>)>>Wait until the user has been
      inactive during <scm-arg|ms> milliseconds.

      <item*|<scm|(:pause <scm-arg|ms>)>>Wait <scm-arg|ms> milliseconds.

      <item*|<scm|(:every <scm-arg|ms>)>>Evaluate <scm-arg|body> every
      <scm-arg|ms> milliseconds (usually combined with <scm|:while> or
      <scm|:permanent>).

      <item*|<scm|(:on-cpu-idle <scm-arg|ms>)>>Wait at least <scm-arg|ms>
      milliseconds, and then until the computer has been idle during 30
      seconds; with <scm|:permanent>, this is used for periodic maintenance,
      such as the backups of the server.

      <item*|<scm|(:refresh <scm-arg|ms>)>>Meant to evaluate
      <scm-arg|body> after a change of the document, once the user has been
      inactive during <scm-arg|ms> milliseconds. It is not used in the
      sources, and its current implementation never evaluates
      <scm-arg|body>.

      <item*|<scm|(:require <scm-arg|cond>)>>Postpone the evaluation until
      <scm-arg|cond> holds.

      <item*|<scm|(:while <scm-arg|cond>)>>Repeat the evaluation as long as
      <scm-arg|cond> holds.

      <item*|<scm|(:permanent <scm-arg|cond>)>>Reschedule the evaluation
      after each execution as long as <scm-arg|cond> evaluates to
      <scm|#t>.

      <item*|<scm|(:clean <scm-arg|expr>)>>Evaluate <scm-arg|expr> once the
      task has finished.

      <item*|<scm|(:do <scm-arg|expr>)>>Evaluate <scm-arg|expr> each time
      the task is checked.
    </description>

    For instance, <scm|(delayed (:idle 1000) (set-message "Hi" ""))>
    displays a message after one second of inactivity. The macro is defined
    in <source-link|kernel/texmacs/tm-dialogue.scm|TeXmacs/progs/kernel/texmacs/tm-dialogue.scm>. Notice that <scm|:idle>
    does not work in headless mode.
  </explain>

  <tmdoc-copyright|2005--2026|Joris van der Hoeven, the <TeXmacs> team>

  <tmdoc-license|Permission is granted to copy, distribute and/or modify this
  document under the terms of the GNU Free Documentation License, Version 1.1
  or any later version published by the Free Software Foundation; with no
  Invariant Sections, with no Front-Cover Texts, and with no Back-Cover
  Texts. A copy of the license is included in the section entitled "GNU Free
  Documentation License".>
</body>

<initial|<\collection>
</collection>>
