<TeXmacs|1.99.8>

<style|<tuple|tmdoc|old-spacing>>

<\body>
  <tmdoc-title|Meta information and logical programming>

  Small software projects usually consist of a collection of routines and
  data. In a large software project, where a typical contributor has no
  complete overview of the program, it is a good practice to associate
  additional <em|meta-information> to the individual routines and data. This
  meta-information typically serves documentation purposes, but becomes even
  more interesting if it can be used in an automated fashion to implement
  more general additional functionality.

  The <scm|tm-define> macro supports several options for associating
  meta-information to <scheme> functions and symbols. For instance, the
  <scm|:synopsis>, <scm|:argument> and <scm|:returns> options allow you to
  associate short documentation strings to the function, its arguments and
  its return value:

  <\scm-code>
    (tm-define (square x)

    \ \ (:synopsis "Compute the square of @x")

    \ \ (:argument x "A number")

    \ \ (:returns "The square of @x")

    \ \ (* x x))
  </scm-code>

  This information is exploited by <TeXmacs> in several ways. For instance,
  the synopsis of the function can be retrieved by executing <scm|(help
  square)>. More interestingly, assuming that we defined <scm|square> as
  above, typing <shortcut|(interactive exec-interactive-command)> followed by
  <scm|square> and <shortcut|(kbd-return)> allows you to execute <scm|square>
  in an interactive way: you will be prompted for \PA number\Q on the footer.
  Moreover, after typing <shortcut|(interactive exec-interactive-command)>,
  you will be able to use \Ptab-completion\Q in order to enter <scm|square>:
  typing <key|s q u tab> will usually complete into<nbsp><scm|square>.

  In a similar vein, the <scm|:interactive> and <scm|:check-mark> options
  allow you to specify that a given routine requires interactive user input
  or when it should give rise to a check-mark when used in a menu. For
  instance, the statement

  <\scm-code>
    (tm-property (open-document-page-format)

    \ \ (:interactive #t))
  </scm-code>

  in the source code of <TeXmacs> (see <source-link|init-texmacs.scm|TeXmacs/progs/init-texmacs.scm>) states
  that <scm|open-document-page-format> is an interactive command. As a
  consequence, the <menu|Document|Page> entry, which is defined by

  <\scm-code>
    ("Page" (open-document-page-format))
  </scm-code>

  will be followed by dots <scm|...> in the <menu|Document> menu. The
  interesting point here is that, although the command
  <scm|open-document-page-format> may be reused
  several times in different menu entries, we only have to specify once that
  it is an interactive command. Similarly, consider the definition

  <\scm-code>
    (tm-define (toggle-session-math-input)

    \ \ (:synopsis "Toggle mathematical input in sessions")

    \ \ (:check-mark "v" session-math-input?)

    \ \ ...)
  </scm-code>

  Given a menu item with <scm|(toggle-session-math-input)> as its associated
  action, this definition specifies in particular that a check-mark should be
  displayed before the menu item whenever the <scm|session-math-input?>
  predicate holds.

  Another frequently used option is <scm|:secure>, which specifies that a
  given routine can be used inside <TeXmacs> documents, in particular inside
  <markup|extern> and <markup|action> macros. For instance, the default
  implementation of the <markup|folded> tag allows the user to click on the
  \P<math|<op|\<circ\>>>\Q before the folded text so as to unfold the tag.
  When doing this, the scheme script <scm|mouse-unfold> is launched. However,
  for this to work, the <scm|mouse-unfold> function needs to be secure (see
  <source-link|utils/edit/variants.scm|TeXmacs/progs/utils/edit/variants.scm>):

  <\scm-code>
    (tm-define (mouse-unfold t)

    \ \ (:type (-\<gtr\> void))

    \ \ (:synopsis "Unfold using the mouse")

    \ \ (:secure #t)

    \ \ (when (tree-\<gtr\>path t)

    \ \ \ \ (tree-go-to t :start)

    \ \ \ \ (when (tree-up t)

    \ \ \ \ \ \ (alternate-unfold (tree-up t)))))
  </scm-code>

  You can read more about the tags which depend on <scheme> scripts in
  \P<hlink|Invoking <scheme> scripts from <TeXmacs>
  markup|overview-start.en.tm#markup-scripts>\Q.\ 

  The complete list of options currently understood by <scm|tm-define> and
  <scm|tm-property> (see <source-link|kernel/texmacs/tm-define.scm|TeXmacs/progs/kernel/texmacs/tm-define.scm>) is:
  <scm|:mode> and <scm|:require> (for <hlink|contextual
  overloading|overview-overloading.en.tm>), <scm|:applicable> (a condition
  under which a menu entry for the command is active, rather than greyed
  out), <scm|:synopsis>, <scm|:synopsis*>, <scm|:returns>, <scm|:note>,
  <scm|:type>, <scm|:argument>, <scm|:default> and <scm|:proposals> (a
  default value <abbr|resp.> a list of proposals for an argument when the
  command is executed interactively), <scm|:secure>, <scm|:check-mark>,
  <scm|:interactive> and <scm|:balloon> (a tooltip for menu entries).
  Unknown options raise an error.

  In the future, the number of options for entering meta-information is
  likely to increase. <TeXmacs> also supports an additional mechanism for the
  automatic deduction of new meta-properties from existing meta-properties.
  This mechanism is based on a less general, but more efficient form of
  <em|logical programming>, which is described in the section on
  <hlink|logical programming|../utils/utils-logic.en.tm>.

  <tmdoc-copyright|2005|Joris van der Hoeven>

  <tmdoc-license|Permission is granted to copy, distribute and/or modify this
  document under the terms of the GNU Free Documentation License, Version 1.1
  or any later version published by the Free Software Foundation; with no
  Invariant Sections, with no Front-Cover Texts, and with no Back-Cover
  Texts. A copy of the license is included in the section entitled "GNU Free
  Documentation License".>
</body>

<initial|<\collection>
</collection>>