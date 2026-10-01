<TeXmacs|1.0.3.10>

<style|tmdoc>

<\body>
  <tmdoc-title|Environment primitives>

  The current environment both defines all style parameters which affect the
  typesetting process and all additional macros provided by the user and the
  current style. The primitives in this section are used to access and modify
  environment variables.

  <\explain>
    <explain-macro|assign|var|val><explain-synopsis|variable mutation>
  <|explain>
    This primitive sets the environment variable named <src-arg|var> (string
    value) to the value of the <src-arg|val> expression. This primitive is
    used to make non-scoped changes to the environment, like defining markup
    or increasing counters.

    This primitive affects the evaluation process <emdash>through
    <markup|value>, <markup|provides>, and macro definitions<emdash> and the
    typesetting process <emdash>through special typesetter variables.

    <\example>
      Enabling page breaking by style.

      The <verbatim|page-medium> is used to enable page breaking. Since only
      the <re-index|initial environment> value for this variable is
      effective, this assignation must occur in a style file, not within a
      document.

      <\tm-fragment>
        <inactive*|<assign|page-medium|paper>>
      </tm-fragment>
    </example>

    <\example>
      Setting the chapter counter.

      The following snippet will cause the immediately following chapter to
      be number 3. This is useful to get the the numbering right in
      <verbatim|book> style when working with projects and <markup|include>.

      <\tm-fragment>
        <inactive*|<assign|chapter-nr|2>>
      </tm-fragment>
    </example>

    The <src-arg|var> operand is evaluated and must yield a string. When
    <src-arg|var> designates a paragraph or page layout variable, the change
    is transmitted to the paragraph or page typesetter.
  </explain>

  <\explain>
    <explain-macro|provide|var|val><explain-synopsis|default definition>
  <|explain>
    This primitive is similar to <markup|assign>, except that the assignment
    only takes place if the environment variable <src-arg|var> is not yet
    defined. It is useful for providing default definitions in packages,
    which can be overridden by previously loaded packages.
  </explain>

  <\explain>
    <explain-macro|with|var-1|val-1|<with|mode|math|\<cdots\>>|var-n|val-n|body><explain-synopsis|variable
    scope>
  <|explain>
    This primitive temporarily sets the environment variables <src-arg|var-1>
    until <src-arg|var-n> (in this order) to the evaluated values of
    <src-arg|val-1> until <src-arg|val-n> and typesets <src-arg|body> in this
    modified environment. All non-scoped change done with <markup|assign> to
    <src-arg|var-1> until <src-arg|var-n> within <src-arg|body> are reverted
    at the end of the <markup|with>.

    This primitive is used extensively in style files to modify the
    typesetter environment. For example to locally set the text font, the
    paragraph style, or the mode for mathematics.
  </explain>

  <\explain>
    <explain-macro|value|var><explain-synopsis|variable value>
  <|explain>
    This primitive evaluates the current value of the environment variable
    <src-arg|var> (whose name is itself evaluated). This is useful to
    display counters and generally to implement environment-sensitive
    behavior. See also <markup|quote-value> for retrieving the value
    without evaluating it.
  </explain>

  <\explain>
    <explain-macro|or-value|var-1|<math|\<cdots\>>|var-n><explain-synopsis|first
    defined value>
  <|explain>
    Evaluates to the value of the first variable among <src-arg|var-1> until
    <src-arg|var-n> which is defined and not uninitialized; the last
    variable is taken if it is defined. If none of the variables is defined,
    then the result is the empty string.
  </explain>

  <\explain>
    <explain-macro|provides|var><explain-synopsis|definition predicate>
  <|explain>
    This predicate evaluates to <verbatim|true> if the environment variable
    <src-arg|var> (string value) is defined, and to <verbatim|false>
    otherwise.

    That is useful for modular markup, like the <markup|session>
    environments, to fall back to a default appearance when a required
    package is not used in the document.
  </explain>

  <\explain>
    <explain-macro|new-theme|theme|var-1|<math|\<cdots\>>|var-n>

    <explain-macro|copy-theme|theme|theme-1|<math|\<cdots\>>|theme-n>

    <explain-macro|select-theme|theme|theme-1|<math|\<cdots\>>|theme-n>

    <explain-macro|apply-theme|theme><explain-synopsis|themes>
  <|explain>
    Themes are named collections of values for environment variables, which
    are used for instance by the presentation and poster styles. The
    <markup|new-theme> primitive declares a new <src-arg|theme> for the
    variables <src-arg|var-1> until <src-arg|var-n>: for each variable
    <verbatim|v> it saves the current value in a variable
    <verbatim|<src-arg|theme>-v>, and it defines a macro
    <markup|with-<src-arg|theme>> which sets <verbatim|v> to the value of
    <verbatim|<src-arg|theme>-v>. When some <src-arg|var-i> is itself the
    name of a theme (that is, <markup|with-<src-arg|var-i>> exists), then
    the new theme inherits from it. The <markup|copy-theme> primitive
    creates a new <src-arg|theme> with the same variables and values as the
    existing themes <src-arg|theme-1> until <src-arg|theme-n>, and
    <markup|select-theme> sets the values of the variables of the existing
    <src-arg|theme> to those of <src-arg|theme-1> until <src-arg|theme-n>.
    Finally, <markup|apply-theme> assigns the values of a <src-arg|theme>
    to the corresponding environment variables, in a non-scoped way. The
    names of the themes are literal strings. These primitives are
    implemented in <verbatim|Typeset/Env/env_exec.cpp>
    (<cpp|edit_env_rep::exec_new_theme> and following).
  </explain>

  For more information on how the environment is implemented, see the
  chapter on <hlink|macro expansion|../../source/macro-expansion.en.tm>, and
  in particular the section on <hlink|the typesetting
  environment|../../source/macro-expansion-env.en.tm>.

  <tmdoc-copyright|2004|David Allouche|Joris van der Hoeven>

  <tmdoc-license|Permission is granted to copy, distribute and/or modify this
  document under the terms of the GNU Free Documentation License, Version 1.1
  or any later version published by the Free Software Foundation; with no
  Invariant Sections, with no Front-Cover Texts, and with no Back-Cover
  Texts. A copy of the license is included in the section entitled "GNU Free
  Documentation License".>
</body>