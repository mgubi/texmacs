<TeXmacs|1.0.7.21>

<style|tmdoc>

<\body>
  <tmdoc-title|Linking primitives>

  The most common linking tags, <markup|label>, <markup|reference>,
  <markup|pageref>, <markup|hlink>, <markup|action> and <markup|include>,
  are actually built-in macros defined in the default environment
  (<source-link|Typeset/Env/env_default.cpp|src/Typeset/Env/env_default.cpp>) in terms of the lower level
  primitives <markup|locus>, <markup|id>, <markup|link>, <markup|url>,
  <markup|script>, <markup|set-binding>, <markup|get-binding> and
  <markup|include*>, which are described at the end of this section. For
  instance, <markup|hlink> is defined as

  <\tm-fragment>
    <inactive*|<assign|hlink|<macro|body|destination|<locus|<id|<hard-id|<arg|body>>>|<link|hyperlink|<id|<hard-id|<arg|body>>>|<url|<arg|destination>>>|<arg|body>>>>>
  </tm-fragment>

  <\explain>
    <explain-macro|label|name><explain-synopsis|reference target>
  <|explain>
    The operand must evaluate to a literal string, it is used as a target
    name which can be referred to by <markup|reference>, <markup|pageref> and
    <markup|hlink> tags.

    Label names should be unique in a document and in a project.

    Examples in this section will make references to an example
    <markup|label> named ``there''.

    <\tm-fragment>
      <inactive*|<label|there>>
    </tm-fragment>
  </explain>

  <\explain>
    <explain-macro|reference|name><explain-synopsis|reference to a name>
  <|explain>
    The operand must evaluate to a literal string, which is the name of a
    <markup|label> defined in the current document or in another document of
    the current project.

    <\tm-fragment>
      <inactive*|<reference|there>>
    </tm-fragment>

    The <markup|reference> is typeset as the value of the variable
    <src-var|the-label> at the point of the target <markup|label>. The
    <src-var|the-label> variable is set by many numbered structures:
    sections, figures, numbered equations, <no-break>etc.

    A <markup|reference> reacts to mouse clicks as an hyperlink.
  </explain>

  <\explain>
    <explain-macro|pageref|name><explain-synopsis|page reference to a name>
  <|explain>
    The operand must evaluate to a literal string, which is the name of a
    <markup|label> defined in the current document or in another document of
    the current project.

    <\tm-fragment>
      <inactive*|<pageref|there>>
    </tm-fragment>

    The <markup|><markup|pageref> is typeset as the number of the page
    containing the target <markup|label>. Note that page numbers are only
    computed when the document is typeset with page-breaking, that is not in
    ``automatic'' or ``papyrus'' page type.

    A <markup|pageref> reacts to mouse clicks as an hyperlink.
  </explain>

  <\explain>
    <explain-macro|hlink|content|url><explain-synopsis|inline hyperlink>
  <|explain>
    This primitive produces an hyperlink with the visible text
    <src-arg|content> pointing to <src-arg|url>. The <src-arg|content> is
    typeset as inline <src-arg|url>. The <src-arg|url> must evaluate to a
    literal string in <abbr|URL> syntax and can point to local or remote
    documents, positions inside documents can be be specified with labels.

    The following examples are typeset as hyperlinks pointing to the label
    ``there'', respectively in the same document, in a document in the same
    directory, and on the web.

    <\tm-fragment>
      <inactive*|<hlink|same document|#there>>

      <inactive*|<hlink|same directory|file.tm#there>>

      <inactive*|<hlink|on the web|http://example.org/#there>>
    </tm-fragment>

    If the document is not editable, the hyperlink is traversed by a simple
    click, if the document is editable, a double-click is required.
  </explain>

  <\explain>
    <explain-macro|include|url><explain-synopsis|include another document>
  <|explain>
    The operand is evaluated and interpreted as a file name, relative to the
    current document. The body of this file is typeset in place of the
    <markup|include> tag, which must be placed in <re-index|block context>.
    The <markup|include> tag is a built-in macro which expands to
    <explain-macro|include*|url>; the <markup|include*> primitive loads the
    file (<cpp|edit_env_rep::rewrite> in
    <source-link|Typeset/Env/env_exec.cpp|src/Typeset/Env/env_exec.cpp>) and refuses to include the current
    document itself.
  </explain>

  <\explain>
    <explain-macro|action|content|script>

    <explain-macro|action|content|script|arg-1|<math|\<cdots\>>|arg-n><explain-synopsis|attach
    an action to content>
  <|explain>
    Bind a <scheme> <src-arg|script> to a mouse click on <src-arg|content>
    (as for hyperlinks, a double click is needed in editable documents).
    Optional arguments <src-arg|arg-1> until <src-arg|arg-n> are passed to
    the script; they are transmitted through <markup|find-accessible>, so
    that the script receives the accessible document subtrees which
    correspond to them. For instance, when clicking <action|here|(lambda ()
    (system "xterm &"))>, you may launch an <verbatim|xterm>. This action is
    encoded by

    <\tm-fragment>
      <inactive*|<action|here|(lambda () (system "xterm &"))>>
    </tm-fragment>

    When clicking on actions, the user is usually prompted for confirmation,
    so as to avoid security problems. The user may control the desired level
    of security in <menu|Edit|Preferences|Security>. Programmers may also
    declare certain <scheme> routines to be ``secure''. <scheme> programs
    which only use secure routines are executed without confirmation from the
    user.
  </explain>

  <paragraph|Low level linking primitives>

  <\explain>
    <explain-macro|locus|link-1|<math|\<cdots\>>|link-n|body><explain-synopsis|linkable
    content>
  <|explain>
    Typeset <src-arg|body> and declare it as a <em|locus>, that is, a part
    of the document which participates in links. Each of the arguments
    <src-arg|link-1> until <src-arg|link-n> is evaluated and should be an
    identifier <explain-macro|id|name>, a link
    <explain-macro|link|type|participant-1|<math|\<cdots\>>|participant-n>
    or an observer <explain-macro|observer|id|call-back>. The identifiers
    and links are registered in the linking environment of the editor, and
    the <src-arg|body> is rendered as an active hyperlink region
    (<verbatim|concater_rep::typeset_locus> in
    <source-link|Typeset/Concat/concat_active.cpp|src/Typeset/Concat/concat_active.cpp>).
  </explain>

  <\explain>
    <explain-macro|id|name><explain-synopsis|identifier of a locus>
  <|explain>
    Represents the unique identifier <src-arg|name> of a locus. Identifiers
    are usually generated by <markup|hard-id>, or derived from the names of
    labels (the <markup|label> macro uses <inactive*|<id|<merge|#|<arg|Id>>>>
    as an anchor).
  </explain>

  <\explain>
    <explain-macro|hard-id>

    <explain-macro|hard-id|content><explain-synopsis|unique identifier>
  <|explain>
    Evaluates to a string which uniquely identifies <src-arg|content> (or,
    without argument, the current typesetting environment). The identifier
    is computed from the memory addresses of the typesetting environment and
    of the source subtree obtained by expanding <src-arg|content> (see
    <markup|find-accessible>), so it should never be stored in documents.
  </explain>

  <\explain>
    <explain-macro|link|type|participant-1|<math|\<cdots\>>|participant-n><explain-synopsis|typed
    link>
  <|explain>
    Declares a link of a given <src-arg|type> (like <verbatim|hyperlink>,
    <verbatim|action> or <verbatim|anchor>) between the participants
    <src-arg|participant-1> until <src-arg|participant-n>. The participants
    are identifiers (<markup|id>), urls (<markup|url>) or scripts
    (<markup|script>). A link only has an effect when it occurs as one of the
    first arguments of a <markup|locus>.
  </explain>

  <\explain>
    <explain-macro|url|destination>

    <explain-macro|url|destination|location><explain-synopsis|link
    destination>
  <|explain>
    Represents an external or internal <src-arg|destination>, in <abbr|URL>
    syntax, as a participant of a <markup|link>.
  </explain>

  <\explain>
    <explain-macro|script|function|arg-1|<math|\<cdots\>>|arg-n><explain-synopsis|script
    to be executed>
  <|explain>
    Represents a <scheme> <src-arg|function>, together with its arguments,
    as a participant of a <markup|link>. The function and the arguments are
    evaluated.
  </explain>

  <\explain>
    <explain-macro|observer|id|call-back><explain-synopsis|call-back on
    changes>
  <|explain>
    When used as an argument of <markup|locus>, register the locus under
    the identifier <src-arg|id>, together with the name of a <scheme>
    <src-arg|call-back> function which is invoked by the linking
    environment. The call-back is only registered if it is secure.
  </explain>

  <\explain>
    <explain-macro|relay|content|arg-1|<math|\<cdots\>>|arg-n><explain-synopsis|relay
    events>
  <|explain>
    Typeset <src-arg|content> inside a special box which transmits the
    (evaluated) arguments <src-arg|arg-1> until <src-arg|arg-n> together
    with the mouse and keyboard events it receives.
  </explain>

  <\explain>
    <explain-macro|set-binding|key|value>

    <explain-macro|set-binding|value><explain-synopsis|bind a label>
  <|explain>
    The first form associates the evaluated <src-arg|value> to the label
    <src-arg|key>, together with the current page number; this binding is
    later retrieved by <markup|get-binding>. The <markup|label> macro is
    defined as a <markup|locus> containing
    <inactive*|<set-binding|<arg|Id>|<value|the-label>>>. The second form
    binds <src-arg|value> to all the keys which have been accumulated in the
    <src-var|the-tags> environment variable (see the <markup|tag> macro), and
    sets <src-var|the-label> to <src-arg|value>. Bindings which are made in
    an included file or in another part of a project also remember the
    corresponding location. When evaluated, <markup|set-binding> returns an
    internal <markup|hidden-binding> tag.
  </explain>

  <\explain>
    <explain-macro|get-binding|key>

    <explain-macro|get-binding|key|kind><explain-synopsis|retrieve a
    binding>
  <|explain>
    Returns the value associated to the label <src-arg|key>. When
    <src-arg|kind> is <verbatim|1>, the page number of the label is returned
    instead. The <markup|reference> and <markup|pageref> macros are
    implemented using <inactive*|<get-binding|<arg|Id>>> and
    <inactive*|<get-binding|<arg|Id>|1>>. Undefined labels are recorded as
    missing references when the <src-var|warn-missing> variable is set.
  </explain>

  <\explain>
    <explain-macro|has-binding|key>

    <explain-macro|has-binding|key|kind><explain-synopsis|test for a
    binding>
  <|explain>
    Returns <verbatim|true> if a value (or a page number, when
    <src-arg|kind> is <verbatim|1>) is associated to the label
    <src-arg|key>, and <verbatim|false> otherwise.
  </explain>

  <\explain>
    <explain-macro|get-attachment|name><explain-synopsis|retrieve auxiliary
    data>
  <|explain>
    Returns the auxiliary data attached to the current document (or the
    master document of the project) under the given <src-arg|name>.
  </explain>

  <\explain>
    <explain-macro|toc-notify|kind|title><explain-synopsis|table of contents
    entry for the viewer>
  <|explain>
    Produces an invisible box which informs the renderer about a sectional
    entry of a given <src-arg|kind> with a given <src-arg|title>. It is used
    by the <name|PDF> renderer for generating the outline (bookmarks) of
    exported documents.
  </explain>

  <tmdoc-copyright|2004|David Allouche|Joris van der Hoeven>

  <tmdoc-license|Permission is granted to copy, distribute and/or modify this
  document under the terms of the GNU Free Documentation License, Version 1.1
  or any later version published by the Free Software Foundation; with no
  Invariant Sections, with no Front-Cover Texts, and with no Back-Cover
  Texts. A copy of the license is included in the section entitled "GNU Free
  Documentation License".>
</body>

<initial|<\collection>
</collection>>