<TeXmacs|1.99.8>

<style|<tuple|tmdoc|old-spacing>>

<\body>
  <tmdoc-title|Internal primitives>

  The primitives in this section are merely for internal use by <TeXmacs>
  only. They are documented for the sake of completeness, but you should only
  use them if you really know what you are doing.

  <\explain|<explain-macro|uninit><explain-synopsis|unknown content or
  uninitialized data>>
    This primitive is mainly used for default uninitialized value of
    environment variables; the main advantage of this tag is to be distinct
    from the empty string.
  </explain>

  <\explain|<explain-macro|unknown><explain-synopsis|unknown content or
  uninitialized data>>
    This primitive is mainly used for default uninitialized value of
    environment variables; the main advantage of this tag is to be distinct
    from the empty string.

    This value is less likely to be encountered than <src-macro|uninit>
  </explain>

  <\explain|<explain-macro|error|message><explain-synopsis|error messages>>
    This primitive should never appear in documents. It is provided as aid in
    tracking down invalid constructs. It is produced at evaluation time by
    any kind of primitive which is given improper operands.
  </explain>

  <\explain>
    <explain-macro|collection|binding-1|<math|\<cdots\>>|binding-n>

    <explain-macro|associate|key|value><explain-synopsis|collections of
    bindings>
  <|explain>
    The <markup|collection> tag is used to represent hash tables with
    bindings <src-arg|binding-1> until <src-arg|binding-n>. Each binding is
    of the form <explain-macro|associate|key|value>, with a <src-arg|key> and
    an associated <src-arg|value>.
  </explain>

  <\explain>
    <explain-macro|attr|key-1|val-1|<math|\<cdots\>>|key-n|val-n><explain-synopsis|XML-like
    attributes>
  <|explain>
    This tag is included for future compatibility with XML. It is used for
    encoding XML-style attributes by <TeXmacs> trees. For instance, the
    fragment

    <\quote-env>
      <framed-fragment|<\verbatim>
        \<less\>blah color="blue" emotion="verbose"\<gtr\>

        \ \ Some XML stuff

        \<less\>/blah\<gtr\>
      </verbatim>>
    </quote-env>

    would typically be represented as

    <\tm-fragment>
      <inactive*|<blah|<attr|color|blue|emotion|verbose>|Some XML stuff>>
    </tm-fragment>
  </explain>

  <\explain>
    <explain-macro|tag|id|body><explain-synopsis|attach a label to some
    content>
  <|explain>
    This built-in macro (defined in <verbatim|Typeset/Env/env_default.cpp>)
    adds the label <src-arg|id> to the <src-var|the-tags> environment
    variable while typesetting <src-arg|body>. The next
    <explain-macro|set-binding|value> inside <src-arg|body> (typically
    issued by a numbered environment) then binds <src-arg|id> to
    <src-arg|value>, so that <src-arg|id> can be referred to using
    <markup|reference>.
  </explain>

  <\explain>
    <explain-macro|backup|save|stack><explain-synopsis|save values on stack>
  <|explain>
    Used to represent temporarily saved values on a stack.
  </explain>

  <\explain>
    <explain-macro|dbox><explain-synopsis|marker for decorations>
  <|explain>
    This primitive is only intended for internal use by the <markup|datoms>,
    <markup|dlines> and <markup|dpages> primitives.
  </explain>

  <\explain>
    <explain-macro|rewrite-inactive|t|var><explain-synopsis|internal
    primitive for rendering inactive markup>
  <|explain>
    This internal primitive is used for rewriting an inactive tree into a new
    tree whose rendering corresponds to the rendering of the inactive tree.
    The first argument must be a macro argument <explain-macro|arg|x> (with
    optional indices). The second argument is a literal string which
    specifies the rewriting mode: <verbatim|once> (only the root is shown
    inactive, as for <markup|inactive>), <verbatim|recurse> (the whole tree
    is shown inactive, as for <markup|inactive*>; this is also the default)
    or <verbatim|error> (as for <markup|error>). The variants
    <verbatim|once*>, <verbatim|recurse*> and <verbatim|error*> are used
    for block content.
    It may be successfully invoked from within a macro.

    e.g. <inactive*|<assign|show-inactive|<macro|x|<rewrite-inactive|<arg|x>|>>>>

    which might be invoked to show itself or another assigned variable using
    <src-macro|quasiquote> in this manner:
    <inactive*|<quasiquote|<show-inactive|<unquote|<value|show-inactive>>>>>

    \;
  </explain>

  <\explain>
    <explain-macro|freeze|content>

    <explain-macro|unfreeze|content><explain-synopsis|frozen content>
  <|explain>
    The <src-arg|content> of a <markup|freeze> tag is typeset as usual, but
    as a decoration, so that it cannot be edited; <markup|unfreeze> can be
    used inside frozen content in order to make parts of it editable again.
  </explain>

  <\explain>
    <explain-macro|hidden-binding|keys|value><explain-synopsis|result of
    <markup|set-binding>>
  <|explain>
    This tag is returned by the evaluation of <markup|set-binding>; it is
    not rendered.
  </explain>

  <\explain>
    <explain-macro|cache-ref|hash|kind>

    <explain-macro|cache-ref|hash|kind|width|height><explain-synopsis|placeholder
    for cached content>
  <|explain>
    Placeholder for content (of a given <src-arg|kind>, such as
    <verbatim|image> or <verbatim|graphics>) which is stored elsewhere
    under the key <src-arg|hash> and which has not been loaded yet. It is
    rendered as a provisional box, whose size may be given by the optional
    <src-arg|width> and <src-arg|height>.
  </explain>

  <\explain>
    <explain-macro|identity|markup><explain-synopsis|identity macro>
  <|explain>
    The identity macro is built-in into <TeXmacs>. It should not really be
    considered as a primitive, but it is not part of any style file either.
  </explain>

  In addition to these primitives for internal use only, there are also quite
  a few obsolete primitives, which are no longer being used by <TeXmacs>, but
  whose names should be avoided when creating your own macros. The full list
  of obsolete primitives is: <markup|format>, <markup|line-sep>,
  <markup|split>, <markup|delay>, <markup|hold>, <markup|release>,
  <markup|old-matrix>, <markup|old-table>, <markup|old-mosaic>,
  <markup|old-mosaic-item>, <markup|set>, <markup|reset>, <markup|expand>,
  <markup|expand*>, <markup|hide-expand>, <markup|apply>, <markup|begin>,
  <markup|end>, <markup|func>, <markup|env>, <markup|authorize>,
  <markup|postscript>.

  <tmdoc-copyright|2004|Joris van der Hoeven>

  <tmdoc-license|Permission is granted to copy, distribute and/or modify this
  document under the terms of the GNU Free Documentation License, Version 1.1
  or any later version published by the Free Software Foundation; with no
  Invariant Sections, with no Front-Cover Texts, and with no Back-Cover
  Texts. A copy of the license is included in the section entitled "GNU Free
  Documentation License".>
</body>

<initial|<\collection>
</collection>>