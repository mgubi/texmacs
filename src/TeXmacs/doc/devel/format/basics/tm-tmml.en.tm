<TeXmacs|1.99.8>

<style|<tuple|tmdoc|english|old-spacing>>

<\body>
  <tmdoc-title|XML serialization>

  For compatibility reasons with the XML technology, <TeXmacs> also supports
  the serialization of <TeXmacs> documents in the XML format. However, the
  XML format is generally more verbose and less readable than the default
  <TeXmacs> format. In order to save or load a file in the XML format (using
  the <verbatim|.tmml> extension), you may use <menu|File|Export|Xml>
  <abbr|resp.> <menu|File|Import|Xml>. The conversion is implemented in
  <scheme> in the directory <source-link|convert/tmml/|TeXmacs/progs/convert/tmml> of
  <verbatim|$TEXMACS_PATH/progs>.

  It should be noticed that <TeXmacs> documents do not match a predefined
  DTD, since the appropriate DTD for a document depends on its style. The XML
  format therefore merely provides an XML representation for <TeXmacs> trees.
  The syntax has both been designed to be close to the tree structure and use
  conventional <acronym|XML> notations which are well supported by standard
  tools. A complete document is represented by a root element
  <verbatim|TeXmacs>, whose <verbatim|version> attribute contains the
  version of <TeXmacs>, and whose children represent the children of the
  top-level <markup|document> tag (<markup|style>, <markup|body>,
  <markup|initial>, <abbr|etc.>).

  <paragraph*|The encoding for strings>

  The leafs of <TeXmacs> trees are translated from the universal <TeXmacs>
  encoding into Unicode (UTF-8). Characters without Unicode equivalents
  are represented using a special <verbatim|tm-sym> tag; for instance, the
  <TeXmacs> symbol <verbatim|\<less\>foo\<gtr\>> would be represented as
  <verbatim|\<less\>tm-sym\<gtr\>foo\<less\>/tm-sym\<gtr\>>.

  <paragraph*|XML representation of regular tags>

  Trees with a single child are simply represented by the corresponding XML
  tag. In the case when a tree has several children, then each child is
  enclosed into a <verbatim|tm-arg> tag. For instance, <math|<sqrt|y+z>> is
  simply represented as

  <\quote-env>
    <framed-fragment|<verbatim|\<less\>sqrt\<gtr\>y+z\<less\>/sqrt\<gtr\>>>
  </quote-env>

  whereas the fraction <math|<frac|1|2>> is represented as

  <\quote-env>
    <\framed-fragment>
      <\with|par-par-sep|0fn>
        <\verbatim>
          \<less\>frac\<gtr\>

          \ \ \<less\>tm-arg\<gtr\>1\<less\>/tm-arg\<gtr\>

          \ \ \<less\>tm-arg\<gtr\>2\<less\>/tm-arg\<gtr\>

          \<less\>/frac\<gtr\>
        </verbatim>
      </with>
    </framed-fragment>
  </quote-env>

  In the above example, the whitespace is ignored. Whitespace may be
  preserved by setting the standard <verbatim|xml:space> attribute to
  <verbatim|preserve>.

  <paragraph*|Special tags>

  Some tags are represented in a special way in XML. The <markup|concat> tag
  is simply represented by a textual concatenation. For instance,
  <math|<frac|1|2>+<sqrt|y+z>> is represented as

  <\quote-env>
    <framed-fragment|<\verbatim>
      \<less\>frac\<gtr\>\<less\>tm-arg\<gtr\>1\<less\>/tm-arg\<gtr\>\<less\>tm-arg\<gtr\>2\<less\>/tm-arg\<gtr\>\<less\>/frac\<gtr\>+\<less\>sqrt\<gtr\>y+z\<less\>/sqrt\<gtr\>
    </verbatim>>
  </quote-env>

  The <markup|document> tag is not explicitly exported. Instead, each
  paragraph argument is enclosed within a <verbatim|tm-par> tag. For
  instance, the quotation

  <\quote-env>
    <\dutch>
      Ik ben de blauwbilgorgel.

      Als ik niet wok of worgel,
    </dutch>
  </quote-env>

  is represented as

  <\tm-fragment>
    <\verbatim>
      \<less\>quote-env\<gtr\>

      \ \ \<less\>tm-par\<gtr\>

      \ \ \ \ Ik ben de blauwbilgorgel.

      \ \ \<less\>/tm-par\<gtr\>

      \ \ \<less\>tm-par\<gtr\>

      \ \ \ \ Als ik niet wok of worgel,

      \ \ \<less\>/tm-par\<gtr\>

      \<less\>/quote-env\<gtr\>
    </verbatim>
  </tm-fragment>

  A <markup|with> tag with only string attributes and values is represented
  using the standard XML attribute notation. For instance, \Psome
  <with|color|blue|blue> text\Q would be represented as

  <\quote-env>
    <framed-fragment|<\verbatim>
      some \<less\>with color="blue"\<gtr\>blue\<less\>/with\<gtr\> text
    </verbatim>>
  </quote-env>

  Conversely, <TeXmacs> provides the <markup|attr> primitive in order to
  represent attributes of XML tags. For instance, the XML fragment

  <\quote-env>
    <framed-fragment|<\verbatim>
      some \<less\>mytag beast="heary"\<gtr\>special\<less\>/mytag\<gtr\>
      text
    </verbatim>>
  </quote-env>

  would be imported as \P<inactive*|some <my-tag|<attr|beast|heary>|special>
  text>\Q. This will make it possible, in principle, to use <TeXmacs> as an
  editor of general XML files.

  Finally, the binary content of a <markup|raw-data> tag is encoded in
  base64 inside a <verbatim|tm-arg> child of a <verbatim|raw-data> element.

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