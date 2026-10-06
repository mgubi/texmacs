<TeXmacs|2.1.4>

<style|<tuple|tmdoc|english>>

<\body>
  <tmdoc-title|Indexation>

  The purpose of the first extension of the basic database API is to permit
  searching for certain keywords in the database entries. The current
  implementation achieves this by maintaining a<nbsp>few additional tables
  with the list of entries in which given keywords occur. We also maintain
  a<nbsp>few additional prefix tables which allow us to search for
  uncompleted keywords (prefixes of up to six characters). For efficiency
  reasons, the indexation is done at a low level in <c++> (see
  <source-link|src/src/Plugins/Database/db_index.cpp|src/Plugins/Database/db_index.cpp> and <hlink|the <TeXmacs>
  database|../../source/database.en.tm>). All field values are indexed
  automatically, except for the values of the <scm|contributor> attribute;
  values of <scm|name> fields are in addition indexed as a whole for the
  purpose of name completion.

  It should be noticed that the indexation mechanism only indexes
  alphanumerical keywords and normalizes all keywords to lowercase. Accented
  characters and characters in other (<abbr|e.g.><nbsp>cyrillic) scripts are
  also \Ptransliterated\Q into basic unaccented roman characters. For
  instance, we write \P\<#E9\>\Q and \P\<#449\>\Q as \Pe\Q and \Pshch\Q. As a
  consequence, a search for \Ppoincare\Q will match \PPoincar\<#E9\>\Q.

  Former versions of the <scheme> layer provided a macro
  <scm|with-indexing> for selecting an indexation method; this macro no
  longer exists, since indexation is now always performed by the <c++>
  database engine.

  <paragraph|Affected routines of the database API>

  <\explain>
    <scm|(db-search q)><explain-synopsis|search for a list of fields>
  <|explain>
    Two types of supplementary constraints are supported: <scm|(:match
    keywords)> and <scm|(:prefix keywords)> (with the synonyms
    <scm|:contains> and <scm|:completes>), where <scm|keywords> is a string.
    The string is split into keywords (normalized as explained above). The
    first constraint only returns entries for which each of the keywords
    occurs in one of the indexed fields. In the second case, the last
    keyword may also occur as a prefix of a keyword in one of the indexed
    fields. The helper <scm|(prefix-\<gtr\>queries s)> returns the query
    <scm|((:completes s))>.
  </explain>

  <paragraph|Other useful routines>

  <\explain>
    <scm|(index-get-completions prefix)><explain-synopsis|get possible
    completions of a prefix>
  <|explain>
    Get the list of all possible completions of a <scm|prefix> into a keyword
    which has been indexed in the current database.
  </explain>

  <\explain>
    <scm|(index-get-name-completions prefix)><explain-synopsis|name field
    completions of a prefix>
  <|explain>
    Get the list of all values of <scm|name> fields in the current database
    which admit <scm|prefix> as a prefix. Contrary to the other indexation
    routines, the completions are not
    necessarily alphanumerical keywords, and case matters. This routine is
    for instance useful for the tab-completion of names of bibliographic
    entries.
  </explain>

  <tmdoc-copyright|2015|Joris van der Hoeven>

  <tmdoc-license|Permission is granted to copy, distribute and/or modify this
  document under the terms of the GNU Free Documentation License, Version 1.1
  or any later version published by the Free Software Foundation; with no
  Invariant Sections, with no Front-Cover Texts, and with no Back-Cover
  Texts. A copy of the license is included in the section entitled "GNU Free
  Documentation License".>
</body>

<initial|<\collection>
</collection>>