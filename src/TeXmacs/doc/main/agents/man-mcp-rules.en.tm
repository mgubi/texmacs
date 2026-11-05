<TeXmacs|2.1>

<style|<tuple|tmdoc|english>>

<\body>
  <tmdoc-title|What an agent may not do, and why>

  A <TeXmacs> document is active: it holds macros that run programs, links
  which run scripts, files which are included when the document is typeset,
  and strings which are written as they are into the <LaTeX> or the
  <name|HTML> which you export. If an agent could write anything, a few words
  planted in a text which it has read (a web page, a paper, an e-mail) could
  turn into an action in your <TeXmacs> or in a file that you send to a
  coauthor. The server therefore checks what the agent writes, and refuses
  the edit entirely, changing nothing, when it breaks a rule. The answer says
  the rule.

  <paragraph*|The rules in short>

  <\description>
    <item*|The definitions are yours>The agent cannot write inside an
    assignment, a macro, a style or package declaration of the document, nor
    define anything new. It can insert text next to them.

    <item*|No dangerous macros>It cannot call a macro of the document or of
    the style which would send its text to a link, a file, a program or the
    name of another macro. The macros are analysed, not just listed by name.

    <item*|Links, files and names>A link goes to a relative address, an
    anchor, <verbatim|http>, <verbatim|https> or <verbatim|mailto>; a link
    to an absolute path or with <verbatim|..> is accepted too, because a
    link only acts when you click it. A file
    (an image, an inclusion) is a relative name inside the folder of the
    document or the folders allowed. Labels, keys and values which end up in
    an export have no backslashes, braces, quotes or angle brackets.

    <item*|No raw code>No scripts, no <verbatim|extern>, no actions, no
    <LaTeX> or other raw text for other programs. <LaTeX> given by the agent
    is scanned for the commands which read files or run programs.

    <item*|Limits>No loops, and bounds on the numbers which multiply the work
    of the typesetter (columns, sizes, resolution).

    <item*|No version markup>Only <TeXmacs> writes the markup of proposals.
  </description>

  What you already have in your document, links, scripts or raw <LaTeX>,
  stays, and does not prevent the agent from editing around it; but a change
  of it counts as new content.

  <paragraph*|What this is not>

  This is a filter on what the agent writes into your documents. It is
  <em|not a sandbox>. The server opens any file which you can read, as
  <TeXmacs> would (limit this with <verbatim|TEXMACS_MCP_ROOTS>), and the
  program of the agent can still do everything which you can do with the
  other tools which you gave it: a shell, other servers, the network. The
  local link between your <TeXmacs> and a program of the same user is not
  protected against a hostile program of that user either. Decide what the
  agent may do with the permissions of the agent itself, and read the
  proposals before you accept them.

  The rules, the reasons for each, what was tried before and what is still
  open are on the page
  <hlink|MCP-security|https://github.com/mgubi/texmacs/wiki/MCP-security>
  of the wiki of the repository.

  <tmdoc-copyright|2026|Massimiliano Gubinelli>

  <tmdoc-license|Permission is granted to copy, distribute and/or modify this
  document under the terms of the GNU Free Documentation License, Version 1.1
  or any later version published by the Free Software Foundation; with no
  Invariant Sections, with no Front-Cover Texts, and with no Back-Cover
  Texts. A copy of the license is included in the section entitled "GNU Free
  Documentation License".>
</body>

<initial|<\collection>
</collection>>
