<TeXmacs|2.1>

<style|<tuple|tmdoc|english>>

<\body>
  <tmdoc-title|Working with an AI agent: the <name|MCP> server>

  <TeXmacs> Upsilon can serve its documents to an AI agent, such as
  <name|Claude Code>, <name|Claude Desktop> or any other client of the
  <name|Model Context Protocol> (<name|MCP>). The command is

  <\verbatim-code>
    texmacs --mcp
  </verbatim-code>

  It starts a <TeXmacs> without a window, which the agent talks to through
  its standard input and output. You do not type anything in a terminal: the
  agent starts the program itself, once you have told it, in its
  configuration, which command to run (see <hlink|Installing and connecting
  an agent|man-mcp-install.en.tm>).

  <paragraph*|What the agent can do>

  The agent reads and edits your documents <em|by their structure>. It asks
  for the outline of a document (the title, the sections and the labels),
  reads the sections that it needs, finds text and tags, lists the labels,
  the references and the citations (with the broken ones), and changes the
  document by replacing, inserting or deleting a node, or by changing a
  word or a sentence in a paragraph. Every node it reads has a handle and a
  hash, and an edit says which node and which version of it it is meant for.

  <paragraph*|Why not simply edit the file?>

  An agent can also edit a <verbatim|.tm> file as text. That works for small
  changes, but the file of <TeXmacs> is a serialized tree, with its own
  encoding of accents and symbols, and an editor of text does not know it.
  Through the server:

  <\itemize>
    <item>the agent reads the outline first and then only the part that
    matters, instead of the whole file, which saves time and cost on long
    documents;

    <item>an edit names a node, not a position in the text of the file; if
    you changed that node in the meantime, the edit is refused with the
    current content, and nothing is lost;

    <item>what the agent writes is parsed first and is never saved as a
    broken file: accents, symbols, formulas and tags stay valid;

    <item>if you have the file open in <TeXmacs>, the agent works on
    <em|the open document>: its edits reach your window at once, you see
    them appear, and you can accept or reject them (see
    <hlink|Working together with the agent|man-mcp-together.en.tm>);

    <item>the content the agent writes is checked by rules which protect
    your definitions, your links and your files (see
    <hlink|What an agent may not do|man-mcp-rules.en.tm>).
  </itemize>

  <paragraph*|In this chapter>

  <\traverse>
    <branch|Installing and connecting an agent|man-mcp-install.en.tm>

    <branch|The tools of the server|man-mcp-tools.en.tm>

    <branch|Working together with the agent|man-mcp-together.en.tm>

    <branch|What an agent may not do, and why|man-mcp-rules.en.tm>

    <branch|Troubleshooting|man-mcp-troubleshooting.en.tm>
  </traverse>

  The server is part of <TeXmacs> Upsilon only (see
  <hlink|<TeXmacs> Upsilon features|../../about/welcome/upsilon-texmacs.en.tm>).
  The developer notes are in the file <verbatim|docs/mcp-server.md> of the
  sources, and the security model is described on the page
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
