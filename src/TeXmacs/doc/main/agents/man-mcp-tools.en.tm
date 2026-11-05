<TeXmacs|2.1>

<style|<tuple|tmdoc|english>>

<\body>
  <tmdoc-title|The tools of the server>

  The agent sees a list of tools, each with a description and its
  arguments; this page tells what they are for. Every tool which works on a
  document takes <verbatim|document>: the identifier given by
  <verbatim|open_document> (<verbatim|d1>, <verbatim|d2>...) or the
  absolute path of the file, which is then opened.

  <paragraph*|Tools for reading>

  <\description>
    <item*|<verbatim|list_documents>>The documents of this session, and the
    files which another <TeXmacs> of the user has open, with the mode in
    which the agent works on them.

    <item*|<verbatim|open_document>>Opens a file by its path (the same
    document if it is already open).

    <item*|<verbatim|get_outline>>The title and the sections (with their
    level, labels and size). The first thing to ask for a long document.

    <item*|<verbatim|read>>The content of the document or of a node, as text
    (the default and the cheapest), <TeXmacs>, an s-expression, <LaTeX> or
    <name|Markdown>, by pieces if it is large.

    <item*|<verbatim|find>>Searches text, tags (<verbatim|tag:theorem>),
    labels and sections.

    <item*|<verbatim|get_references>>The labels, the references and the
    citations, and the references which point nowhere or the labels which
    are defined twice.
  </description>

  <paragraph*|Tools for editing>

  They are not offered when <verbatim|TEXMACS_MCP_READONLY> is set. Each
  call is one step which can be undone, and it is applied entirely or not at
  all. An edit says the hash of the node as the agent read it; if the node
  has changed since, the answer is a <em|conflict> with the current content.

  <\description>
    <item*|<verbatim|replace>>Replaces a node by new content.

    <item*|<verbatim|insert>>Inserts content before or after a node, or at
    the beginning or the end of the document or of a node.

    <item*|<verbatim|delete>>Deletes a node.

    <item*|<verbatim|edit_text>>Changes a word or a sentence in a node
    (<em|old> becomes <em|new>); the best tool for small corrections.

    <item*|<verbatim|undo_agent>>Undoes the last steps of the agent. It does
    not undo what you did, and it cannot undo a proposal.
  </description>

  The new content can be given as <TeXmacs>, as an s-expression, as
  <LaTeX> or as <name|Markdown>. It is parsed first, and refused if it does
  not parse or if it breaks the rules of
  <hlink|What an agent may not do|man-mcp-rules.en.tm>.

  <paragraph*|Tools for proposals>

  <\description>
    <item*|<verbatim|list_proposals>>The proposals of the agent in a
    document, with their state (pending, accepted, rejected, withdrawn).

    <item*|<verbatim|withdraw_proposal>>Takes a pending proposal back; your
    document returns to the old text.
  </description>

  <paragraph*|Tools for files>

  <\description>
    <item*|<verbatim|save_document>>Saves the document to its file. If you
    have the file open and the agent is not in the direct mode, the save is
    refused: you save.

    <item*|<verbatim|close_document>>Closes the document; refused when it
    has unsaved changes, unless it is asked to discard them.

    <item*|<verbatim|create_document>>Creates a new <verbatim|.tm> file (the
    path is absolute, the folder exists, the file does not) with a style and
    optionally a content, and opens it.
  </description>

  There is no tool which runs code, starts a session of a plug-in, changes
  your preferences or goes to the network.

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
