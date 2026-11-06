<TeXmacs|2.1>

<style|<tuple|tmdoc|english>>

<\body>
  <tmdoc-title|Troubleshooting>

  <paragraph*|The agent does not start the server, or says that it has no
  tools>

  Run the command by hand (see <hlink|Installing and connecting an
  agent|man-mcp-install.en.tm>, ``Checking that it works''). On
  <name|macOS> check the quotes around the path with spaces, and that the
  path exists. Look at <verbatim|system/mcp.log> in the home folder.

  <paragraph*|The agent cannot open a file>

  The path must be absolute, and inside one of the folders of
  <verbatim|TEXMACS_MCP_ROOTS> if it is set. A path with <verbatim|..> or
  with a colon is refused. The agent sees the real path of a file, after
  symbolic links.

  <paragraph*|The agent cannot see the document that I have open>

  The files must be shared (<menu|Edit|Preferences|Other|Share files open in
  several TeXmacs windows>), in the home that both programs use; then the
  agent opens the same path. The list of documents says which files are open
  elsewhere.

  <paragraph*|The edits are refused>

  The answer of the agent says why. If the rule is ``read only'', look at
  <menu|Tools|Agents> and at <verbatim|TEXMACS_MCP_READONLY>. If it is one
  of the rules of content, see <hlink|What an agent may not do|man-mcp-rules.en.tm>:
  the definitions of the document cannot be changed by an agent, and links
  and files have to be plain. A ``conflict'' means that you changed the node
  after the agent read it: it must read it again.

  <paragraph*|No proposals appear>

  Proposals exist when you have the file open and the mode is
  <menu|Propose edits (I accept or reject them)>. In the mode ``direct'' the
  edit is applied; if nobody has the file open, it is applied to the file in
  the agent's own copy, and the agent saves. The menu <menu|Tools|Agents>
  shows up only once an agent has joined the file.

  <paragraph*|<menu|Tools|Agents> is not in the menu>

  The menu appears only while an agent is joined to the document of the
  current window, and only in the window that hosts the file: the first one
  that opened it. A second window of the file (another TeXmacs process, which
  joined the first) never shows it. To check, ask the agent for <verbatim|list_documents> and look at the
  entry of the file:

  <\itemize>
    <item><verbatim|shared> with the role <verbatim|participant> and
    <verbatim|agent_mode> <verbatim|propose> (or the mode you chose): the
    agent joined your window.

    <item>The role <verbatim|host> with one participant: the agent holds the
    file alone, and your window did not join it (the file was opened in the
    agent first, or it was loaded before sharing was on).

    <item><verbatim|shared> null, <verbatim|agent_mode>
    <verbatim|read-only> and a note saying that the file is open in your
    <TeXmacs> but not joined: the agent could not join your window (the
    process does not answer, or the content cannot be shared). The agent
    neither edits nor saves the file then, and a Save in a window of yours
    that did not join is refused too (use Save as for a copy). This stays so
    until the agent closes the document and opens it again.
  </itemize>

  <paragraph*|The agent cannot save>

  When you have the file open and the mode is not direct, you save the file.
  When the agent could not join your window (read-only, ``not joined''), its
  save is refused whatever the mode: the file is yours, and it must close
  the document and open it again.

  <paragraph*|The server keeps running, or uses a lot of the processor>

  It ends when the agent closes its end. An idle server uses about 1.5 per
  cent of a processor (a few per cent at most). Use the process list to stop
  a program which has outlived its agent.

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
