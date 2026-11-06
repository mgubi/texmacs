<TeXmacs|2.1>

<style|<tuple|tmdoc|english>>

<\body>
  <tmdoc-title|Working together with the agent>

  The most useful way of working is to have the document open in your
  <TeXmacs> while the agent works on it. This needs the files to be
  <em|shared>, and the agent is then a participant of the document, as a
  coauthor would be.

  <paragraph*|Shared files>

  When the preference <menu|Edit|Preferences|Other|Share files open in
  several TeXmacs windows> is on (the default), a file which is open in one <TeXmacs> and
  which another <TeXmacs> of the same user opens is not read again from the
  disk: the second joins the first, and both show the same document, live.
  The agent's server is such a program. Without sharing, the agent opens
  the file from the disk, alone, and what it does reaches you only through
  the file.

  Sharing is on by default, and can be turned off in the same place; the
  preference applies to the files loaded afterwards, and not to a batch
  conversion (<verbatim|texmacs -c>), which never shares. If the agent does
  not see your open document (the answer to <verbatim|list_documents> lists
  the file as open elsewhere, and the agent cannot work on it live), check
  that the preference is on in the home folder that both programs use. The
  preference is not available on <name|Windows> and in the browser.

  <paragraph*|The three modes>

  For each file that is open in your <TeXmacs>, you decide what becomes of
  the edits of the agents, with <menu|Tools|Agents>. The menu appears when
  an agent has joined the file.

  <\description>
    <item*|<menu|Propose edits (I accept or reject them)>>The default when
    you have the file open. The edits do not change your text: they appear
    as <em|proposals>.

    <item*|<menu|Let them edit directly>>The edits are applied at once. This
    is also what happens when nobody has the file open.

    <item*|<menu|Read only (refuse their edits)>>The agent can read, and
    every edit is refused.
  </description>

  The agent is told the mode in the answer of its tools.

  <paragraph*|Proposals>

  A proposal shows both versions in the document, with the same markup as
  <menu|Version|Compare>: the old text and the new one side by side, in
  colours. In <menu|Tools|Agents> there is a submenu for each pending
  proposal, with <menu|Accept> and <menu|Reject>, and the entries
  <menu|Accept all> and <menu|Reject all>. The agent is told, once, in its
  next answer, whether a proposal of its own was accepted or rejected, and
  it can take a pending one back (<verbatim|withdraw_proposal>).

  A file which is saved while proposals are pending holds both versions as
  markup; so accept or reject them before saving.

  <paragraph*|What the window shows>

  While a file is shared with something, the footer of its window says so:
  <em|Shared: hosting>, with the number of windows, or <em|Shared: joined>
  in a window that joined another. A file that only one window has open
  says nothing. When an agent works on the file the footer names it, its
  mode and the proposals waiting (<em|Shared with agent 1234 (propose), 1
  proposal>). A window which could not join the other <TeXmacs> that holds
  its file says <em|Not shared>, and, when that <TeXmacs> has closed the
  file since, that the document must be closed and opened again.

  In the window that hosts the file the focus bar also has a group for the
  agent, when there is one: its name (with a remark when it is busy or does
  not answer for some seconds, or has left), the mode, the number of
  proposals with buttons to go to the previous and the next one, and a
  <em|...> menu with <em|Accept the proposal here>, <em|Reject the proposal
  here>, <em|Accept all>, <em|Reject all> and <em|Check connection>, which asks the agent's program
  and says in the footer whether it answers.

  When a window joins a file, or an agent joins yours, a message in the
  footer says so (<em|an agent joined this document>), and another one says
  when a program which is not marked as an agent joined, or when the file
  could not be joined (<em|this file is open in another <TeXmacs>, but
  ...; it is opened from the file, without sharing, and not saved>).

  <paragraph*|Who is an agent>

  A program which joins your file without a window is taken for an agent,
  whatever name it gives itself. The entry <menu|Treat ... as a person> of
  <menu|Tools|Agents> appears only for a program which does not mark itself
  as an agent; <verbatim|texmacs --mcp> does, so it never gets it.

  <paragraph*|Saving>

  When you have the file open, you save it: in the propose and read only
  modes the agent's <verbatim|save_document> is refused.

  If a file is held by a <TeXmacs> which the agent (or another window) could
  not join, it is opened without sharing and <em|read only>: the agent
  neither edits nor saves it, and a Save in such a window is refused, with a
  message, so that it never overwrites the file of the window that holds it
  (Save as writes a copy). The same guard applies to export and to Save as
  over that file, and it does not depend on the preference. It stays so
  until the agent closes the document and opens it again.

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
