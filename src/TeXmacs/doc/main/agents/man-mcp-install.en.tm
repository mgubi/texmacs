<TeXmacs|2.1>

<style|<tuple|tmdoc|english>>

<\body>
  <tmdoc-title|Installing and connecting an agent>

  There is nothing to install besides <TeXmacs> Upsilon itself: the server is
  the program, started with the option <verbatim|--mcp>. What you do is to
  tell your agent which command to run. The agent then starts <TeXmacs> by
  itself, when it needs it, and stops it when it is done.

  <paragraph*|The command on each platform>

  <\description>
    <item*|<name|macOS>>The program is the executable inside the
    application. For the <name|Cocoa> application it is

    <\verbatim-code>
      /Applications/TeXmacs Upsilon.app/Contents/MacOS/TeXmacs
    </verbatim-code>

    (<verbatim|TeXmacs Upsilon Vue.app> and <verbatim|TeXmacs Upsilon
    Qt.app> for the other interfaces). The path has spaces: in a shell it
    must be quoted, in a configuration file it is a string. The option is
    added after it: <verbatim|--mcp>.

    <item*|<name|Linux>>The launcher <verbatim|texmacs> of an installation,
    for instance <verbatim|/usr/bin/texmacs> when it was installed in
    <verbatim|/usr>, or <verbatim|TeXmacs/bin/texmacs> in the folder of a
    build from the sources. If it is in the <verbatim|PATH>, then
    <verbatim|texmacs --mcp> is enough. The launcher keeps the standard
    input for <verbatim|--mcp>, which the server needs. (The
    <name|AppImage> closes the standard input of the program, so that it
    cannot be used as a server.)

    <item*|<name|Windows>>The server is not available yet: the standard
    input and output of the program are not connected on this platform.
  </description>

  <paragraph*|<name|Claude Code>>

  <\verbatim-code>
    claude mcp add texmacs -- texmacs --mcp
  </verbatim-code>

  On <name|macOS> give the whole path of the program (quote it because of
  the spaces):

  <\verbatim-code>
    claude mcp add texmacs -- "/Applications/TeXmacs Upsilon.app/Contents/MacOS/TeXmacs" --mcp
  </verbatim-code>

  Environment variables are added with <verbatim|-e>, before the name of the
  server:

  <\verbatim-code>
    claude mcp add texmacs -e TEXMACS_MCP_ROOTS=$HOME/papers -- texmacs --mcp
  </verbatim-code>

  <paragraph*|<name|Claude Desktop>>

  Open <menu|Settings|Developer|Edit Config> (the file
  <verbatim|claude_desktop_config.json>, on <name|macOS> in
  <verbatim|~/Library/Application Support/Claude/>), add the server and
  restart the application:

  <\verbatim-code>
    {

    \ \ "mcpServers": {

    \ \ \ \ "texmacs": {

    \ \ \ \ \ \ "command": "/Applications/TeXmacs Upsilon.app/Contents/MacOS/TeXmacs",

    \ \ \ \ \ \ "args": ["--mcp"],

    \ \ \ \ \ \ "env": { "TEXMACS_MCP_ROOTS": "/Users/you/papers" }

    \ \ \ \ }

    \ \ }

    }
  </verbatim-code>

  <paragraph*|Another client>

  Any client of <name|MCP> which can start a program over standard input and
  output (the transport called <em|stdio>) will do: give it the command and
  the argument <verbatim|--mcp>, and optionally the environment. The
  server speaks JSON-RPC, one message per line.

  <paragraph*|Environment variables>

  <\description>
    <item*|<verbatim|TEXMACS_MCP_ROOTS>>The folders in which the agent may
    open files, separated by colons (<verbatim|/home/me/papers:/home/me/notes>).
    Without it, the agent can open any file which you can read. Paths with
    <verbatim|..> are refused, and symbolic links are resolved before the
    check.

    <item*|<verbatim|TEXMACS_MCP_READONLY>>If it is set (and is neither empty
    nor <verbatim|0>), the server does not offer the tools which change
    anything: the edits, <verbatim|save_document>, <verbatim|close_document>
    and <verbatim|create_document>.

    <item*|<verbatim|TEXMACS_MCP_DEBUG>>Set to <verbatim|1>, it also writes
    every message to the log.

    <item*|<verbatim|TEXMACS_HOME_PATH>>The folder of the user files of
    <TeXmacs> (see below).
  </description>

  <paragraph*|The home folder and the log>

  The server uses the same home folder as the application:
  <verbatim|~/.TeXmacs_Upsilon> (<verbatim|%APPDATA%\\TeXmacs_Upsilon> on
  <name|Windows>), or the folder given by <verbatim|TEXMACS_HOME_PATH>. Your
  preferences are read from there. Nothing which <TeXmacs> prints can
  disturb the protocol: everything goes to the file
  <verbatim|system/mcp.log> of the home folder, which is renamed when it
  grows over 4 MB.

  <paragraph*|Checking that it works>

  In a terminal (here the launcher, on <name|macOS> the path of the
  program) send one message and read the answer; the server ends when its
  input ends:

  <\verbatim-code>
    echo '{"jsonrpc":"2.0","id":1,"method":"initialize","params":{"protocolVersion":"2025-06-18","capabilities":{},"clientInfo":{"name":"test","version":"1"}}}' \| texmacs --mcp
  </verbatim-code>

  The answer is a line with <verbatim|"serverInfo"> and the name
  <verbatim|texmacs>. In the agent, ask for the list of its tools (in
  <name|Claude Code>, <verbatim|/mcp>), and then to list the open documents
  or to give the outline of one of your files.

  <paragraph*|Working on a file which you have open>

  To let the agent work on the document that you have open in
  <TeXmacs>, the files must be shared between the programs; see
  <hlink|Working together with the agent|man-mcp-together.en.tm>.

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
