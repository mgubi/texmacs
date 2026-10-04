<TeXmacs|2.1.4>

<style|<tuple|tmdoc|english>>

<\body>
  <tmdoc-title|Manipulating <TeXmacs> buffers>

  Buffers are identified by their <abbr|URL>s. Most of the routines below
  are glued directly to <c++> functions in <verbatim|Texmacs/Data/new_buffer.cpp>
  (see <verbatim|Scheme/Glue/build-glue-basic.scm>); a few convenience
  wrappers are defined in <verbatim|kernel/library/base.scm> and
  <verbatim|texmacs/texmacs/tm-files.scm>. The underlying <c++> data
  structures are described in <hlink|buffers|../../source/server-buffers.en.tm>.

  <paragraph|Basic buffer management>

  <\explain>
    <scm|(buffer-list)><explain-synopsis|list of all buffers>
  <|explain>
    This routine returns the list of all open buffers, the most recently
    created ones first.
  </explain>

  <\explain>
    <scm|(buffer-exists? <scm-arg|buf>)><explain-synopsis|check whether a
    buffer is open>
  <|explain>
    Check whether <scm-arg|buf> (a <abbr|URL> or a string) is among the open
    buffers.
  </explain>

  <\explain>
    <scm|(current-buffer)><explain-synopsis|current buffer>
  <|explain>
    Return the buffer of the current view, or <scm|#f> if there is no
    current view. The underlying glued routine <scm|(current-buffer-url)>
    returns <scm|(url-none)> in the latter case.
  </explain>

  <\explain>
    <scm|(path-\<gtr\>buffer <scm-arg|p>)><explain-synopsis|buffer which
    contains a certain path>
  <|explain>
    Return the buffer which contains a certain path <scm-arg|p>, or <scm|#f>.
    The glued variant <scm|path-to-buffer> returns <scm|(url-none)> instead
    of <scm|#f>.
  </explain>

  <\explain>
    <scm|(tree-\<gtr\>buffer <scm-arg|t>)><explain-synopsis|buffer which
    contains a certain tree>
  <|explain>
    Return the buffer which contains a certain tree <scm-arg|t>, or <scm|#f>.
  </explain>

  <\explain>
    <scm|(buffer-\<gtr\>tree <scm-arg|buf>)>

    <scm|(buffer-\<gtr\>path <scm-arg|buf>)><explain-synopsis|body of a
    buffer as a tree or path>
  <|explain>
    Return the body of the buffer <scm-arg|buf> as an active tree
    (<abbr|resp.> its path in the global edit tree), or <scm|#f> if the buffer
    does not exist.
  </explain>

  <\explain>
    <scm|(buffer-\<gtr\>views <scm-arg|buf>)><explain-synopsis|list of views
    on a buffer>
  <|explain>
    This routine returns the list of views on the buffer <scm-arg|buf>.
  </explain>

  <\explain>
    <scm|(buffer-\<gtr\>windows <scm-arg|buf>)><explain-synopsis|list of
    windows containing buffer>
  <|explain>
    This routine returns the list of windows in which the buffer
    <scm-arg|buf> is currently being displayed. The routine
    <scm|(buffer-\<gtr\>window <scm-arg|buf>)> returns the first of these
    windows, or <scm|#f>.
  </explain>

  <\explain>
    <scm|(buffer-new)><explain-synopsis|create a new buffer>
  <|explain>
    Create a new buffer with an empty document and return its URL. The URL is
    a scratch URL of the form <verbatim|no_name_<em|n>.tm>, for which
    <scm|buffer-has-name?> returns <scm|#f>. The buffer is not displayed in
    any window; use <scm|switch-to-buffer> or <scm|open-buffer-in-window> for
    this purpose (the glued routine <scm|(new-buffer)> creates a new buffer
    and switches to it in the current window).
  </explain>

  <\explain>
    <scm|(buffer-rename <scm-arg|buf> <scm-arg|new-name>)><explain-synopsis|rename
    a buffer>
  <|explain>
    Give a new name <scm-arg|new-name> to the buffer <scm-arg|buf>. Any
    other buffer which was already open under the name <scm-arg|new-name> is
    closed first. The master of the buffer is reset to <scm-arg|new-name> and
    a new title is proposed.
  </explain>

  <\explain>
    <scm|(buffer-close <scm-arg|buf>)><explain-synopsis|close a buffer>
  <|explain>
    Close the buffer <scm-arg|buf> without asking for confirmation, even if
    it was modified. Windows which displayed the buffer switch to another
    buffer. Closing the last buffer quits <TeXmacs> (unless it is running as
    a server).
  </explain>

  <\explain>
    <scm|(switch-to-buffer <scm-arg|buf>)><explain-synopsis|switch the
    editor's focus>
  <|explain>
    Display the buffer <scm-arg|buf> in the current window and make it the
    current buffer. If the buffer is not open yet, then it is loaded first.
    The variant <scm|(switch-to-buffer* <scm-arg|buf>)> rather switches to a
    window which already displays <scm-arg|buf>, if such a window exists.
  </explain>

  <\explain>
    <scm|(buffer-focus <scm-arg|buf>)><explain-synopsis|focus on a buffer>
  <|explain>
    Make the most recent view on <scm-arg|buf> (preferably one which is
    displayed in a window) the current view, without changing what is
    displayed in the windows. Returns <scm|#f> if there is no view on
    <scm-arg|buf>. This routine is mainly used through the macro
    <scm|with-buffer> below.
  </explain>

  <\explain>
    <scm|(with-buffer <scm-arg|buf> <scm-arg|body> ...)><explain-synopsis|execute
    code in the context of a buffer>
  <|explain>
    Temporarily focus on the buffer <scm-arg|buf>, evaluate
    <scm-arg|body>, restore the focus and return the value of the last
    expression. If <scm-arg|buf> is not an open buffer, then <scm-arg|body>
    is not evaluated and <scm|#f> is returned. The similar macro
    <scm|(with-window <scm-arg|win> <scm-arg|body> ...)> executes
    <scm-arg|body> in the context of the buffer displayed in the window
    <scm-arg|win>. Both macros are defined in
    <verbatim|utils/library/cursor.scm>.
  </explain>

  <paragraph|Information associated to buffers>

  <\explain>
    <scm|(buffer-set <scm-arg|buf> <scm-arg|rich-t>)>

    <scm|(buffer-get <scm-arg|buf>)><explain-synopsis|set/get the contents of
    the buffer>
  <|explain>
    Set the contents of the buffer <scm-arg|buf> to the rich tree
    <scm-arg|rich-t>, <abbr|resp.> get the rich contents of <scm-arg|buf>.
    Rich trees do not only contain the actual body of the document, but also
    some meta-data, such as its style, initial values of environment
    variables, and other auxiliary data attached to the document.
  </explain>

  <\explain>
    <scm|(buffer-set-body <scm-arg|buf> <scm-arg|t>)>

    <scm|(buffer-get-body <scm-arg|buf>)><explain-synopsis|set/get the main
    body of the buffer>
  <|explain>
    Set the main body of the buffer <scm-arg|buf> to the tree <scm-arg|t>,
    <abbr|resp.> get the main body of <scm-arg|buf>.
  </explain>

  <\explain>
    <scm|(buffer-set-master <scm-arg|buf> <scm-arg|master>)>

    <scm|(buffer-get-master <scm-arg|buf>)><explain-synopsis|set/get the
    master of the buffer>
  <|explain>
    Set the master of the buffer <scm-arg|buf> to <scm-arg|master>,
    <abbr|resp.> get the master of <scm-arg|buf>. The master of a buffer
    should again be a buffer. Usually, the master of a buffer is the buffer
    itself. Otherwise, the buffer will behave similarly as its master in some
    respects. For instance, if a buffer <verbatim|a/b.tm> admits
    <verbatim|x/y.tm> as its master, then a hyperlink to <verbatim|c.tm> will
    point to <verbatim|x/c.tm> and not to <verbatim|a/c.tm>.
  </explain>

  <\explain>
    <scm|(buffer-set-title <scm-arg|buf> <scm-arg|name>)>

    <scm|(buffer-get-title <scm-arg|buf>)><explain-synopsis|set/get the title
    of the buffer>
  <|explain>
    Set the title of the buffer <scm-arg|buf> to the string <scm-arg|name>,
    <abbr|resp.> get the title of <scm-arg|buf>. The title is for instance
    used as the title for the window.
  </explain>

  <\explain>
    <scm|(buffer-last-save <scm-arg|buf>)>

    <scm|(buffer-last-visited <scm-arg|buf>)><explain-synopsis|time when a
    buffer was visited/saved last>
  <|explain>
    Return the time when the buffer <scm-arg|buf> was saved (an integer)
    <abbr|resp.> visited (a floating point number) for the last time.
  </explain>

  <\explain>
    <scm|(buffer-modified? <scm-arg|buf>)>

    <scm|(buffer-pretend-saved <scm-arg|buf>)><explain-synopsis|check for
    modifications since last save>
  <|explain>
    The predicate <scm|buffer-modified?> check whether the buffer
    <scm-arg|buf> was modified since the last time it was saved. The routine
    <scm|buffer-pretend-saved> can be used in order to pretend that
    the<nbsp>buffer <scm-arg|buf> was saved, without actually saving it. This
    can for instance be useful if no worthwhile changes occurred in the
    buffer since the genuine last save. Conversely,
    <scm|(buffer-pretend-modified <scm-arg|buf>)> marks the buffer as
    modified. The routines <scm|buffer-modified-since-autosave?> and
    <scm|buffer-pretend-autosaved> are similar, but with respect to the last
    automatic save.
  </explain>

  <\explain>
    <scm|(buffer-has-name? <scm-arg|buf>)>

    <scm|(buffer-aux? <scm-arg|buf>)>

    <scm|(buffer-embedded? <scm-arg|buf>)><explain-synopsis|kind of buffer>
  <|explain>
    The predicate <scm|buffer-has-name?> checks whether <scm-arg|buf> has a
    genuine name (and not a scratch name as created by <scm|buffer-new>).
    The predicate <scm|buffer-aux?> checks whether the buffer is auxiliary,
    <abbr|i.e.> whether its master differs from the buffer itself. The
    predicate <scm|buffer-embedded?> checks whether the buffer is displayed
    in an auxiliary window, as an embedded <TeXmacs> widget.
  </explain>

  <paragraph|Synchronizing with the external world>

  Buffers inside <TeXmacs> usually correspond to actual files on disk or
  elsewhere. When changes occur on either side (<abbr|e.g.> when editing the
  buffer, or modifying the file on disk using an external program), the
  following routines can be used in order to synchronize the buffer inside
  <TeXmacs> with its corresponding file on disk.

  <\explain>
    <scm|(buffer-load <scm-arg|buf>)><explain-synopsis|load buffer>
  <|explain>
    Retrieve the buffer <scm-arg|buf> from disk (or elsewhere). Returns
    <scm|#t> on error and <scm|#f> otherwise. The format being used for
    loading files is chosen as a function of the extension of <scm-arg|buf>.
  </explain>

  <\explain>
    <scm|(buffer-save <scm-arg|buf>)><explain-synopsis|save buffer>
  <|explain>
    Save the buffer <scm-arg|buf> to disk (or elsewhere). Returns <scm|#t> on
    error and <scm|#f> otherwise. The format being used for saving files is
    chosen as a function of the extension of <scm-arg|buf>.
  </explain>

  <\explain>
    <scm|(buffer-import <scm-arg|buf> <scm-arg|src>
    <scm-arg|fm>)><explain-synopsis|import buffer>
  <|explain>
    Import the buffer <scm-arg|buf> from <scm-arg|src>, using the format
    <scm-arg|fm>. Returns <scm|#t> on error and <scm|#f> otherwise.
  </explain>

  <\explain>
    <scm|(buffer-export <scm-arg|buf> <scm-arg|dest>
    <scm-arg|fm>)><explain-synopsis|export buffer>
  <|explain>
    Export the buffer <scm-arg|buf> to <scm-arg|dest>, using the format
    <scm-arg|fm>. Returns <scm|#t> on error and <scm|#f> otherwise.
  </explain>

  <\explain>
    <scm|(tree-import <scm-arg|src> <scm-arg|fm>)><explain-synopsis|import a
    tree>
  <|explain>
    Import a tree from the URL <scm-arg|src>, using the format <scm-arg|fm>.
    If <scm-arg|src> is relative and cannot be resolved, it is resolved with
    respect to the directory of the current buffer. On failure, the string
    tree <scm|"error"> is returned.
  </explain>

  <\explain>
    <scm|(tree-export <scm-arg|t> <scm-arg|dest>
    <scm-arg|fm>)><explain-synopsis|export a tree>
  <|explain>
    Export a tree to the URL <scm-arg|dest>, using the format <scm-arg|fm>.
    Returns <scm|#t> on error and <scm|#f> otherwise.
  </explain>

  These are low level routines. The interactive commands
  <scm|(load-buffer <scm-arg|name>)> and <scm|(save-buffer)> from
  <verbatim|texmacs/texmacs/tm-files.scm> in addition take care of opening
  the buffer in a window, of autosave files, permissions, confirmation
  dialogues, <abbr|etc.>

  <tmdoc-copyright|2012|Joris van der Hoeven>

  <tmdoc-license|Permission is granted to copy, distribute and/or modify this
  document under the terms of the GNU Free Documentation License, Version 1.1
  or any later version published by the Free Software Foundation; with no
  Invariant Sections, with no Front-Cover Texts, and with no Back-Cover
  Texts. A copy of the license is included in the section entitled "GNU Free
  Documentation License".>
</body>

<initial|<\collection>
</collection>>