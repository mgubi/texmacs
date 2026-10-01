<TeXmacs|2.1.4>

<style|<tuple|tmdoc|english>>

<\body>
  <tmdoc-title|Manipulating <TeXmacs> windows>

  <\explain>
    <scm|(window-list)><explain-synopsis|list of all <TeXmacs> windows>
  <|explain>
    Return the list of all <TeXmacs> windows. The number of windows is
    returned by <scm|(windows-number)>.
  </explain>

  <\explain>
    <scm|(current-window)><explain-synopsis|current window>
  <|explain>
    Return the window in which the current view is displayed. If the current
    view is not displayed in a window, then an empty <abbr|URL> is returned.
  </explain>

  <\explain>
    <scm|(window-\<gtr\>buffer <scm-arg|win>)><explain-synopsis|buffer
    displayed in window>
  <|explain>
    This routine returns the buffer which is currently being displayed in the
    window <scm-arg|win>, or <scm|#f>. The glued variant
    <scm|window-to-buffer> returns <scm|(url-none)> instead of <scm|#f>.
    Warning: in the future, when a window will be
    allowed to contain multiple buffers, this routine might be replaced by
    <scm|window-\<gtr\>buffers>.
  </explain>

  <\explain>
    <scm|(window-\<gtr\>view <scm-arg|win>)><explain-synopsis|view displayed
    in window>
  <|explain>
    This routine returns the view which is currently being displayed in the
    window <scm-arg|win>, or <scm|(url-none)>. Warning: in the future, when a
    window will be allowed to contain multiple views, this routine might be replaced by
    <scm|window-\<gtr\>views>.
  </explain>

  <\explain>
    <scm|(window-set-buffer <scm-arg|win>
    <scm-arg|buf>)><explain-synopsis|show buffer in window>
  <|explain>
    Display the buffer <scm-arg|buf> in the window <scm-arg|win>, using a
    view on <scm-arg|buf> which is not yet displayed in another window (a new
    view is created if necessary).
  </explain>

  <\explain>
    <scm|(window-set-view <scm-arg|win> <scm-arg|vw>
    <scm-arg|focus?>)><explain-synopsis|show view in window>
  <|explain>
    Display the view <scm-arg|vw> in the window <scm-arg|win>. If
    <scm-arg|focus?> is <scm|#t>, or if the view previously displayed in
    <scm-arg|win> was the current view, then <scm-arg|vw> becomes the
    current view. The program aborts if the view was already attached to
    another window.
  </explain>

  <\explain>
    <scm|(window-focus <scm-arg|win>)><explain-synopsis|focus window>
  <|explain>
    Set the current focus to the window <scm-arg|win>. The current
    implementation is still a bit bugged and only correct if you want to
    execute a sequence of commands under the assumption that <scm-arg|win>
    carries the focus and if you return the focus to the original window at
    the end.
  </explain>

  <\explain>
    <scm|(switch-to-window <scm-arg|win>)><explain-synopsis|switch to another
    window>
  <|explain>
    Map the window <scm-arg|win> and give it the keyboard focus.
  </explain>

  <\explain>
    <scm|(open-window)><explain-synopsis|create new window>
  <|explain>
    Create a new window with an empty buffer and return the <abbr|URL> of the
    window. The variant <scm|(open-window-geometry <scm-arg|geom>)> in
    addition specifies the geometry of the new window, and
    <scm|(clone-window)> opens a new window on the current buffer.
  </explain>

  <\explain>
    <scm|(open-buffer-in-window <scm-arg|buf> <scm-arg|cnt>
    <scm-arg|attrs>)><explain-synopsis|open buffer in new window>
  <|explain>
    Create a new window and set its main buffer to that identified by the
    <abbr|URL> <scm-arg|buf>. If <scm-arg|buf> is not yet a valid buffer, it
    is created and its contents set to <scm-arg|cnt>, otherwise the second
    parameter is ignored. The window is created with its attributes set to
    <scm-arg|attrs> (currently only the geometry is taken into account, but
    this might be extended in the future, see the <c++> function <cpp|url
    new_window (bool map_flag= true, tree geom= "")> in
    <verbatim|Texmacs/Data/new_window.cpp>). The <abbr|URL> of the new window
    is returned.
  </explain>

  <\explain>
    <scm|(kill-window <scm-arg|win>)><explain-synopsis|close a window>
  <|explain>
    Close the window <scm-arg|win>, without closing the buffer which it
    displays. If this was the last window, then <TeXmacs> quits (unless it
    is running as a server).
  </explain>

  See <hlink|the server, buffers, views and
  windows|../../source/server.en.tm> for the corresponding <c++> data
  structures.

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