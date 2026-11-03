<TeXmacs|2.1.4>

<style|<tuple|tmdoc|english>>

<\body>
  <tmdoc-title|Manipulating <TeXmacs> views>

  <\explain>
    <scm|(view-list)><explain-synopsis|list of all views>
  <|explain>
    This routine returns the list of all available views, sorted by inverse
    chronological order. That is, views which were selected more recently
    will occur earlier in the list.
  </explain>

  <\explain>
    <scm|(current-view)><explain-synopsis|current view>
  <|explain>
    Return the current view or <scm|#f>. The underlying glued routine
    <scm|(current-view-url)> returns <scm|(url-none)> if there is no current
    view.
  </explain>

  <\explain>
    <scm|(buffer-\<gtr\>views <scm-arg|buf>)><explain-synopsis|views on a
    buffer>
  <|explain>
    Return the list of all views on the buffer <scm-arg|buf>.
  </explain>

  <\explain>
    <scm|(view-\<gtr\>buffer <scm-arg|vw>)><explain-synopsis|buffer to which
    the view is attached>
  <|explain>
    This routine returns the buffer to which the view <scm-arg|vw> is
    attached.
  </explain>

  <\explain>
    <scm|(view-\<gtr\>window <scm-arg|vw>)><explain-synopsis|window to which
    the view is attached>
  <|explain>
    This routine returns the window in which the view <scm-arg|vw> is being
    displayed or <scm|#f>. The glued variant <scm|view-\<gtr\>window-url>
    returns <scm|(url-none)> instead of <scm|#f>.
  </explain>

  <\explain>
    <scm|(view-new <scm-arg|buf>)>

    <scm|(view-passive <scm-arg|buf>)>

    <scm|(view-recent <scm-arg|buf>)><explain-synopsis|get view on buffer>
  <|explain>
    All three routines return a view on the buffer <scm-arg|buf>. In the case
    of <scm|view-new>, we systematically create a new view. The routine
    <scm|view-passive> first attempts to find an existing view on
    <scm-arg|buf> which is not attached to a window; if no such view exists,
    then a new one is created. The last routine <scm|view-recent> returns the
    most recent existing view, with a preference for the current view, or
    another visible view. Again, a new view is created if no suitable recent
    view exists. The routines <scm|view-new> and <scm|view-passive> create
    the buffer <scm-arg|buf> if it does not exist yet (<scm|view-passive>
    attempts to load it from disk).
  </explain>

  <\explain>
    <scm|(view-delete <scm-arg|vw>)><explain-synopsis|delete a view>
  <|explain>
    Delete the view <scm-arg|vw>. The view should not be displayed in a
    window.
  </explain>

  The <c++> counterparts of these routines can be found in
  <source-link|Texmacs/Data/new_view.cpp|src/Texmacs/Data/new_view.cpp>; see <hlink|views and the current
  view|../../source/server-views.en.tm> for more details.

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