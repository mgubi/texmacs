<TeXmacs|2.1.4>

<style|<tuple|tmdoc|english>>

<\body>
  <tmdoc-title|Introduction>

  There are three main kinds of objects for buffer management in <TeXmacs>:

  <\description>
    <item*|Buffers>Every open <TeXmacs> document is stored in a unique
    editable buffer. Buffers typically admit a<nbsp>one to one correspondence
    to files on disk or elsewhere on the web. Some buffers are of a<nbsp>more
    auxiliary nature, such as automatically generated help buffers. All
    buffers admit a unique URL. In the case of auxiliary buffers, this URL is
    really a read-only \Pplaceholder\Q, so saving this kind of buffers is
    impossible (of course, it remains possible to save the buffer under a new
    name).

    <item*|Views>It is possible to have multiple views on the same buffer.
    Every view is identified by a<nbsp>unique automatically generated URL,
    which again acts as a placeholder. View URLs have the form
    <verbatim|tmfs://view/<em|n>/<em|buffer>>, where <em|n> is a number and
    <em|buffer> an encoding of the URL of the underlying buffer.

    <item*|Windows>Views (contrary to the buffers themselves) can be
    displayed in actual windows. Currently, any <TeXmacs> window contains a
    unique view and a view may only be displayed in one window at the same
    time (of course, it is possible to display different views on the same
    buffer in different windows). Windows are again represented by
    automatically generated<nbsp>URLs of the form
    <verbatim|tmfs://window/<em|n>>.
  </description>

  At any moment, there is at most one <em|current view>; the current buffer
  and the current window are the buffer of the current view and the window
  in which it is displayed. Most editing routines act on the current view.
  The <c++> implementation of these objects is described in <hlink|the
  server: buffers, views and windows|../../source/server.en.tm>.

  <\remark>
    In the future, views and windows should really be considered as documents
    themselves. Changes in the view will be automatically propagated (or not)
    to the corresponding buffer, and the other views. Windows will contain a
    document which specifies its layout (menus and toolbars). The
    corresponding view (or views) will be an active hyperlink (or active
    hyperlinks). The current APIs already reflect these future development
    intentions.
  </remark>

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