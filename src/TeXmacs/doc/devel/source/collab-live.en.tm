<TeXmacs|2.1.4>

<style|<tuple|tmdoc|english>>

<\body>
  <tmdoc-title|Live documents and shared editing>

  <section|Introduction>

  A <em|live document> is a document fragment which can be displayed and
  edited simultaneously in several places: in several buffers of the same
  <TeXmacs> process, or by several users connected to the same server. Live
  editing is built on three ingredients:

  <\enumerate>
    <item>The <c++> algebra of <em|modifications> and <em|patches>, which
    also underlies undo and redo.

    <item>A purely local layer (<source-link|utils/relate/live-document.scm|TeXmacs/progs/utils/relate/live-document.scm>
    and <source-link|live-view.scm|TeXmacs/progs/utils/relate/live-view.scm>) which maintains, for each live document,
    its current value, a history of states, and the <em|views> which
    display it.

    <item>A network layer (<source-link|utils/relate/live-connection.scm|TeXmacs/progs/utils/relate/live-connection.scm>,
    <source-link|client/client-live.scm|TeXmacs/progs/client/client-live.scm> and <source-link|server/server-live.scm|TeXmacs/progs/server/server-live.scm>)
    in which the server holds the reference copy of the document and
    orders the modifications of the clients.
  </enumerate>

  The implementation is experimental: it works for simple cases, but the
  code still contains <verbatim|FIXME> notes and debugging output, and
  conflicting modifications are resolved by discarding local changes (see
  below). For the user interface, see <hlink|Remote tools and collaborative
  editing|../../main/remote/man-collaborative.en.tm>.

  <section|Modifications and patches>

  A <em|modification> (<source-link|Kernel/Types/modification.hpp|src/Kernel/Types/modification.hpp>) is an
  elementary change of a tree at a given path. Its kind is one of
  <cpp|MOD_ASSIGN>, <cpp|MOD_INSERT>, <cpp|MOD_REMOVE>, <cpp|MOD_SPLIT>,
  <cpp|MOD_JOIN>, <cpp|MOD_ASSIGN_NODE>, <cpp|MOD_INSERT_NODE>,
  <cpp|MOD_REMOVE_NODE> and <cpp|MOD_SET_CURSOR>; in <scheme> it is
  printed as a list <scm|(<scm-arg|kind> <scm-arg|path> <scm-arg|tree>)>
  by <scm|modification-\<gtr\>scheme> and read back by
  <scm|scheme-\<gtr\>modification> (<source-link|kernel/library/patch.scm|TeXmacs/progs/kernel/library/patch.scm>), with
  kinds <scm|assign>, <scm|insert>, <scm|remove>, <scm|split>, <scm|join>,
  <scm|assign-node>, <scm|insert-node>, <scm|remove-node> and
  <scm|set-cursor>. This is the form in which changes travel over the
  network.

  A <em|patch> (<source-link|Data/History/patch.hpp|src/Data/History/patch.hpp>) is either a pair of a
  modification and its inverse (<cpp|PATCH_MODIFICATION>), a sequence of
  patches (<cpp|PATCH_COMPOUND>), a set of alternatives
  (<cpp|PATCH_BRANCH>, used for redo trees), a birth or death marker
  (<cpp|PATCH_BIRTH>) or a patch labeled with an author
  (<cpp|PATCH_AUTHOR>). The essential operations, available in <scheme>
  under the names given in parentheses, are:

  <\itemize>
    <item>inversion with respect to the tree before the patch
    (<scm|patch-invert>), application (<scm|patch-apply>,
    <scm|patch-applicable?>);

    <item>commutation (<source-link|Data/History/commute.cpp|src/Data/History/commute.cpp>):
    <cpp|swap (p1, p2)> tries to rewrite <math|p<rsub|1>;p<rsub|2>> as
    <math|p<rsub|2><rprime|*>;p<rsub|1><rprime|*>> with the same effect, and
    <cpp|can_pull>, <cpp|pull> and <cpp|co_pull> (<scm|patch-can-pull?>,
    <scm|patch-pull>, <scm|patch-co-pull>) return the two transformed
    patches. This is the operational transformation used both by the undo
    system (to undo one's own changes past the changes of other authors) and
    by live editing (to rebase local changes on top of remote ones).
  </itemize>

  The conversion between patches and the network form is done by
  <scm|patch-\<gtr\>modlist> and <scm|modlist-\<gtr\>patch>
  (<source-link|utils/relate/live-connection.scm|TeXmacs/progs/utils/relate/live-connection.scm>); the latter needs the tree
  to which the modifications apply in order to compute the inverses.

  <section|Local live documents>

  <subsection|States and history>

  <source-link|utils/relate/live-document.scm|TeXmacs/progs/utils/relate/live-document.scm> keeps three tables indexed by a
  <em|live identifier> <scm|lid> (for remote documents, the <abbr|URL>
  <verbatim|tmfs://live/<em|server>/<em|name>>):

  <\description>
    <item*|<scm|live-documents>>The current tree.

    <item*|<scm|live-states>>The list of state identifiers, most recent
    first. States are unique identifiers (<scm|create-unique-id>).

    <item*|<scm|live-changes>>For each state except the oldest one, the
    inverse patch leading back to the previous state.
  </description>

  <scm|(live-apply-patch <scm-arg|lid> <scm-arg|p>
  [<scm-arg|state>])> applies a patch, pushes a new state (a fresh one, or
  the given one) and returns it. From the history one can recover the
  document at any recorded state (<scm|live-get-document>), the list of
  states and patches since a given state (<scm|live-get-state-list>,
  <scm|live-get-patch-list>), and the patch from a past state to the
  current one (<scm|live-get-inverse-patch>). <scm|live-retract> undoes the
  most recent states until a given one is current, and
  <scm|live-rewrite-history> replaces the part of the history above a
  given state by an equivalent sequence of states and changes; it raises an
  error if the new history is not equivalent to the old one
  (<scm|patch-strong-equivalent?>). To bound memory, <scm|live-forget-obsolete>
  drops all states older than the oldest state still in use; the states in
  use are collected by the overloaded function <scm|live-states-in-use>
  (states of views and of remote peers).

  <subsection|Views>

  A view is a subtree of an ordinary buffer marked up with
  <markup|live-io> (<source-link|packages/utilities/live.ts|TeXmacs/packages/utilities/live.ts>) or
  <markup|live-io*> (<source-link|packages/miscellaneous/live-document.ts|TeXmacs/packages/miscellaneous/live-document.ts>):

  <\verbatim-code>
    \<less\>live-io\|view-id\|live-id\|body\<gtr\>
  </verbatim-code>

  The macro wraps the body in a locus whose identifier is the live
  identifier, attaches an <markup|observer> which calls the <scheme>
  function <scm|live-notify> on every modification of the view, and
  contains a hidden <markup|extern> call to <scm|live-initialize>, which
  is evaluated when the view is first typeset. <source-link|live-view.scm|TeXmacs/progs/utils/relate/live-view.scm>
  keeps, for each view identifier, the state of the live document which it
  displays.

  <\itemize>
    <item><em|Initialization.> <scm|live-initialize> either restores the
    view from an existing live document or calls <scm|live-retrieve>, which
    by default creates the live document from the contents of the view and
    which is overloaded for remote documents (see below). If the same view
    identifier occurs twice (e.g. after copy and paste),
    <scm|live-view-separate> gives fresh identifiers to the copies.

    <item><em|Local edits.> <scm|live-notify> ignores cursor movements and
    changes made while <scm|live-updating?> is set, turns each
    modification into a patch and queues it in <scm|live-pending>. The
    queue is flushed shortly afterwards (<scm|delayed> with
    <scm|:idle 1>) by <scm|live-treat-pending>, which applies the
    accumulated patch of the edited view to the live document. If two views
    of the same document were edited in the same interval, only the first
    one is taken into account and the others are reset.

    <item><em|Propagation to views.> <scm|live-update-views> brings every
    view to the current state, by applying the patch from the view's state
    to the current state when possible, and by replacing the whole view
    otherwise. These changes are made with <scm|live-updating?> set, and
    under a dedicated author <scm|live-author>, obtained with
    <scm|new-author>.
  </itemize>

  <section|The network protocol>

  <subsection|Opening a remote live document>

  The load handler of <verbatim|tmfs://live/<em|server>/<em|name>>
  (<source-link|client/client-live.scm|TeXmacs/progs/client/client-live.scm>) returns a document in the style
  <verbatim|live-document> whose body is a single
  <markup|live-io*> view with an empty body. When the view is
  initialized, the overloaded <scm|live-retrieve> sends <scm|(live-open
  <scm-arg|lid>)> to the server. The service <scm|live-open>
  (<source-link|server/server-live.scm|TeXmacs/progs/server/server-live.scm>) creates the document if necessary (a
  database entry of type <verbatim|"live"> and a file in the repository),
  loads it into memory, checks read access, registers the connection with
  <scm|live-connect> and answers <scm|(<scm-arg|state>
  <scm-arg|doc>)>. The client then creates its local copy with the
  <em|same> state identifier (<scm|(live-create lid doc state)>), records
  that the server is at that state (<scm|live-connect>) and restores its
  views.

  Both sides thus share the state identifiers. For every connection, the
  server remembers the state that the client is known to have
  (<scm|live-get-remote-state>); the client remembers the state that the
  server is known to have.

  <subsection|Client to server>

  When a local edit is applied to a remote live document, the overloaded
  <scm|live-apply-patch> of <source-link|client-live.scm|TeXmacs/progs/client/client-live.scm> sends

  <\scm-code>
    (live-modify lid mods old-state new-state)
  </scm-code>

  where <scm|mods> is the list of modifications and the two states are
  the local states before and after the edit. The service
  <scm|live-modify> accepts the change if and only if the user may write
  the document, the client's last known state and the server's current
  state both equal <scm|old-state>, and the patch is applicable
  (<scm|live-applicable?>). In that case it applies the patch, adopting
  <scm|new-state> as its own new state, and answers <scm|#t>; otherwise
  it answers <scm|#f> and does nothing. In other words, the server never
  transforms patches: it only accepts patches which are based on its
  current state, and leaves the rebasing to the clients.

  <subsection|Server to clients>

  After each accepted change, <scm|live-broadcast> considers every
  connected client whose known state differs from the current state, and
  sends it (<scm|server-remote-eval>) the patch from its known state to
  the current state, again in the form <scm|(live-modify <scm-arg|lid>
  <scm-arg|mods> <scm-arg|old-state> <scm-arg|new-state>)>. At most one
  such request per client and document is outstanding at any time (table
  <scm|live-waiting>); when the client confirms, the server updates the
  client's known state and checks again whether more changes need to be
  sent.

  <subsection|Rebasing on the client>

  The call-back <scm|live-modify> of <source-link|client-live.scm|TeXmacs/progs/client/client-live.scm> receives a
  patch <math|p> based on <scm|old-state>, a state which the client
  necessarily has in its history (it is the last state which the server
  acknowledged or sent). The local history may contain changes made since
  then which the server has not yet accepted. The call-back:

  <\enumerate>
    <item>flushes pending local edits (<scm|live-treat-pending>);

    <item>computes, with <scm|live-latest-compatible>, the most recent
    local state such that all local changes between <scm|old-state> and
    that state commute with <math|p> (tested with <scm|patch-can-pull?> and
    by checking that both orders give the same tree);

    <item>retracts all local changes after that state
    (<scm|live-retract>, which also updates the views): <em|these changes
    are lost>;

    <item>pulls <math|p> over the remaining local changes, giving a patch
    <math|p<rprime|*>> which applies to the current local document, and
    transforms the local changes accordingly (<scm|patch-pull>,
    <scm|patch-co-pull>);

    <item>applies <math|p<rprime|*>> without sending it back (the flag
    <scm|following-server-instruction?> is set), and rewrites the history
    so that it reads: <scm|old-state>, the server change leading to
    <scm|new-state>, then the transformed local changes with fresh state
    identifiers;

    <item>records that the server is at <scm|new-state>, answers
    <scm|#t> and resends the remaining local changes, now based on
    <scm|new-state> (<scm|live-resend-local-changes>).
  </enumerate>

  When the server refuses a client change (because another change was
  accepted first), the client does nothing: the server will send the
  concurrent change, the client will rebase its own change on top of it as
  above, and resend it. Since the server processes one message at a time,
  this converges as long as the changes commute. When they do not, the
  local changes which do not commute with the remote ones are discarded;
  the functions <scm|live-find-latest-compatible> and
  <scm|live-rewrite-history> print diagnostics in that case.

  <subsection|Persistence and disconnection>

  The server keeps live documents in memory. The document is written to the
  repository (as a <TeXmacs> snippet, <scm|live-save>) when it is first
  created and whenever a client disconnects: the overloaded
  <scm|server-remove> saves every live document the client was connected
  to and removes the client from the peers (<scm|live-hang-up>). There is no
  periodic saving, so modifications made while clients stay connected are
  lost if the server process dies. Live documents are created readable and
  writable by <verbatim|"all">. The service <scm|remote-list-live> lists
  the live documents owned by the user, and <scm|live-exists?> tests the
  existence of a document.

  <section|Interaction with undo and redo>

  Each view of a buffer has an <cpp|archiver> (<source-link|Data/History/archiver.hpp|src/Data/History/archiver.hpp>)
  which records modifications as patches labeled with their author. Changes
  coming from other participants are applied to the views under the author
  <scm|live-author>, so they are recorded as changes of another author.
  When the user undoes, <cpp|archiver_rep::undo> first tries to move the
  user's own most recent change to the top of the history by commuting it
  with more recent changes of other authors (<cpp|archiver_rep::expose>,
  which relies on <cpp|swap> for patches); if this succeeds, only the own
  change is undone. Otherwise the changes of other authors on top of it are
  undone as well. The resulting modifications of the view are ordinary edits
  (<scm|live-updating?> is not set), so <scm|live-notify> propagates the
  undo to the live document and hence to the server like any other local
  change. There is no global, collaborative undo. The history itself is
  described in <hlink|undo, redo and the modification history|undo.en.tm>.

  <section|Extending live documents>

  The local layer is independent of the network: the functions
  <scm|live-retrieve>, <scm|live-apply-patch> and
  <scm|live-states-in-use> are designed to be overloaded (with
  <scm|:require> or through <scm|former>). A new transport, for instance
  live documents shared with a plug-in, would provide its own
  <scm|live-retrieve> for its identifiers, overload
  <scm|live-apply-patch> to forward local changes, and apply incoming
  changes with the same rebasing logic as the <scm|live-modify> call-back.

  <tmdoc-copyright|2026|the <TeXmacs> team>

  <tmdoc-license|Permission is granted to copy, distribute and/or modify this
  document under the terms of the GNU Free Documentation License, Version 1.1
  or any later version published by the Free Software Foundation; with no
  Invariant Sections, with no Front-Cover Texts, and with no Back-Cover
  Texts. A copy of the license is included in the section entitled "GNU Free
  Documentation License".>
</body>

<initial|<\collection>
</collection>>
