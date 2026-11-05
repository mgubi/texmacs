# Shared files

Several TeXmacs processes of the same user which open the same file stay in
sync, with no server. This is part 1a of step 1 of the
[MCP server design](https://github.com/mgubi/texmacs/wiki/MCP-server-design)
(sections 3 and 4.4). Not in this part: the cursors of the other
participants, `texmacs --mcp`, the menus (but see Agents below).

**Switch.** The preference `share-open-files` (`on`/`off`), default **off**:
nothing changes for a user who does not turn it on. There is no menu yet:
`(set-preference "share-open-files" "on")`. It is read when a file is loaded;
a file already shared stays so.

## What is shared

A file is shared when it is a plain local file in TeXmacs format (`.tm`) which
exists, and the per-user folder can be made. What is shared is the body of the
buffer and its **style and initial environment** (the style list, the
packages, the document style, the page setup, the language, the font and
paragraph settings...), which the editor keeps in its own copies, not in the
body tree. The references and the auxiliary data (labels, table of contents,
bibliography) are not shared: each process typesets them. The variables of
the window (size, zoom, full screen) stay those of the process.

One tree holds both for the states and the patches: `(shared META BODY)`,
with `META` = `(meta STYLE (initial C0 ... C15))`: `STYLE` is the tuple of the
style, and the `Ci` are collections of `(associate variable value)`, a
variable being in the collection given by a hash of its name (two settings
added at once would else be two inserts at the same place of one collection,
which the rebase refuses, as for two texts typed at the same place). Paths of
patches start with 0 for the meta tree and 1 for the body: one stream of states,
one ordering, one rule of rebase (`patch-can-pull?`, else refused), one
history, handover and recovery for both. In a process, the body is observed
by a tree observer; for the meta tree there is a shadow tree that mirrors the
editor (`buffer-get-meta`): at each tick the editor's style and initial
environment are compared with it, which takes in every way they change
(Document > Style, style packages, `init-style`, `init-env`,
`init-default`, page setup, language, the `buffer-set` of a whole document),
and the differences are applied to the shadow by modifications, which are
the patches. A patch of the meta tree from another process is applied to the
shadow and installed in the editor (`buffer-set-style`, `buffer-set-init`,
`buffer-reset-init`, the same calls the menus end in), so that both
typeset the same, and the next comparison finds nothing to send back (no
echo). The meta tree comes with the welcome, with the document. A change of
the style while the participants are in a handover is a state like another.
A patch which mixes both parts is split for applying (`split-patch`).

Loading goes through `load-buffer-main` as before (the question
for a large source file, wildcards, the startup buffer `startup-reserved` are
upstream of the join); the join is made in `load-buffer-check-autosave`, so
that a joining process neither reads the file nor asks about an autosave file,
and the host registers itself in `load-buffer-open`, after any load.

## Pieces

| File | What |
|---|---|
| `src/System/Link/local_link.{hpp,cpp}` | loopback links (`local-listen`, `local-accept`, `local-connect`, `local-send`, `local-receive`, ...) as `tm_link_rep` over a nonblocking socket of 127.0.0.1, with `write_packet`/`read_packet` (the `<length>\n<payload>` packets) of `tm_link`; the helpers of the entries (`shared-user-dir`, `shared-real-path`, `shared-create-file`, `shared-replace-file`, `shared-random-token`, `shared-lock-acquire`) |
| `src/Data/Observers/tree_pointer.cpp` | `tree-observe`: a tree pointer whose callback is a Scheme function (what the live documents have in the markup `live-io`), here on the body of a buffer |
| `TeXmacs/progs/utils/relate/shared-files.scm` | the protocol; uses `live-document.scm` for the states of a file (`live-create`, `live-apply-patch`, `live-get-patch-list`, `live-get-document`, `live-forget-prior`) |
| `tm-files.scm`, `tm-server.scm` | the hooks: join, host at load, save, autosave, close, save as |
| `tests/shared/` | the tests: `check.sh`, `lib.scm`, one `s-<name>.scm` per scenario |

The existing TeXmacs server (`tm_server`, `tm_sockets.cpp`, `QTMSockets.cpp`,
its backoff polling, its authentication) is not used and not changed: its
sockets bind all interfaces, and the Qt and the other builds have different
classes. `local_link.cpp` does not depend on the interface (Qt, Cocoa, Vue):
the polling is a Scheme `delayed` command every 20 ms, which all the event
loops run (about 20 to 35 ms of latency, on the main thread), so that Cocoa and
Vue need nothing more than the source in their lists (the file is in
`misc/wasm/sources.txt`; the browser has no listening sockets, and the
preference is off). The winsock branch of `local_link.cpp` was never built, and on Windows the
per-user folder is not made: the feature is off there.

## Discovery

The folder is one for the user, the same for every process whatever its
environment (`TMPDIR`, `XDG_RUNTIME_DIR` do not count): `/tmp/texmacs-<uid>/shared`,
with `/tmp` as the system resolves it (on macOS `/private/tmp`). It is made with
mode 0700 and must be a directory owned by the user (checked with `lstat`),
else there is no sharing. It does not depend on `TEXMACS_HOME_PATH`. (The
variable `TEXMACS_SHARED_DIR` replaces `/tmp`; the tests use it, to have a folder
of their own.) On Windows there is none. As cleaners of `/tmp` go by age, a host
touches its lock and its entry once an hour.

**Who hosts a file is decided by the system.** For each file the folder has
`f-<hash of the canonical path>.lock`, and a process is the host of the file
exactly while it holds an exclusive advisory lock on it (`flock` with
`LOCK_EX|LOCK_NB`; the descriptor is close-on-exec). The system frees the lock
when the process dies, however it dies (`kill -9` too), and never while it only
stalls. Becoming host is getting the lock without waiting; failing means that
another process hosts. There is no test of a pid, no probe of a port, no age
of a file, no election by timeout; every action of a host (accepting a patch,
broadcasting, saving) is done while the process holds the lock, which is an
invariant of one process.

The entry `f-<hash>.entry` only tells how to reach the host:
`(entry key port token pid interactive host-id npeers)` (8 fields): the canonical path (symlinks
and relative names resolved by `realpath`), the port of the host on 127.0.0.1,
a random token of 128 bits from `/dev/urandom`, the pid, whether the host is
interactive, and its id. It is written (whole, replaced atomically by
temporary file and rename) by the holder of the lock after it has got it, and
never trusted for authority: a leftover entry, empty, damaged or of a process
that is gone, makes no difference, since the lock is free then. It has the
number of participants (rewritten at each join and leave): when a host gives the
file up or dies, it tells how many are going to seek the lock. An entry is
removed when a host leaves with nobody; leftovers stay (one small file per
shared file). A host checks once a second that the lock file is still the file
at its path (`fstat` against `stat`), and that its entry exists: if the file was
removed it makes it again and locks it; if that works nobody else hosts and it
goes on; if it fails another process hosts (it made and locked a new one), and
this one stops at once, saves its document in the recovered folder with a
notice, and joins that host as a participant. Lock files are never removed.

## Transport

One listening socket per process (port chosen by the system) serves all the
files it hosts. The first packet of a link must be `(hello token key id pid
interactive base)` with the token of the entry of `key`: anything else, or no
packet in 5 s, closes the link. The first packet is parsed before the token is
compared (the comparison does not stop at the first different character); a
link is closed at the first packet that is not hello with the token. Messages
are Scheme lists printed with `object->string` and read with `string->object`
(read, never evaluated). Before the hello a link may send 64 KB at most, and
at most 16 such links are kept; after it, a packet may be 64 MB at most. The
header of a packet must be at most 11 decimal digits and not more than the
limit (no sign, no `!`, the prefix of the encrypted links of `tm_link`, which
these links never use); else the link is closed and its bytes dropped, so
that a peer cannot make the host use memory. Writes never block: what the
peer does not take is kept in the link; a link that is closed with output
left is kept, and written by the next polls, for 5 s at most.

## States, patches

The host keeps the states as the live documents do: the document of the last
state, the list of states (unique ids), the inverse of each change. Every
participant keeps them too, from the patches it is sent, so that it can take
over, and keeps the last 100 states.

- A participant's local changes (a tree observer on the body, `set_cursor`
  left out) become, at each tick, one patch of its list `pending`. It sends
  the first to the host with the state it is made against; one at a time.
- The host applies a patch made against its current state; made against an
  older state, it is rebased over the changes since with `patch-pull`, **only
  when `patch-can-pull?`** says so at each step, and **the result is checked**
  (it applies, and the co-pulled inverses lead back to the base with the
  patch); at most 200 changes. Else it answers `(refused id state)`. A
  patch is applied to the buffer in place with the author of the participant
  (`new-author`, so the selective undo of the host skips it).
- Then the host sends `(applied from to author id patch)` to all, the author
  included, which takes it as the confirmation.
- The **host's own changes** are applied directly: its observer collects them
  and each tick (and before it handles any message) makes them a state; they
  are never refused.
- A participant which receives a patch of the host while it has pending changes
  takes its changes back, applies the host's patch, and puts its changes back
  as the host would rebase them (`transform-pending`: pull of each pending
  patch over the inverse of the host's patch, co-pulled for the next, the
  same tie-breaking as in the host). A change which cannot be put back is
  dropped, with the later ones that depend on it (see "Changes that are not
  kept"). When it was in flight, the host's refusal is then ignored. A refusal
  of a patch which the participant had kept (the host and the participant do
  not agree) rolls back all its pending changes. Any mismatch of states
  (an `applied` from a state other than the confirmed one) asks the host for
  the whole document (`resync`, `welcome-reset`).

### Why the participant and the host agree

The host rebases a patch `P` made against the state `S0` over the inverses
`q1..qn` of the changes since: `P` is pulled over `q1` (`patch-can-pull?`,
`patch-pull`), the result over `q2`, and so on; the `patch-co-pull` of each
step gives the inverse of the change as it is after `P`, and the check that
applying everything back leads to the base with `P` makes the result
trustworthy. A participant holding `P` (pending, maybe sent) when the change
`R` of the host arrives makes the same computation with the same roles:
`rev` is the inverse of `R` (against the confirmed document), `P` is pulled
over `rev` (the same `patch-can-pull?` and `patch-pull` the host will use),
and `rev` becomes the `patch-co-pull`, the inverse of `R` after `P`, for the
next pending patch. The buffer is then taken back to the confirmed document,
`R` is applied, and the pulled patches are applied: the document is that of
the host after it rebases and applies `P`, ties included. So a patch that the
host would accept is never dropped by the participant, and one that the host
would refuse is dropped at once (the refusal that follows is ignored). (The
first version cleared the "in flight" mark of the patch while it was taken
back, so that the host's confirmation of a kept patch was taken for a change
of another, which cannot be combined with it: a false "undone".)

## Changes that are not kept

Nothing of the person's is lost silently. When changes of a participant are
refused, dropped as above, replaced by the host's document (`welcome-reset`),
or not yet sent when the buffer is closed (the close waits 1 s at the most
for the host to take them), what the person **typed** (the contents which the
lost patches insert or put in place) is saved in a document in
`$TEXMACS_HOME_PATH/system/recovered/shared-lost-<time>-<pid>-<n>.tm`, the footer
says so, and an interactive TeXmacs asks whether to open it (a question
stays until answered). The document has the text typed, the content that was
deleted (from the inverse modification of the deletion, after "Deleted:"),
the settings changed or removed ("Setting name: value"), and a description of
the changes of structure (a paragraph split or joined, a tag changed). Why a
document and not the inverse patches: an inverse can only be applied to the
document it was made for, and the host's document has moved; the text can be
pasted anywhere. The files are kept 30 days and the last 50.

## Cost of the polling

At each tick the editor is asked for a number (`buffer-meta-hash`: a sum over
its style and initial environment, which does not depend on the order of the
variables); the trees are made and compared only when it changed, and then
the comparison does not depend on the order within a collection. A tick
without a change costs a pass over the variables in C++ (microseconds for
1500 variables), not a conversion to Scheme. At most 4 new links are accepted
per tick.

## Patch ids

A patch is sent with an id `<author>/<n>`. Every process keeps, for each author,
the highest `n` it knows to have been applied (from the `applied` messages of
the host, so that a participant that becomes host has it). A host does not apply
a patch whose `n` is not above, and answers `(ack id state)`. A participant that
reconnects to a new host sends the id of the patch that was on its way to the
old one: if the new host knows it applied (the old host died after applying it
and before the author heard), it sends the whole document instead of a patch
which would put the change in twice (the pending changes of the author are then
dropped and saved, as for any reset). A reset also saves the author's whole
document in the recovered folder whenever it is not the same as the one that
arrives, with a notice, and replaces it as a whole, never as a difference.

## Saving

Only the host writes the file. A Save in a participant (`save-buffer-main`) is queued whatever the state
of its connection (the footer says so when there is none: seeking a host, or a
host that does not answer) and is done when the host has all the changes, or when
this process becomes the host; it sends its local changes to the host and, when the host has confirmed all of
them, sends `(save)`; it does nothing else; the host runs its usual `save-buffer-main`
(with its checks and dialogs; if the host has applied patches since the last
save it first calls `buffer-pretend-modified`, since the modified flag is
not reliable for changes applied by patches), and after a save
(`save-buffer-save`) it sends `(saved state)`, on which the participants call
`buffer-pretend-saved` (and `buffer-pretend-modified` again if the participant
has changes the host has not got). That sets their time of last save to the time of the
file, so that a participant which becomes host is not asked "The file has
changed on disk. Really save?" for the save of the previous host. (The same
call is made when a participant joins.) `autosave-buffer` skips the buffers of
participants. Save as, and closing, leave the sharing of the buffer.

## Hosting

Priority: interactive first (`(not (headless?))`), then the earlier to have
joined (the host's roster, broadcast at each join and leave, has a sequence
number for each).

- **Join**: a process which loads a file reads its entry, connects, says
  hello, gets `(welcome state document modified roster host)` (waiting 500 ms
  at the most, in a loop on the main thread, which is the one wait that is
  left) and makes the buffer from it (`buffer-set`, `buffer-pretend-saved`).
  If the host does not answer in time or the connection fails, the lock tells:
  free, then there is no host (the file is loaded and this process hosts it);
  held, then a host exists which is busy or stopped, and the file is opened
  from the file, not shared, with a message in the footer. **A stalled host is
  never replaced: it holds the lock.** (Any error of `local-connect` other than
  a refusal gives the same: the lock decides.)
- **A host that is gone, with participants** (the entry is there, the lock is
  free, the entry says that it had participants): they are seeking the lock, and
  have confirmed work that is not on the disk. A process that opens the file then
  does not host from the disk. It opens the file **read-only** (the footer says
  that it is connecting), and on the timer, without blocking, waits for as long
  as the participants take (300 ms for each, and one second) for one of them to
  take the lock: then it joins that host (replacing the buffer, which has not
  changed). Only if the lock is still free after that does it host from the disk.
  With no participant in the entry, it hosts at once.
- **Host at load**: after a normal load the process tries the lock; if it gets
  it, it hosts (listens, writes the entry). If not, a host appeared meanwhile:
  it joins it, replacing what it read; if the host has no entry yet (it is
  starting) it waits for it in the same way (two seconds), read-only; if the host
  does not answer, it opens unshared with the message.
- **Giving up the file** (the file is closed, or a non-interactive host has an
  interactive participant, offered at the hello of the interactive process
  and again every 5 s): the host sends all its changes, then
  `(released token preferred roster)` to all, closes its links, frees the lock
  and, unless it is leaving, goes on as a participant. There is no waiting and
  no state in between. (The entry stays, with its number of participants,
  until the new host writes its own; joiners do not connect to it: the lock is
  free or taken by the new host, and they wait as above. A participant that closes
  the file still waits up to 1 s for its changes to reach the host, see
  "Changes that are not kept".)
- **Seeking** (after `released`, or when the link of a host is dead, which is
  how its death is seen): each participant tries the lock, the preferred one
  (first by priority, interactive first) at once, the next ones 300 ms later
  each, the old host last. Whoever gets it hosts (its pending changes become
  new states, it writes a new entry with a new token); the others read the
  entry, skipping the one of the host they had (same token), and connect to
  it, sending their state: same, the patch since, or the whole document (their
  pending changes are then dropped and saved, see "Changes that are not kept").
  The delays only say who is asked first: the lock decides. While the lock is
  held and the entry is not new, the participants queue their changes and try
  again, five times a second (the lock) and once a second (a host that holds
  it but does not answer, with the message "the host is not answering; your
  changes are kept" after the second failure). A preferred process that
  stalls only delays hosting: the next one takes the lock, and offers the file
  again later if it is not interactive.
- **No time decides who hosts**, so there is no moment with two hosts: a host
  is the one that holds the lock, and a process gives up the role by freeing
  it, never by a message.
- **A buffer removed by the C++ code without `buffer-close`** (the window and
  buffer killed together): `notify-buffer-closed`, which `remove_buffer`
  calls, makes the process leave the sharing as above.

## Agents: modes, the host's filter, proposals

(Step 4, stage A, of the MCP server design.) A participant whose id begins
with `agent.` (the process of `texmacs --mcp` takes such ids, `shared-set-agent!`)
is an agent. When the **host is interactive** (the person's TeXmacs; a test sets
it with `shared-set-interactive!`), the host decides what to do with each patch of
an agent, in `host-handle-patch*` (`agent-gate`). Patches of other participants
are never touched. When no interactive TeXmacs hosts the file (the agent's own
process hosts, or only headless ones), the agent edits directly, as before.

- **Mode per file**, kept by the host (`shared-set-agent-mode!`, `shared-agent-mode`;
  not in the document, not settable by an agent, told to the agents with the message
  `(mode m)`): `propose` (the default when the host is interactive), `direct` or
  `read-only`. The menu **Tools > Agents** (shown when an agent has the file) sets
  it, with a check mark, and lists the pending proposals to accept or reject one by
  one or all.
- **The filter.** In every mode the host runs the content rules of the MCP server
  (`mcp-host-check` in `agents/mcp-server.scm`, see mcp-server.md) on the patch: the
  patch is applied to the body as a pure function (`patch-apply`, which shares the
  unchanged blocks), `shared-changed-blocks` finds the top-level blocks that differ
  (by sharing, then by hash), and the violations of the new blocks (as written and as
  they expand) that the old blocks did not have are the refusal. The same functions
  as the agent's own check are used, not copies; the definitions (`assign`, `set`,
  `reset`) of the changed blocks must be the same before and after, and code nodes
  (`extern`, `script`, `latex`, `specific`...) cannot be added or changed. A patch that
  changes the style or the initial environment, or has an odd shape, is refused. A
  failure of the check refuses the patch.
- **A refusal** is the message `(refused id state ((rule level text) ...))`: `rule`
  names the rule (definitions, link, file, names, dangerous-macro, code, limits,
  style, read-only, pending, other), `level` says what a person could be asked to
  allow (`style`, `links`) or never (`code`, `limits`, `mode`), `text` is the reason
  with the content concerned. The agent takes its change back (as for any refusal)
  and `shared-file-verdict` gives it the reasons once. (Stage B uses the levels.)
- **Propose.** The patch is not applied. The host answers `(proposed id state pid)`
  (the agent takes its change back), then edits its own document, as the agent's
  author (the person's undo skips it): the changed blocks are replaced by one block
  that holds the markup of Version > Compare. When one block replaces one block it
  is the word-by-word difference of `compare-versions`; otherwise a single
  `version-both` with the old and the new blocks (`(version-suppressed)` for an
  empty side). The agent receives that edit as a normal patch of the host. The
  person accepts or rejects with **Tools > Agents** (`shared-proposal-resolve!`,
  which replaces the markup by the new or the old version as an edit of the person,
  so the person's undo brings the markup back) or with the Version tools (the host
  notices that the markup is gone and finds out which version was kept: accepted,
  rejected, or resolved when it was edited). A patch of an agent that touches blocks
  with version markup is refused (`pending`).
- **The host's records** (`shared-proposals`): id (`p1`...), owner, status
  (pending, accepted, rejected, withdrawn, resolved), the place; resolved ones are
  kept (30) so that the agent hears. The host sends each agent the list of its
  proposals when it changes (`(proposals ((id status index) ...))`); the agent
  withdraws one with `(withdraw id)` (`shared-file-send-withdraw`).
- **Proposals in the saved file.** The markup is part of the document, so a saved
  file with pending proposals holds both versions and is valid and readable (it is
  the markup of Version > Compare, which the Version tools resolve). The author and
  the id are not in the file: they are in the host's records, so after a reload the
  markup is plain version markup. Keeping them only on screen was rejected: a
  document saved by the person's Save, by an autosave, or closed, would lose the
  agent's work silently; both versions in the file lose nothing. The cost is that a
  saved file can hold markup that the agent no longer owns.
- **Cost on the host.** The check runs once per patch of an agent, in the person's
  window: about 1 ms for a small document, 35 to 45 ms for a document of 5000
  paragraphs (one walk of the document to find its definitions), less if they are
  few; `shared-file-gate-stats` gives the count and the time.

### Hardening of the host (audit of stage A)

- **Who is an agent.** The id `agent.*` is chosen by the participant, so the host
  does not rely on it: every participant that is not interactive (headless,
  whatever its id) is treated as an agent, unless the person marks it as a person
  (`shared-treat-as-person!`, **Tools > Agents > Treat ... as a person**, kept in the
  person's process for the file). A message tells the person when such a program
  joins. **The local link is not a security boundary against a process of the same
  user that has a shell**: such a process can claim to be interactive, or write the
  file on disk. The filter protects the person from what an agent writes through the
  usual paths; it is not a sandbox.
- **Shape of patches.** Every patch of a participant is checked before any patch
  function sees it: only `pair` and `compound`, each modification of a known kind with
  a path under the part 0 (style) or 1 (body) of at least the length that kind needs
  (a modification at the root once made the host die in C++, `tm_throw` "empty path").
  A patch of another shape is refused with the rule `patch`. The same check is made by
  a participant on what the host sends (it asks for the document again if it fails),
  and in C++ `is_applicable (tree, modification)` now answers false for a path that
  is too short, whoever calls it.
- **Inverses.** The inverse that a participant sends is ignored: the host rebuilds the
  patch with the inverses it computes from the document (`rebuild-patch`), so
  rebasing, history and undo never use a forged inverse.
- **Malformed messages** (`(withdraw)`, `(patch 1)`, a patch with an id that is not a
  string...) get the answer `(refused "malformed" state reasons)` and the host goes on.
- **The Save of an agent** is refused (`(save-refused reason)`, `save_document` answers
  `refused`) unless the mode is `direct`.
- **Blocks that changed** are found by equality of trees, not of hashes (a hash can be
  forged).
- **The mode** is kept in the person's process for the file, so it survives a handover
  and coming back.
- **Version markup** written by an agent (`version-both` ...) is refused (rule
  `version-markup`, level `mode`): only the host writes it, for a proposal. The menu
  shows, for each pending proposal, the agent and a preview of its text.

- **Integers.** Every integer of a path is bounded to 0..10^6 by the shape check, and the
  C++ tests of applicability (`can_remove`, `can_join`) are written so that the sums
  cannot overflow (`join (1 2147483647)` and `remove (1 2147483646 2147483647)` used to
  kill the host). `scheme->patch` of what comes from another process is always in a
  `catch` (`safe-patch`).
- **A host that sends garbage.** A participant checks that a patch of the host is of the
  right shape and applies to its confirmed document before it calls `patch-invert` or
  anything else that aborts on a patch that does not apply; otherwise it asks for the
  document again. Every call of `patch-invert`, `patch-pull`, `patch-co-pull`,
  `patch-can-pull?`, `modification-invert` and `patch-apply` in `shared-files.scm` is
  made on a patch that `patch-applicable?` accepted on the same document (or that the
  host built itself). The C++ primitives themselves still abort on a patch that does not
  apply: the glue was not changed.
- **The person mark** (`shared-treat-as-person!`) is dropped when the participant
  leaves. While it is connected, a process that takes the id of a marked one is not
  told apart (the id is the only thing the host has). The menu names the process
  (pid and command line, `shared-file-agent-info`). There is no preference to treat the
  person's own headless TeXmacs as a person; the menu is the way, once per connection.
- **Random patches** (`tests/shared` scenario `fuzz`): a few thousand random patches
  with small and hostile integers go through `patch-applicable?`, the host's
  `shared-sanitize-patch`, `patch-invert`, `patch-apply`, `patch-pull`, `patch-co-pull`,
  `patch-can-pull?` and `patch-commute?`; the process must survive (`FZ_N`, `FZ_SEED`
  for longer runs: 5 runs of 40000 found nothing). The first version of the fuzz, which
  called `patch-can-pull?` on patches of a shape that the host refuses, found a crash in
  `swap (modification&, modification&)`: that is why the shape check comes first.
- An agent edit elsewhere in a document that holds the person's version markup (or a
  pending proposal) is accepted (and becomes a proposal in propose mode); the markup is
  not touched (`agent` scenario).

- **No set-cursor.** No patch that is sent carries a `set-cursor` (the cursor changes are
  taken out before a patch is sent), so the shape check refuses it from a peer; the
  editor (`edit_modify_rep::notify_set_cursor`) also checks that the data is a tree with
  children (it read the first child of a string and crashed).
- **Depth.** The conversions of trees are recursive and crash about 100000 levels deep
  (`stree->tree`, `tm->stree`, `patch->scheme`, `object->string`, and the reader). A message
  whose parentheses are nested more than 1500 deep is dropped before it is read
  (`nesting-ok?` in `msg-receive`, for the hello, the welcome, the patches, the catch-up;
  messages shorter than 3000 characters cannot be that deep and are not scanned; a document
  of 400 KB is scanned in a fraction of a second). The
  agent's own content is limited to 200 levels by the MCP filter. A message dropped gets no
  answer. **Not covered (pre-existing):** a `.tm` file with a tree 100000 levels deep, opened
  from disk, makes TeXmacs crash in the same recursive conversions; the stack guard of
  #812/#813 could cover them.
- **Confirmation.** When the host says it took my patch but `live-apply-patch` fails, the
  participant asks for the document again (it used to drop its pending patch and go on).

- **Syntax of messages.** The count of the nesting is made by a scanner that knows only what
  `object->string` writes: parentheses, atoms, strings, numbers, `#t` and `#f`. A message that
  has anything else outside a string (`#\x`, `#| |#`, `;`, quote characters, `|`, a backslash)
  is dropped, because the reader knows more syntax than the scanner (a deep tree hidden
  behind a comment defeated it). The same check is made on the entry files.
- **Arity.** For every peer (agent or person), a patch is refused (rule `arity`) if the
  blocks it changes have a tag that the DRD knows as a primitive with a number of children it
  cannot have (`(ornament)`, `(with)`, `(hlink "x")`...); a participant that receives such a
  patch from its host, or such a document in a welcome, asks for the document again (or does
  not join). `mcp-stree-arity-ok?` is the test, from the rule of the agent's filter.
- **The editor and the typesetter** no longer crash on such trees, whoever makes them (a
  patch that got through, another version, a file): `make_bridge` shows every primitive with
  a wrong arity as an inactive tree, an insertion or a removal that leaves a primitive with a
  wrong arity is told to the typesetter as an assignment of that tree (`edit_modify.cpp`), the
  assertions of the bridges accept the same tag when children come or go, the bridges of
  `with`, `formatting` and the ornaments (and canvas, locus...) do not index a child that is
  not there, and the correction of the cursor (`correct_cursor`) keeps only the part of the
  cursor path that leads into the tree.
- **Random patches in a real buffer** (scenario `edfuzz`, `FZ_N` and `FZ_SEED`): the patches
  that the host would accept are applied in place to a buffer of a process that has the
  editor, as a host or a participant does. It found the crashes above (ornament, canvas,
  compound, formatting, with, the cursor); 8 runs of 20000 iterations pass. In about one
  patch in 2000 the buffer after the in-place application differs from the same patch applied
  as a pure function (the editor corrects the tree: not studied).

- **Structure.** Besides the arity, a patch (and the agent's content) is refused when a table
  has a string for a row, a row a string for a cell, a `tformat` ends with a string, a table or a
  row has no child, or a `document` has none (the editor puts a document with an empty
  paragraph in its place, so the host and the participants would differ). The typesetter
  (`table.cpp`) also treats a string where a table or a row is expected as a table or a row with
  one cell, an empty table or row as one with an empty cell, and the curves of the graphics
  (`concat_graphics.cpp`) show an error unless all their points have two or three
  coordinates and the same number: all of them crashed it.
- **`assign-node` to the label of a string** was applicable (`can_assign_node`): the compound
  became a "string" made of the pointers to its children. It is refused in C++ and by the shape
  check (the label must be a symbol).
- **Symbols.** `#`, `'`, `,`, backquote and `|` are accepted by the scanner of messages inside a
  token, after a character of an atom (the printer writes the name of a macro like `l'x` so,
  and the reader reads it as one symbol), never at the start of a token or after a space, a
  parenthesis or the end of a string.
- `edfuzz` now also typesets (`update-forced` every third patch, `FZ_TYPESET` to change it),
  applies only what the host accepts (`shared-patch-acceptable?`) and fails if the buffer after
  an in-place application differs from the pure result (it did for empty documents, and for
  `assign-node` to a string). 7 runs of 20000 iterations pass. Typesetting arbitrary garbage
  (another primitive with children that its box code does not expect) can still crash the
  typesetter: it was never made robust to every tree.

A pre-existing bug found on the way: under s7 `object->string` cut every list at 40
elements, so a document of more than 40 blocks was cut when it was sent to a
participant (`msg-text` now raises the print length). The tests had small documents.

## Tests

`tests/shared/check.sh [scenario...]` runs each scenario (`s-<name>.scm`) with
two TeXmacs processes at once (`-headless -X`, offscreen, memory capped at
2 GB each and 120 s, a scratch `TMPDIR` and homes with the preference on), which
do their steps one after the other from the delayed commands and meet through
marker files. Scenarios: `basic` (join, edits both ways, save asked by the
participant, once), `concurrent` (edits at once in different paragraphs:
rebased by the host, transformed by the participant), `refuse` (the host
deletes what the participant edits, in both orders, and a patch with a state
the host does not know), `handover`, `death` (kill -9), `takeover` (and
`takeover-window`: B with a window on the offscreen platform), `late` (the
interactive target of a handover stalls: the old host gets the lock back, keeps
its change, and gives the file again when the target is back; one host at a
time), `hostage` (a new host that stalls is not replaced, and the message
is shown), `lockheld` (a lock held by a process that is not TeXmacs, a `perl`
started by the test: nobody hosts, then one hosts and the other joins),
`corrupt` (entries that are empty or of a program that serves nothing make no
difference), `off` (preference off), `stale` (an entry of a dead process; a
wrong token and a packet that is not a message are closed), `slow`, `attack`,
`conflict`, `meta`, `save`, `lockdel` (the lock file removed under a host: made again, or the host stops and joins), `race` (a host that closed the file opens it again at once while its participant has confirmed work: it waits, read-only, and joins), `seeksave` (a Save with no host is queued; a process that is not TeXmacs holds the lock), `diffdir` (different `TMPDIR` and `XDG_RUNTIME_DIR`, one folder), `dupe` (a patch sent twice; a replaced document saved whole), `agent` (the host applies the modes to a participant that is an agent: proposals accepted, rejected, withdrawn, the person's undo, direct, read-only, the content rules in each mode, a saved file with a pending proposal) `agentplain` (a headless participant that does not say it is an agent is treated as one, until the person marks it), `malformed` (patches and messages of a wrong shape, a dishonest inverse) `agentmode` (the mode survives a handover) `fuzz` (random patches with hostile integers, no crash) and `edfuzz` (random patches applied to a real buffer). The tests pause the polling
(`shared-pause!`) to make changes cross, set a process interactive
(`shared-set-interactive!`) and read counters (`shared-stat`). Each scenario
checks that the two documents end equal. Running them with
`TM_SHARED_PREF=off` must fail all but `off`.

## Open points, limits

- **A participant's confirmed work can be replaced, then saved whole.** A
  participant that is busy for more than about 1.3 s (the wait of a joiner is
  300 ms for each participant in the entry, and one second) while its host
  dies, while a third process opens the file meanwhile, can see that process
  host the file from the disk. The participant's confirmed work that was
  not on the disk is then replaced by the new host's document (never combined
  with it). It is saved whole in the recovered folder with a notice, and has to
  be merged by hand.
- **About a second with two hosts after an external removal of a lock
  file, longer if the host is busy or the system cannot open files.** The host
  checks once a second that its lock file is the file at the path. If someone
  removes the file (a cleaner, `rm`) and another process makes and locks a new
  one before that check, both host until it: the first then stops, saves its
  document in the recovered folder and joins the other. A check that comes late
  (a host busy typesetting) makes the window longer. When the file cannot be made
  again, the answer is immediate if the reason is known (no such folder, not
  allowed, another file at the path: the host steps down); for other errors
  (no descriptor left, no room) the host tries at each check and steps down after
  5, that is about 5 s.

- The cursors of the participants, the names of the participants, the menus.
- Two processes that start to host at the same instant are separated by the
  lock. A `git pull` over a shared file is the business of the host at save
  time, as before. The lock is advisory: a program that does not take it is not
  stopped (the files are written by the host, as before, without a lock on the
  document itself). `flock` on a network file system (the folder is local) is
  not a concern here.
- A host that dies after it applied a patch and before it sent it: the patch is
  sent again by its author to the new host, which does not know it was
  applied, if the other participants did not get it (and is then applied
  once); if one of them did get it, the new host (that one, or a participant
  that has it) has it, and the author's second copy is a patch against an older
  state, rebased or refused as any other.
- A joining process waits for the welcome in a loop (500 ms at the most).
- Undo after a remote patch: a crash in `edit_modify_rep::undo` was seen
  headless; it happens with the feature off too (pre-existing), and is not
  studied here.
- A case-insensitive volume: on macOS the key is made from the name the system
  stores (`F_GETPATH`), so that `F.TM` and `f.tm` are one file; on Windows the
  feature is off, and on Linux a volume that ignores case (rare) is not
  handled.
- Heavy use of undo: not studied (design 3.5).
- Windows: the feature is off there (the preference has no effect): it needs a
  system source of random numbers (`BCryptGenRandom`) and a file lock
  (`LockFileEx`), which were not written, and the winsock code was never built.
- While any file is shared, the 20 ms polling command runs (a `recv` per link
  and an `accept`); with the preference off, or no file shared, no timer, no
  socket and no folder is made.
