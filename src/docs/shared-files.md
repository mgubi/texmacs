# Shared files

Several TeXmacs processes of the same user which open the same file stay in
sync, with no server. This is part 1a of step 1 of the
[MCP server design](https://github.com/mgubi/texmacs/wiki/MCP-server-design)
(sections 3 and 4.4). Not in this part: the cursors of the other
participants, `texmacs --mcp`, agents and proposals, the menus.

**Switch.** The preference `share-open-files` (`on`/`off`), default **off**:
nothing changes for a user who does not turn it on. There is no menu yet:
`(set-preference "share-open-files" "on")`. It is read when a file is loaded;
a file already shared stays so.

## What is shared

A file is shared when it is a plain local file in TeXmacs format (`.tm`) which
exists, and the per-user folder can be made. What is shared is the **body** of
the buffer; the style, the initial environment and the references travel at
joining only (a change of the style by one participant is not propagated:
open point). Loading goes through `load-buffer-main` as before (the question
for a large source file, wildcards, the startup buffer `startup-reserved` are
upstream of the join); the join is made in `load-buffer-check-autosave`, so
that a joining process neither reads the file nor asks about an autosave file,
and the host registers itself in `load-buffer-open`, after any load.

## Pieces

| File | What |
|---|---|
| `src/System/Link/local_link.{hpp,cpp}` | loopback links (`local-listen`, `local-accept`, `local-connect`, `local-send`, `local-receive`, ...) as `tm_link_rep` over a nonblocking socket of 127.0.0.1, with `write_packet`/`read_packet` (the `<length>\n<payload>` packets) of `tm_link`; the helpers of the entries (`shared-user-dir`, `shared-real-path`, `shared-create-file`, `shared-replace-file`, `shared-random-token`, `shared-process-alive?`) |
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

The folder is `$XDG_RUNTIME_DIR` (Linux) or `$TMPDIR` or `/tmp`, then
`texmacs-<uid>/shared`; on macOS `$TMPDIR` is a folder of the user. On Windows there is none. It is made with mode 0700, and if it exists it
must be a directory owned by the user (else no sharing). It does not depend on
`TEXMACS_HOME_PATH`.

One entry per file, `f-<hash of the canonical path>.entry`, containing
`(entry key port token pid interactive host-id)`: the canonical path (symlinks
and relative names resolved by `realpath`), the port of the host on
127.0.0.1, a random token of 128 bits from `/dev/urandom` (on Windows, a weak
fallback: the folder is the protection), the pid, whether the host is
interactive, and its id. An entry is created with `O_EXCL` by the host and
replaced atomically (temporary file and rename) when hosting changes, so that
two processes which open the file together do not both host it, and a reader
never sees half an entry. A host whose connection is refused has a stale
entry, which is removed; the entries of dead pids are swept when a host starts.

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
`$TEXMACS_HOME_PATH/system/recovered/shared-lost-<pid>-<n>.tm`, the footer
says so, and an interactive TeXmacs asks whether to open it (a question
stays until answered). Text is what can be recovered this way; the deletions
and moves among the lost changes are not (the text they removed is still in
the document), and the notice says that changes were not kept. Why a document
and not the inverse patches: an inverse can only be applied to the document
it was made for, and the host's document has moved; the text can be pasted
anywhere.

## Saving

Only the host writes the file. A Save in a participant (`save-buffer-main`)
sends its local changes to the host and, when the host has confirmed all of
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

- **Join**: a process which loads a file with an entry connects, says hello,
  gets `(welcome state document modified roster host)` (waiting 500 ms at the
  most, in a loop on the main thread) and makes the buffer from it
  (`buffer-set`, `buffer-pretend-saved`). **A host whose process is alive is
  never replaced**: if it does not answer in time (it is busy or stopped) or
  refuses the connection, the file is opened from the file, not shared, with
  a message in the footer; an entry is removed only when its process is dead.
  So two hosts never write the same file. A pid reused by another program
  keeps a file from being shared until that program ends.
- **An interactive process joins a file hosted by an interactive one**: it is
  a participant (the first interactive process hosts). **An interactive
  process joins a file hosted by a non-interactive one**:
  the host flushes its changes and sends `(become-host state)` to it; it
  accepts if its state is the host's (otherwise `host-failed` and the host goes
  on), listens, writes the entry (new token) and answers `(host-ready port
  token)`; the old host sends `(rehost port token id)` to the others, and
  connects itself to the new host, as a participant. Pending changes are
  sent again to the new host with their state (the new host has the history, as
  it kept the states); a participant whose state it does not have gets the whole
  document (its pending changes are dropped, and it is told).
- **The host closes the file** (`buffer-close`): the same handover to the
  first participant by priority, waiting for the answer (4 s), then it
  leaves; without a participant it removes its entry. Changes of the host which
  were not yet sent are sent before.
- **A buffer removed by the C++ code without `buffer-close`** (the window and
  buffer killed together): `notify-buffer-closed`, which `remove_buffer`
  calls, makes the process leave the sharing as above.
- **The host dies**: the participants see the link close. Each computes the
  candidates from the last roster (without the dead host); the first becomes
  host from its own copy (its pending changes become new states); the others
  wait up to 3 s for an entry of another host and connect to it (hello with
  their state: same, or the patch since, or the whole document), then
  leave out the candidate that did not appear and try the next one.

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
`takeover-window`: B with a window on the offscreen platform), `off`
(preference off), `stale` (an entry of a dead process; a wrong token and a
packet that is not a message are closed). The tests pause the polling
(`shared-pause!`) to make changes cross, set a process interactive
(`shared-set-interactive!`) and read counters (`shared-stat`). Each scenario
checks that the two documents end equal. Running them with
`TM_SHARED_PREF=off` must fail all but `off`.

## Open points, limits

- The cursors of the participants, the names of the participants, the menus.
- Two processes which start to host at the same instant are separated by the
  entry (`O_EXCL`). When the host dies, a process that opens the file before
  its successor has written the new entry finds a dead pid and hosts; the
  entry is replaced atomically by whichever writes last, and the other joins
  it (or resyncs). A `git pull` over a shared file is the business of the host
  at save time, as before.
- A joining process waits for the welcome in a loop (500 ms at the most); two
  processes joining each other at once would wait for each other that long.
- Undo after a remote patch: a crash in `edit_modify_rep::undo` was seen
  headless; it happens with the feature off too (pre-existing), and is not
  studied here.
- Heavy use of undo: not studied (design 3.5).
- Windows: the feature is off there (the preference has no effect): it needs a
  system source of random numbers (`BCryptGenRandom`) and an exclusive create
  of the entries, which were not written, and the winsock code was never built.
- While any file is shared, the 20 ms polling command runs (a `recv` per link
  and an `accept`); with the preference off, or no file shared, no timer, no
  socket and no folder is made.
