<TeXmacs|2.1.4>

<style|<tuple|tmdoc|english>>

<\body>
  <tmdoc-title|Synchronization of files and databases>

  <section|Introduction>

  Besides editing remote files directly, a client can keep a local
  directory in sync with a remote directory, and keep some local databases
  (currently the bibliographic database) in sync with the server database.
  Both mechanisms follow the same scheme: the client computes a
  <em|status list> describing what has changed on either side since the
  last synchronization, lets the user resolve conflicts, and then applies
  the resulting operations with a few bulk requests. Both are experimental
  and are mainly driven by the widgets in <verbatim|client/client-widgets.scm>
  (<scm|open-sync-widget>, <scm|remote-interactive-sync>,
  <scm|client-auto-sync>).

  <section|File synchronization>

  <subsection|Bookkeeping>

  The client remembers, for every pair of a local file and a remote file
  that were synchronized, an entry of type <verbatim|"sync"> in the client
  database <scm|(user-database "sync")>, with fields <verbatim|name> (local
  file), <verbatim|remote-name>, <verbatim|date> (local modification time at
  the last synchronization), <verbatim|remote-id> (resource identifier of
  the remote file at the last synchronization) and <verbatim|sync-date>.
  Since every save on the server creates a new resource
  (<hlink|see|collab-remote-fs.en.tm>), a change of the remote identifier
  means that the remote file has been modified. Directories which should be
  synchronized automatically are recorded as entries of type
  <verbatim|"auto-sync"> (<scm|client-auto-sync-add>,
  <scm|client-auto-sync-list>).

  <subsection|Computing the status list>

  <scm|(client-sync-status <scm-arg|local-base> <scm-arg|remote-base>
  <scm-arg|cont>)> in <verbatim|client/client-sync.scm> proceeds as
  follows.

  <\enumerate>
    <item><scm|client-sync-list> enumerates the local tree, skipping files
    for which <scm|dont-sync?> holds (hidden files, backups, <LaTeX>
    auxiliary files, ...). The service <scm|remote-sync-list> returns the
    readable remote tree as a list of <scm|(<scm-arg|dir?>
    <scm-arg|name> <scm-arg|rid>)>.

    <item>Both lists are merged on relative names (<scm|compute-sync-list>).
    For each name, <scm|get-url-sync-info> fetches or creates the
    bookkeeping entry, which yields the current local date <scm|date>, the
    recorded date <scm|date*>, the current remote identifier <scm|remote-id>
    and the recorded one <scm|remote-id*>.

    <item><scm|get-sync-status> classifies the entry:

    <\description>
      <item*|nothing>if both sides are unchanged (for directories: if both
      sides exist and existed);

      <item*|<verbatim|local-delete>>if the local file is unchanged and the
      remote file disappeared;

      <item*|<verbatim|remote-delete>>if the local file disappeared and the
      remote file is unchanged;

      <item*|<verbatim|download>>if only the remote side changed, or the
      file only exists remotely and was never synchronized;

      <item*|<verbatim|upload>>if only the local side changed, or the file
      only exists locally and was never synchronized;

      <item*|<verbatim|conflict<em|xy>>>otherwise, where <em|x> and
      <em|y> are <verbatim|*> or <verbatim|-> according to whether the file
      currently exists locally resp. remotely.
    </description>

    <item><scm|requalify-deleted> turns a deletion into a conflict when
    some descendant of the deleted directory was modified on the other
    side.
  </enumerate>

  Each status line has the form <scm|(<scm-arg|cmd> <scm-arg|dir?>
  <scm-arg|local-name> <scm-arg|local-id> <scm-arg|remote-name>
  <scm-arg|remote-id>)>. The synchronization widget displays the conflicts
  and lets the user choose <verbatim|Keep>, <verbatim|Local> or
  <verbatim|Remote> for each of them; <scm|requalify-conflicting> then
  converts a conflict into an upload, download or deletion.

  <subsection|Applying the changes>

  <scm|(client-sync-proceed <scm-arg|l> <scm-arg|msg> <scm-arg|cont>)>
  processes the status list in four asynchronous steps: uploads (one
  <scm|remote-upload> request with the contents of all files and the
  commit message <scm-arg|msg>), downloads (one <scm|remote-download>
  request), remote deletions (<scm|remote-remove-several>) and local
  deletions. After each successful transfer the bookkeeping entry is
  updated with the new modification time and remote identifier
  (<scm|post-upload>, <scm|post-download>). On the server
  (<verbatim|server/server-sync.scm>), the upload service creates missing
  directories and files and saves existing ones through the same functions
  as the remote file system services, so that every upload creates a new
  version. Remaining conflicts which the user decided to keep are not
  touched.

  The functions <scm|remote-upload> and <scm|remote-download> of
  <verbatim|client-sync.scm> (not to be confused with the services of the
  same names) implement one-way transfers: they treat all conflicts as
  uploads, resp. downloads. <scm|sync-repair> removes bookkeeping entries
  whose local file no longer exists.

  <section|Database synchronization>

  <verbatim|client/client-db-sync.scm> and
  <verbatim|server/server-db-sync.scm> synchronize database entries of
  given <em|kinds>. A kind stands for a set of entry types (table
  <scm|db-kind-table>); <scm|db-sync-kinds> currently only returns
  <verbatim|"bib"> (the user's bibliographic database), unless disabled with
  <scm|db-sync-kind>. Entries are matched by their <verbatim|name> field.

  <\enumerate>
    <item>The client stores the local and remote times of the last
    synchronization in an entry of type <verbatim|"db-sync"> of
    <scm|(user-database "sync")> (<scm|db-last-sync>).

    <item><scm|db-client-sync-status> computes the local changes since the
    local time with <scm|db-change-list> (<verbatim|database/db-convert.scm>,
    which uses the <scm|:modified> query of the database), and asks the
    server for its changes since the remote time with
    <scm|remote-db-changes>. For each name, <scm|db-change-status>
    compares both sides and produces <verbatim|upload>,
    <verbatim|download>, <verbatim|local-delete>,
    <verbatim|remote-delete> or <verbatim|conflict> lines (entries which
    only differ by access rights or meta data are considered equal, see
    <scm|db-equivalent?>). Conflicts are resolved in favour of the server
    by default (<scm|db-requalify-conflicting>).

    <item><scm|db-client-sync-proceed> sends the list to the service
    <scm|remote-db-sync>, which applies uploads and remote deletions to the
    entries owned by the user (uploaded entries are given the user as owner
    and are made readable by <verbatim|"all">), but only if no other change happened on the
    server since the remote time; otherwise it returns <scm|#f> and the
    client must start again. Likewise, the client applies downloads and
    local deletions only if no local change happened in the meantime. New
    versions of entries are created with <scm|db-update-entry>, which
    keeps a <verbatim|newer> link from the old version.

    <item>Finally <scm|db-dub-in-sync> records the new times.
  </enumerate>

  This optimistic scheme avoids locks: a synchronization is simply retried
  when the other side changed concurrently.

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
