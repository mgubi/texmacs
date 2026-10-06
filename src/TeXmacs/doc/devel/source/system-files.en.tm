<TeXmacs|2.1.4>

<style|<tuple|tmdoc|english>>

<\body>
  <tmdoc-title|Files and caches>

  This page describes the file routines of
  <source-link|System/Files/file.hpp|src/System/Files/file.hpp> and <source-link|file.cpp|src/System/Files/file.cpp>, the caches of
  <source-link|System/Misc/data_cache.cpp|src/System/Misc/data_cache.cpp>, the persistent store of
  <source-link|System/Misc/persistent.cpp|src/System/Misc/persistent.cpp> and the generated files of
  <source-link|System/Files/make_file.cpp|src/System/Files/make_file.cpp>. All of them take <abbr|URL>s (see
  <hlink|URLs, resolution and concretization|system-urls.en.tm>) and end up
  in the platform functions <cpp|texmacs_fopen>, <cpp|texmacs_stat>, ...
  of <hlink|the platform layers|system-platforms.en.tm>.

  <section|Reading and writing files>

  <\explain>
    <cpp|bool load_string (url u, string& s, bool fatal, bool lock= true)>
  <|explain>
    Reads the whole file into <cpp|s>. Unless <cpp|u> is already a rooted
    name, it is first resolved with the filter <verbatim|"fr">; the read
    fails if this gives nothing or a directory. The concretized name is
    looked up in the file cache (below); otherwise the file is opened with
    <cpp|texmacs_fopen (name, "r", lock)>, which also takes an exclusive
    lock on the file when <cpp|lock> is set. Files of at most 10000 bytes
    in the cached directories, and files which were already cached, are
    stored in the cache. As in the rest of the file layer, the result is
    <cpp|true> on <em|failure>; with <cpp|fatal> a failure raises an error
    instead. The empty <abbr|URL> is not an error: it gives the empty
    string.
  </explain>

  <\explain>
    <cpp|bool save_string (url u, string s, bool fatal= false)>
  <|explain>
    <cpp|append_string (u, s, fatal)> is the same with appending.
    <verbatim|tmfs> <abbr|URL>s are passed to the <scheme> save handler
    (<cpp|save_to_server>); they cannot be appended to. Other names are
    resolved with the empty filter, which picks the first candidate of a
    search path whether it exists or not, and written with
    <cpp|texmacs_fopen> in mode <verbatim|"w"> or <verbatim|"a">. The file
    cache is updated for small cached files, and the parent directory is
    declared out of date (<cpp|declare_out_of_date>), so that the directory
    and attribute caches are refreshed.
  </explain>

  <section|File tests and attributes>

  <\description>
    <item*|<cpp|is_of_type (u, filter)>>Tests each letter of the filter
    (see <hlink|resolution|system-urls.en.tm>). For web <abbr|URL>s, the
    file is downloaded and only <verbatim|d>, <verbatim|l>, <verbatim|w>
    and <verbatim|x> fail; <verbatim|tmfs> <abbr|URL>s ask the
    <scheme> permission handler for <verbatim|r> and <verbatim|w>; a
    ramdisc is tested through its temporary file. Local files are tested
    with the attributes returned by <cpp|texmacs_stat>, possibly from the
    attribute cache. On <name|Windows>, the test <verbatim|x> accepts
    readable files with the suffixes <verbatim|exe>, <verbatim|com> and
    <verbatim|bat> and adds <verbatim|.exe> to names without one of these
    suffixes. <cpp|is_regular>, <cpp|is_directory> and
    <cpp|is_symbolic_link> are shortcuts.

    <item*|<cpp|file_size (u)>, <cpp|last_modified (u, cache_flag)>>The
    size and the modification time in seconds; <math|-1> and the most
    negative <cpp|int> but one for web and <verbatim|tmfs> <abbr|URL>s and
    for missing files.

    <item*|<cpp|is_newer (u1, u2)>>Whether <cpp|u1> was modified after
    <cpp|u2>.

    <item*|<cpp|file_format (u)>>The format of a file, from its suffix
    (<cpp|suffix_to_format>) or, for <verbatim|tmfs>, from
    <scm|tmfs-format>.
  </description>

  <section|Special names>

  <\description>
    <item*|<cpp|url_temp_dir ()>>The temporary directory of the session,
    <verbatim|$TEXMACS_HOME_PATH/system/tmp/<em|pid>> (the start time
    instead of the process id on 32-bit <name|Windows>), created on first
    use. Directories of processes which no longer run are removed at boot
    time (<hlink|paths, directories and settings|system-boot.en.tm>).

    <item*|<cpp|url_temp (suffix)>>A fresh name
    <verbatim|tmp_<em|random><em|suffix>> in that directory. The file is
    not created.

    <item*|<cpp|url_numbered (dir, prefix, postfix)>>The first name
    <verbatim|<em|prefix><em|i><em|postfix>> which does not exist in
    <cpp|dir>, creating <cpp|dir> if needed.

    <item*|<cpp|url_scratch ()>, <cpp|is_scratch (u)>>Names
    <verbatim|no_name_<em|i>.tm> in
    <verbatim|$TEXMACS_HOME_PATH/texts/scratch>, used for new buffers.

    <item*|<cpp|url_backup (u)>, <cpp|is_backup (u)>>A backup name in
    <verbatim|$TEXMACS_HOME_PATH/texts/backup>, made of the base name of
    <cpp|u>, a hash of the whole <abbr|URL> and the suffix.
  </description>

  <section|Directories and file operations>

  <\description>
    <item*|<cpp|read_directory (u, error_flag)>>The sorted list of entries
    (including <verbatim|.> and <verbatim|..>) of the directory <cpp|u>,
    resolved with the filter <verbatim|"dr">; possibly from the directory
    cache. <cpp|error_flag> is only set when the directory is actually
    opened: it is left untouched when the result comes from the cache or
    when <cpp|u> does not resolve, so callers must initialize it.

    <item*|<cpp|move>, <cpp|copy>, <cpp|append_to>><cpp|move> renames,
    <cpp|copy> and <cpp|append_to> go through <cpp|load_string> and
    <cpp|save_string> or <cpp|append_string> (so file permissions are not
    copied).

    <item*|<cpp|remove (u)>>Removes all readable regular files which
    match <cpp|u> (wildcards are allowed:
    <cpp|remove (dir * url_wildcard ("*"))> empties a directory).
    <cpp|rmdir> removes matching empty directories and
    <cpp|rmdir_recursive> a whole tree.

    <item*|<cpp|mkdir (u)>>Creates a directory and its missing parents,
    with mode <verbatim|0744>; does nothing if <cpp|u> exists.
    <cpp|change_mode> sets the mode.

    <item*|Searching><cpp|search_file_in (dir, name)> searches a directory
    tree for a file name, caching directory listings for 10 seconds;
    <cpp|search_file_upwards> also searches the ancestors up to one of
    a list of stop names. <cpp|search_sub_dirs> lists all subdirectories
    (used for the style and text paths). <cpp|grep>, <cpp|search_score>
    and <cpp|file_completions> support searching in the documentation and
    file name completion.

    <item*|Shell helpers><cpp|system (cmd, u1, ...)> and
    <cpp|eval_system (cmd, u1, ...)> append the concretized, shell quoted
    names to a command (<hlink|programs, web requests, messages and
    timing|system-utils.en.tm>).
  </description>

  <section|The data cache>

  Starting <TeXmacs> reads thousands of files below
  <verbatim|$TEXMACS_PATH>: <scheme> modules, style files, fonts, icons and
  documentation. To avoid most of the corresponding system calls, the file
  layer keeps several caches, which are saved in
  <verbatim|$TEXMACS_HOME_PATH/system/cache> and reloaded at the next
  start.

  <subsection|Buffers>

  The cache is a single hash table from pairs <verbatim|(<em|buffer>
  <em|key>)> to trees, with the following buffers; the key is always a
  concretized file or directory name.

  <\description>
    <item*|<verbatim|dir_cache.scm>>The contents of directories, as
    stored by <cpp|read_directory>.

    <item*|<verbatim|stat_cache.scm>>The mode, modification time and size
    of files, as stored by the attribute routines, or <verbatim|#f> for a
    file which does not exist.

    <item*|<verbatim|file_cache>>The contents of small files.

    <item*|<verbatim|doc_cache>>The contents of small documentation files;
    only loaded when a documentation file is first read.

    <item*|<verbatim|font_cache.scm>>Locations of <name|TrueType> fonts,
    used by <source-link|Plugins/Freetype/tt_file.cpp|src/Plugins/Freetype/tt_file.cpp>.

    <item*|<verbatim|validate_cache.scm>>For each directory, the
    modification time it had when its entries were cached.
  </description>

  <cpp|cache_set>, <cpp|cache_get>, <cpp|is_cached> and <cpp|cache_reset>
  access the table; <cpp|cache_set> marks the buffer as changed.

  <subsection|What is cached>

  Only files below a few fixed directories are cached, as decided by the
  predicates <cpp|do_cache_dir>, <cpp|do_cache_stat>,
  <cpp|do_cache_stat_fail>, <cpp|do_cache_file> and <cpp|do_cache_doc>:

  <\itemize>
    <item>directory listings and file attributes below
    <verbatim|$TEXMACS_PATH> and the documentation path (attributes also
    below <verbatim|$TEXMACS_HOME_PATH/fonts>);

    <item>missing files below the same directories, except style files
    (<verbatim|.ts>), so that a new style file is found without clearing
    the cache;

    <item>contents of files below <verbatim|$TEXMACS_PATH> and
    <verbatim|$TEXMACS_HOME_PATH/fonts>, except style files, in the file
    cache, and of files below the documentation path in the documentation
    cache.
  </itemize>

  The tests compare string prefixes of concretized names; the three base
  directories are fixed by <cpp|cache_initialize> during the boot.

  <subsection|Validity>

  An entry for a file is used only if the directory containing it is
  <em|up to date>: <cpp|is_up_to_date (dir)> compares the current
  modification time of the directory with the one recorded in
  <verbatim|validate_cache.scm>, records the new time if it differs, and
  memorizes the answer for the rest of the session. A directory whose time
  changed is thus out of date for the whole session, and its entries are
  reloaded from disk; it becomes up to date again at the next start.
  <cpp|declare_out_of_date (dir)> forces this after <TeXmacs> itself wrote
  into a directory, and <cpp|is_recursively_up_to_date> checks a whole
  tree.

  <subsection|Saving and loading>

  <cpp|cache_memorize ()> writes all changed buffers to their files. It is
  called by <cpp|edit_interface_rep::update_menus>, that is, after user
  actions, so the cache is saved regularly while <TeXmacs> runs.
  <cpp|cache_refresh ()> empties the table and reloads the buffers other
  than the documentation cache. <cpp|cache_initialize ()>, called by
  <cpp|texmacs_entrypoint>, determines the base directories, calls
  <cpp|cache_refresh>, and removes the font error files in
  <verbatim|$TEXMACS_HOME_PATH/fonts/error> if a font directory changed.
  The file and documentation caches are stored as plain text, with
  entries separated by the line <verbatim|%-%-tm-cache-%-%>; the other
  buffers are stored as <scheme> tuples.

  <subsection|Other files in the cache directory>

  The same directory holds caches which are managed elsewhere: the style
  caches <verbatim|__<em|style>__...> of
  <source-link|Data/Document/new_style.cpp|src/Data/Document/new_style.cpp> (see <hlink|style and document
  DRDs|drd-documents.en.tm>), <verbatim|plugin_cache.scm> of
  <source-link|kernel/texmacs/tm-plugins.scm|TeXmacs/progs/kernel/texmacs/tm-plugins.scm>, and the persistent stores of
  the <scheme> documentation tools (below). The command line options
  <verbatim|-delete-cache>, <verbatim|-delete-style-cache>,
  <verbatim|-delete-file-cache>, <verbatim|-delete-doc-cache>,
  <verbatim|-delete-font-cache> and <verbatim|-delete-plugin-cache> remove
  parts of it (<hlink|the main program|server-startup.en.tm>).

  <section|The persistent store>

  <source-link|System/Misc/persistent.cpp|src/System/Misc/persistent.cpp> implements a simple key-value
  store of strings on disk, exported to <scheme> as <scm|persistent-set>,
  <scm|persistent-get>, <scm|persistent-has?>, <scm|persistent-remove>
  and <scm|persistent-file-name> (and <scm|persistent-ref> in
  <source-link|kernel/boot/abbrevs.scm|TeXmacs/progs/kernel/boot/abbrevs.scm>). A store is a directory. The entries
  are distributed over files according to a hash of the key: a file holds
  at most 10 entries and 4096 bytes, after which it is replaced by a
  directory of up to 26 files <verbatim|a> to <verbatim|z>, selected by the
  next digit of the hash in base 26. Values are cached in memory once read.
  <cpp|persistent_file_name (dir, suffix)> allocates fresh file names in
  the subdirectory <verbatim|_> of a store, using a counter stored in the
  file <verbatim|_/_>. The documentation tools use this to keep the
  collected <scheme> and macro documentation
  (<source-link|doc/apidoc-collect.scm|TeXmacs/progs/doc/apidoc-collect.scm>).

  <section|Generated files>

  <cpp|make_file (cmd, data, args)> (<source-link|System/Files/make_file.cpp|src/System/Files/make_file.cpp>)
  produces a file in <verbatim|$TEXMACS_HOME_PATH/system/make> whose name
  is a hash of the command and its arguments. The commands are
  <cpp|CMD_GET_FROM_WEB> and <cpp|CMD_GET_FROM_SERVER> (local copies of
  remote images, named with the suffix of the remote file) and
  <cpp|CMD_APPLY_EFFECT> (an image to which an effect has been applied, as
  <abbr|PNG>). A file <verbatim|<em|hash>.check> next to the result
  records the modification times of the local arguments; the result is
  reused across sessions as long as these times do not change, and within
  a session the name is remembered in memory. Remote arguments are first
  fetched with recursive calls; <verbatim|tmfs://artwork/...> images are
  taken from <verbatim|$TEXMACS_HOME_PATH/misc/...> when they exist there.

  <section|Pitfalls>

  <\itemize>
    <item>Cached contents are only invalidated when the <em|directory>
    changes. Editing a file in place below <verbatim|$TEXMACS_PATH>
    usually does not change the modification time of its directory, so
    <TeXmacs> keeps using the old contents of small files (and the old
    size and date) until a file is added to or removed from the directory,
    or until the cache is deleted with <verbatim|-delete-file-cache>. This
    often surprises developers who edit <scheme> files of the
    installation.

    <item><cpp|is_newer (u1, u2)> returns <cpp|false> as soon as one of
    the two files has an entry in the attribute cache
    (<verbatim|System/Files/file.cpp:397-398>, under a comment
    \Pwhy was this?\Q), without comparing
    anything. Since every test like <cpp|exists> creates such entries for
    files below <verbatim|$TEXMACS_PATH> and the documentation path,
    <scm|url-newer?> is practically always false there. For instance,
    after <scm|(url-exists? a)> and <scm|(url-exists? b)> on
    <verbatim|$TEXMACS_PATH/progs/init-texmacs.scm> and
    <verbatim|$TEXMACS_PATH/LICENSE>, <scm|(url-newer? a b)> returns
    <scm|#f> although the first file is more recent. This affects in
    particular the incremental update of web sites built from the
    documentation (<source-link|doc/tmweb.scm|TeXmacs/progs/doc/tmweb.scm>).

    <item>The filter <verbatim|l> never succeeds on <name|Unix>: the
    attributes come from <cpp|texmacs_stat>, which follows symbolic links
    (<cpp|stat>, not <cpp|lstat>), and the <cpp|link_flag> argument of
    the attribute routine is ignored. <cpp|is_symbolic_link> and
    <scm|url-link?> therefore always return false (checked with a link
    to a regular file: <scm|url-link?> gives <scm|#f>, <scm|url-regular?>
    gives <scm|#t>).

    <item>Files are locked with an exclusive lock both for reading and for
    writing, so concurrent readers of the same file are serialized. When
    writing, the file is truncated by <cpp|fopen> <em|before> the lock is
    taken, so the lock does not protect a concurrent reader from seeing a
    truncated file.

    <item>A generated file whose arguments are remote is never refreshed:
    their entry in the <verbatim|.check> file is the constant
    <verbatim|#f>. A remote image which changes on the server keeps its old
    local copy until <verbatim|system/make> is cleared.

    <item><cpp|remove> only removes readable regular files: it resolves
    its argument with the default filter <verbatim|"fr">. Use <cpp|rmdir>
    or <cpp|rmdir_recursive> for directories.
  </itemize>

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
