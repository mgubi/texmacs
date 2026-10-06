<TeXmacs|2.1.4>

<style|<tuple|tmdoc|english>>

<\body>
  <tmdoc-title|Security, encryption and trusted documents>

  <section|Introduction>

  <TeXmacs> documents are not just passive data: they may contain
  <scheme> code, through the <markup|extern> primitive, through
  hyperlinks and <markup|action> tags whose target is a script, through
  observers attached to loci, and through the scripts of interactive
  widgets. Documents may also contain confidential material which should be
  stored encrypted on disk. This chapter describes the mechanisms which
  <TeXmacs> provides for both concerns:

  <\itemize>
    <item>the notion of <em|trusted> (\Psecure\Q) documents and the checker
    <scm|secure?> which decides whether a piece of <scheme> code coming from
    an untrusted document may be evaluated;

    <item>the interface with <name|GnuPG>, which allows to encrypt regions
    of a document for a list of recipients or with a passphrase, and to
    encrypt whole documents with a passphrase;

    <item>the <em|wallet>, an encrypted store for passphrases, and its
    optional connection to the keychain of the operating system;

    <item>the generation of passwords and salts, used by the <TeXmacs>
    server.
  </itemize>

  The cryptography of the client/server system (<abbr|TLS>, certificates,
  password hashing on the server) is described in the chapters on
  <hlink|the server|collab-server.en.tm> and on <hlink|the
  protocol|collab-protocol.en.tm>, and is not repeated here.

  <section|Overview>

  The two concerns are handled by independent code:

  <\description>
    <item*|Script security>is implemented partly in <c++> and partly in
    <scheme>. A document is trusted if its file name lies below one of the
    directories of <verbatim|$TEXMACS_SECURE_PATH>
    (<cpp|is_secure>, <source-link|System/Classes/url.cpp|src/System/Classes/url.cpp>). The typesetter
    records this in the environment (<cpp|edit_env_rep::secure>); code
    coming from untrusted documents is passed to the <scheme> predicate
    <scm|secure?> (<source-link|kernel/texmacs/tm-secure.scm|TeXmacs/progs/kernel/texmacs/tm-secure.scm>), which accepts
    an expression only if it calls functions declared secure. The user
    preference <verbatim|security> (\Paccept no scripts\Q, \Pprompt on
    scripts\Q, \Paccept all scripts\Q) decides what happens to the other
    expressions.

    <item*|Encryption>is implemented entirely in <scheme>, in
    <verbatim|progs/security/>, on top of the external program
    <verbatim|gpg>. The only hooks in <c++> are the call of
    <scm|tree-export-encrypted> in <cpp|export_tree>
    (<source-link|Texmacs/Data/new_buffer.cpp|src/Texmacs/Data/new_buffer.cpp>) and the generic
    <cpp|evaluate_system> routine which runs <verbatim|gpg> with
    passphrases sent through pipes. The markup for encrypted regions is
    defined in <source-link|packages/standard/std-security.ts|TeXmacs/packages/standard/std-security.ts>. All encryption
    features are disabled unless the preference <verbatim|experimental
    encryption> is <verbatim|on> and a <verbatim|gpg> executable is found.
  </description>

  <section|Source files>

  Paths below are relative to <verbatim|src/TeXmacs/progs/> for <scheme>
  files and to <verbatim|src/src/> for <c++> files.

  <\description-paragraphs>
    <item*|<source-link|kernel/texmacs/tm-secure.scm|TeXmacs/progs/kernel/texmacs/tm-secure.scm>>The checker
    <scm|secure?>, <scm|secure-eval> and the list of primitive functions
    which are declared secure.

    <item*|<source-link|kernel/texmacs/tm-define.scm|TeXmacs/progs/kernel/texmacs/tm-define.scm>>The option
    <scm|:secure> of <scm|tm-define>.

    <item*|<source-link|System/Classes/url.cpp|src/System/Classes/url.cpp>,
    <source-link|System/Boot/init_texmacs.cpp|src/System/Boot/init_texmacs.cpp>>The predicate <cpp|is_secure>
    and the default value of <verbatim|$TEXMACS_SECURE_PATH>.

    <item*|<source-link|Typeset/Env/env_exec.cpp|src/Typeset/Env/env_exec.cpp>,
    <source-link|Style/Evaluate/evaluate_rewrite.cpp|src/Style/Evaluate/evaluate_rewrite.cpp>,
    <source-link|Typeset/Concat/concat_active.cpp|src/Typeset/Concat/concat_active.cpp>>The checks on
    <markup|extern> and on observers, and the transmission of the trust
    status to links.

    <item*|<source-link|link/link-navigate.scm|TeXmacs/progs/link/link-navigate.scm>>The execution of scripts which
    are the target of a link or an <markup|action> (<scm|execute-script>).

    <item*|<source-link|texmacs/texmacs/tm-server.scm|TeXmacs/progs/texmacs/texmacs/tm-server.scm>>The preference
    <verbatim|security> and <scm|set-script-status>.

    <item*|<source-link|security/gpg/gpg-base.scm|TeXmacs/progs/security/gpg/gpg-base.scm>>The interface to
    <verbatim|gpg>: key generation, key listing, import and export,
    encryption and decryption, passphrase encryption, collected public
    keys.

    <item*|<source-link|security/gpg/gpg-edit.scm|TeXmacs/progs/security/gpg/gpg-edit.scm>>Encrypted regions in
    documents, encryption of whole buffers, the save and load hooks.

    <item*|<source-link|security/gpg/gpg-widgets.scm|TeXmacs/progs/security/gpg/gpg-widgets.scm>,
    <source-link|security/gpg/gpg-menu.scm|TeXmacs/progs/security/gpg/gpg-menu.scm>>Dialogs (key manager, passphrase
    prompts, recipient selection), menus and preferences.

    <item*|<source-link|security/gpg/gpg-wallet.scm|TeXmacs/progs/security/gpg/gpg-wallet.scm>,
    <verbatim|security/wallet/>>The wallet.

    <item*|<verbatim|security/keychain/>>Access to the keychain of
    <name|macOS> (<verbatim|security> command) and <name|Windows>
    (<verbatim|winwallet> helper).

    <item*|<source-link|security/password.scm|TeXmacs/progs/security/password.scm>>Generation of passwords and
    salts.

    <item*|<source-link|packages/standard/std-security.ts|TeXmacs/packages/standard/std-security.ts>>Rendering of the
    encrypted and decrypted regions; the packages
    <verbatim|packages/customize/encryption/gpg-info-level-*.ts> choose
    how much information about the recipients is displayed.
  </description-paragraphs>

  <section|Contents of this chapter>

  <\traverse>
    <branch|Trusted documents and secure evaluation of
    scripts|security-scripts.en.tm>

    <branch|<name|GnuPG> encryption of documents|security-gpg.en.tm>

    <branch|The wallet, system keychains and passwords|security-wallet.en.tm>

    <branch|Threat model and pitfalls|security-pitfalls.en.tm>
  </traverse>

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
