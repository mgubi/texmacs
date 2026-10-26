# GPL compliance of the Claude Code contributions in mgubi/texmacs

*26 October 2026 — Massimiliano Gubinelli*

The fork [mgubi/texmacs](https://github.com/mgubi/texmacs) adds code on top of
the official TeXmacs, and most of that code was written with Claude Code. None
of it is under a license that is incompatible with GPL 3 or later. A line-by-line
audit found a few passages adapted from permissively licensed code without the
notice their license requires. Those notices have now been added. One question is
still open: the license of five web tutorials that were recreated from the old
ones on texmacs.org.

This report is a technical audit, not legal advice.

## Scope and method

- **What was audited:**
  - every commit on top of `svn_sync`, the mirror of the official SVN;
  - on `upsilon_texmacs`, 2,698 commits other than merges, 2,499 of them co-authored by Claude;
  - on `wip_fixes`, 557 commits, 534 co-authored by Claude;
  - the unmerged commits of the open PR branches;
  - 4,187 added files, plus every modified file.
- **Who audited:** six independent review agents, one per area:
  - the Scheme engines (s7, femtolisp);
  - tree-sitter, LSP and the code features;
  - the user interfaces, graphics and the browser build;
  - the converters, CSL, BibTeX and the plug-ins;
  - the core in C++ and Scheme;
  - data, documentation, icons and tests.

  Each agent read the code, while separate checks were run by hand on the findings that mattered most.
- **How they looked:**
  - They searched for provenance markers: "port", "adapted", "based on", URLs, Stack Overflow, blogs, foreign copyright lines.
  - Where code looked lifted, they compared it with the likely upstream by fetching the upstream source.
  - They checked the origin of test data, documents, fonts and icons.
  - They compared vendored code byte for byte with its upstream commits.
- **What counted as a problem:**
  - code or data copied, closely translated or ported from third-party work under a GPL-incompatible license, or with no license;
  - code copied under a compatible license but without the notice that license requires.

  Particular attention went to what an AI model might reproduce from memory: famous library functions, ports of known projects, upstream test suites, snippets from Stack Overflow or blogs.

## Findings and fixes

Nothing incompatible was found. All findings are notice or attribution gaps, and
all are fixed in the pull requests below:
- [#504](https://github.com/mgubi/texmacs/pull/504) on `upsilon_texmacs`, merged as `362dc76f34`;
- [#506](https://github.com/mgubi/texmacs/pull/506) and [#507](https://github.com/mgubi/texmacs/pull/507) on `wip_fixes`, merged as `613e34cb18` and `316328f8d8`.

| What | Origin | License | Fix |
|---|---|---|---|
| Clay grid layout (`clay_grid.h`, `GridComponent`) | Message in Clay's Discord, 2025, before the Claude sessions | **None** | Unused: removed (#504) |
| Inflater in `src/System/Files/zip_files.cpp` (Claude) | Close translation of `puff.c` 2.3 by Mark Adler (zlib `contrib/puff`) | zlib | Marked as altered, full notice added (#504) |
| Slug glyph shaders in the Vue GPU backends and `thorvg-bench` (Claude) | Follow Eric Lengyel's reference Slug shaders | MIT | Credit + `misc/licenses/Slug-MIT.txt` (#504) |
| `last-pair` in `compat-s7.scm` (2021) | SRFI-1 reference implementation, Olin Shivers | Permissive, notice required | Notice added (#504) |
| `hash` wrapper in `compat-s7.scm` and `compat-femtolisp.scm` | SRFI-69 reference implementation, Panu Kalliokoski | MIT | Credit added (#504) |
| femtolisp patches 0028–0036 | Fixes from Julia's `src/flisp` | BSD 3-clause | Named in the license file (#504) |
| Mersenne Twister in femtolisp (`llt/mt19937ar.c`) | Matsumoto, Nishimura | BSD 3-clause; binary notice required | Notice in the shipped license file (#504) |
| Scheme ports of the tree-sitter generator, runtime and grammar scanners; `ts_tables.cpp` (Claude) | tree-sitter and its grammars | MIT | Upstream copyright lines added (#504) |
| Port of `get_indent` of nvim-treesitter in `syntax_region.cpp` (Claude) | nvim-treesitter | Apache 2.0 | License, origin and changes stated (#504) |
| Third-party license texts in the packages: s7, femtolisp, tree-sitter, Clay, SDL3, ThorVG, MuPDF and its libraries, GnuTLS, FreeType, Hunspell… | — | Various, all compatible | `TeXmacs/misc/licenses/` with a README; bundles collect the system libraries' licenses (#504) |
| Browser build: TikZJax and Asymptote-web pointed to npm packages (built files only) | — | GPL 3, LGPL 3 | Source repositories and commits served with them (#504) |
| MuPDF patch for the browser build | MuPDF | AGPL 3, §5(a) | Dated "modified" notice in the patched file (#504) |
| Fira, Libertine, TeX Gyre shipped without license files; `fonts/LICENSE` claimed the LPPL for all fonts | **Also in the official TeXmacs** | OFL, GPL with font exception, GUST | License files added, notice corrected (#506, #504) |
| `pixmaps/LICENSE` claimed the GPL for the Lucide icons | Lucide | ISC/MIT | Notice corrected (#504) |
| Twemoji attribution | Twitter, Mozilla | CC BY 4.0 | Licensor line and credits page (#504) |
| Icon specimen PDFs embedded Helvetica and Menlo | Apple fonts | Proprietary | Regenerated with DejaVu (#504) |
| Recreated web tutorials named their authors but not their copyright | Ratier, Lesourd, Seidl | Unknown | Copyright lines added (#507); license still open, see below |

Three of these are notice gaps in code written with Claude: the inflater, the
Slug shaders and the tree-sitter ports. In each case the code follows a
permissively licensed original. The inflater and the Slug shaders follow it
closely enough to need its notice; the tree-sitter ports were openly described
as ports, but lacked the upstream copyright lines.

## What was found clean

- **Vendored code:**
  - s7 is upstream commit `6ce8ff0` with patches 0001–0007.
  - femtolisp is upstream commit `ec76010` with patches 0001–0036.

  Applying the patches to those upstream commits gives files byte-identical to ours. The tree-sitter runtime is identical to 0.27.1, and XZ Embedded is unchanged.
- **Grammars and queries:**
  - All 22 grammars are MIT and identical to their upstream at the recorded commits.
  - The 17 indentation queries are identical to nvim-treesitter (Apache 2.0).
  - No queries come from helix (MPL 2.0) or zed.
- **CSL processor (written with Claude):** an independent implementation of the CSL 1.0.2 specification. It is not a port of citeproc-js, whose CPAL and AGPL-1.0 would be incompatible: it shares none of citeproc-js's vocabulary, data model or disambiguation design. This judgment rests on structure and naming; it was not diffed against citeproc-js. The CSL test suite has no license and is not shipped.
- **Other Claude-written subsystems** (markdown, docx/odt, git integration, Zotero, LSP client): original code.
  - The office tests build their documents inline; no third-party sample documents are committed.
  - The git integration calls git as a program; no code from git (GPL 2 only) or libgit2.
- **GPU backends and the browser's JavaScript:**
  - No sample code from Apple, Google or learnopengl.com.
  - No shaders from Shadertoy or Inigo Quilez.
  - No Dear ImGui code, and no Stack Overflow snippets.
- **Icons:** none of the ~1,500 icon paths matches Lucide (1,862 icons) or Material Design (8,491); none of the 2,202 added SVG files carries foreign metadata.
- **PDFs:** the 192 added PDFs embed only free fonts, apart from the icon specimens fixed above.
- **Documentation and tests:**
  - All 283 new documentation files carry the GNU FDL block, except 11 small snippets.
  - Screenshots and test documents contain only the fork's own content.
  - No copied prose was found.

Some components have other licenses but are compatible:
- **MuPDF** (AGPL 3) can be combined with GPL 3 code under §13 of both licenses.
- **Hunspell** is offered under MPL 1.1, GPL 2+ or LGPL 2.1+; it is used under the GPL or LGPL option.
- **Data shipped unmodified and with attribution**, as separate works:
  - CSL styles and locales (CC BY-SA 3.0);
  - pgf data (LPPL);
  - fonts (OFL, GUST);
  - emoji (CC BY 4.0).

## Claude Code and the GPL

- **Ownership of the output.** Under Anthropic's terms, the user owns what Claude produces: Anthropic assigns its rights in the output to the user. The code written with Claude Code can therefore be licensed under the GPL like any other contribution from the fork's author, and it is: every new source file carries the standard TeXmacs GPL 3-or-later header.
- **Attribution.** Commits made with Claude Code carry a `Co-Authored-By: Claude …` trailer, so the history shows which changes were made with it.
- **Copyrightability.** How far purely AI-generated material can be protected by copyright is unsettled, especially in the US. In this fork the user directs, reviews and edits the work, so most of it is not purely generated. Either way, this does not weaken the GPL for TeXmacs: the license covers the program as a whole, the human contributions, and everything derived from GPL code. At worst, some AI-written parts would be less protected; they would not be unlawful to use or distribute.
- **The real risk: reproduced third-party code.** A model can reproduce code it has seen, or port a project whose license is incompatible. The audit looked for this specifically. It found two Claude-written passages that follow permissively licensed originals closely, the inflater and the Slug shaders. Both originals are GPL-compatible, and both now carry the notices required. Nothing was taken from code under an incompatible license.
- **The process from now on.**
  - New ports name their upstream, its license and its commit in the file.
  - Third-party texts go into `TeXmacs/misc/licenses`.
  - Large new subsystems are checked against their likely upstream before merging.

## Open points and recommendations for upstream

1. **The five recreated tutorials** in `web/documents/tutorials`:
   - TeXmacs in 60 minutes (Andreas Seidl);
   - the quick-start guide, customization and graphical plug-ins (Nicolas Ratier);
   - the graphics tutorial (Henri Lesourd).

   They are derivative works of the originals hosted on texmacs.org, whose PDFs and HTML state no license. They now carry their authors' copyrights. They are web content, not part of the program. **Request:** the official TeXmacs maintainers probably know under what terms these were contributed; please confirm a license (presumably the GNU FDL, like the rest of the documentation), or ask the authors.
2. **The `FOR_EACH_PAIR` macros** in `src/Plugins/Vue/vue_widget.cpp`: about 15 lines of a generic macro idiom from a blog post that has no license. They predate the Claude sessions and are credited to the post. They are kept as they are, being too generic to raise a real question.
3. **For code taken from the fork into the official TeXmacs:**
   - **Font license files:** Fira, Libertine and TeX Gyre are shipped without license files in the official TeXmacs as well, and its `fonts/LICENSE` misstates their license. PR #506 has the fix in a form that applies to the official tree.
   - **Code adapted from others** keeps its notices and needs the matching entry in `TeXmacs/misc/licenses` if that folder is not taken along: the inflater in `zip_files.cpp`, the Slug shaders, and the tree-sitter ports.
   - **Commit history:** the `Co-Authored-By` trailers identify every change made with Claude, for anyone who wants to review those changes separately.
