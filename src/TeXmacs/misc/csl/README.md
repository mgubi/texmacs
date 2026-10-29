# CSL styles and locales

The files in `styles` and `locales` come from the repositories of the
Citation Style Language project,

- <https://github.com/citation-style-language/styles>
- <https://github.com/citation-style-language/locales>

They were taken from the branch `master` of these repositories and added
to TeXmacs on 9 October 2026; the revisions were not recorded (the latest
`<updated>` date among the files is 2026-09-10). They are distributed under the Creative Commons Attribution-ShareAlike 3.0
Unported license, <https://creativecommons.org/licenses/by-sa/3.0/>. Each
style names its authors in its `<info>` element. They are data read by the
CSL processor of TeXmacs (`progs/csl`), not part of the program.

More styles can be put in `$TEXMACS_HOME_PATH/csl/styles`, or next to a
document: a bibliography whose style is `csl-NAME` uses `NAME.csl`.
