# CSL styles and locales

The files in `styles` and `locales` come from the repositories of the
Citation Style Language project,

- <https://github.com/citation-style-language/styles>
- <https://github.com/citation-style-language/locales>

and are distributed under the Creative Commons Attribution-ShareAlike 3.0
Unported license, <https://creativecommons.org/licenses/by-sa/3.0/>. Each
style names its authors in its `<info>` element. They are data read by the
CSL processor of TeXmacs (`progs/csl`), not part of the program.

More styles can be put in `$TEXMACS_HOME_PATH/csl/styles`, or next to a
document: a bibliography whose style is `csl-NAME` uses `NAME.csl`.
