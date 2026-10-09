# Font samples

The pictures of the page of the design of the fonts (`Font design` in the
menu of the fonts, `TeXmacs/progs/fonts/font-design.scm`): one for each
font of that page which comes with TeXmacs, named by the kind and the name
of its entry. They are pictures so that the page does not load the fonts.

They are made by TeXmacs itself and have to be made again when a font
is added, removed or updated, or when the sample changes:

    texmacs -x '(begin (use-modules (fonts font-design)) (font-design-make-samples "$TEXMACS_PATH/misc/font-samples") (exit 0))' -q

The samples of the other fonts of a system are made on request, in
`$TEXMACS_HOME_PATH/system/cache/font-samples`.
