# Bug report for MuPDF: subsetting a CFF font fails on hintmasks after stems declared in subroutines

*For https://bugs.ghostscript.com (product MuPDF, component fitz): the
GitHub repository of MuPDF is a mirror, whose pull requests are not read.
The patch to attach is
`0001-Subset-CFF-fonts-whose-subroutines-declare-stem-hint.patch` (next to
this file; `git format-patch` against master d587982f971e, `git am`
applies it); `misc/wasm/mupdf-subset-cff.patch` is the same change, which
the browser build applies to 1.28.5.*

---

**Summary:** `pdf_subset_fonts` fails with "Reserved charstring byte
c=0x0" on CFF fonts whose stem hints are declared in subroutines (Fira
Sans), and then no font of the document is subset.

**Version:** MuPDF 1.28.5 (built from the source, and the `mutool`
1.28.5 of Homebrew, macOS arm64), and master at d587982f971e (2026-09-24,
`mutool` 1.29.0): `execute_charstring` and `scan_charstrings` are the same
there, save for the renaming of the usage lists (`fz_list`), and the
reproduction below fails the same way.

**Font:** Fira Sans Bold, `FiraSans-Bold.otf`, "Version 4.203;PS
004.203;hotconv 1.0.88;makeotf.lib2.5.64775" (SIL OFL,
https://github.com/mozilla/Fira), SHA-256
`965ee245a8a4e25d52802b0f07c507613969da0b39dcc882b58f10b907243540`.
FiraSans-Regular.otf fails the same way.

### Steps to reproduce

`repro.js`:

```js
// mutool run repro.js FiraSans-Bold.otf out.pdf m
var font = new Font("FiraSans-Bold", scriptArgs[0]);
var doc = new PDFDocument();
var f = doc.addFont(font);                  // Type0, Identity-H
var gids = "";
(scriptArgs[2] || "Hello").split("").forEach(function (ch) {
  var g = font.encodeCharacter(ch.charCodeAt(0));
  gids += ("0000" + g.toString(16)).slice(-4);
});
var page = doc.addPage([0, 0, 300, 100], 0, { Font: { F1: f } },
                       "BT /F1 24 Tf 20 40 Td <" + gids + "> Tj ET");
doc.insertPage(-1, page);
doc.subsetFonts();
doc.save(scriptArgs[1], "compress");
```

```
$ mutool run repro.js FiraSans-Bold.otf out.pdf m
format error: Reserved charstring byte c=0x0
```

`out.pdf` has the whole font (224 KB for the printable ASCII, instead of
55 KB subset). With the text "Hello" it is subset correctly; the single letters
`3 Q i j m n p q t` each fail.

### Expected

The font is subset, as with "Hello".

### Analysis

The charstring of `m` (glyph 541 in this font), decompiled with fontTools:

```
261 21 255 callgsubr  126 546 callsubr  hintmask <00>  hintmask <dc>
649 548 164 callsubr
  gsubr 255: -21 432 272 callgsubr return
  subr 546:  158 126 158 return
  subr 164:  252 callgsubr hintmask <bc> 717 callgsubr hintmask <dc> ...
```

The stem hints of `m` are declared by the subroutines it calls: the
operands they leave on the stack before the first `hintmask` are its
(implicit) stems. `execute_charstring` in `subset-cff.c` counts none of
them:

1. A subroutine is not executed within the charstring which calls it:
   `callsubr` and `callgsubr` only mark it as used, and it is scanned later
   by `scan_charstrings`, on its own, with an empty stack and
   `stem_hints = 0`.
2. `callgsubr` (29) and `callsubr` (10) set `start = 2`, since they are
   operators below 32 other than the hint ones, and the operands on the
   stack at a `hintmask` are counted only when `start == 1`.

So the first `hintmask` of `m` is taken to have 0 bytes of mask, and its
mask byte `0x00` is read as an operator: "Reserved charstring byte c=0x0".
The same happens when subroutine 164, which has `hintmask` itself, is
scanned on its own. The exception ends `pdf_subset_fonts` for the whole
document: the other fonts are not subset either.

Hundreds of the subroutines of Fira Sans hold stem hints or hintmasks (in
FiraSans-Bold.otf, 153 of the 852 local and 131 of the 853 global
subroutines declare stems; 112 and 94 hold a hintmask), as usual in fonts
subroutinized by makeotf/tx.

### Fix

Execute the subroutines within the charstrings which call them, as a
renderer does, with the stack, the stem count and the state of the caller:

- the state of `execute_charstring` (stack, `sp`, `stem_hints`, `start`,
  transient array) goes in a struct shared by the calls; `callsubr` and
  `callgsubr` mark the subroutine as used and run it in that state (the
  local index of a CID font is the one of the FD of the glyph), with a
  depth limit of 10; `return` leaves the stack to the caller; `endchar`
  ends the glyph, also from within a subroutine;
- at `hintmask`/`cntrmask` the operands on the stack are counted as
  implicit vstems (as the Type 2 charstring format says of `hintmask`),
  whatever came before; the calls and returns of subroutines, and `vstem`/`vstemhm`, no
  longer end the hint declarations;
- the loops of `scan_charstrings` over the used subroutines no longer
  execute them on their own (they are executed within their callers, and
  alone they would still fail).

The patch attached, made against 1.28.5, applies to master as it is. With
it, on 1.28.5 and on master alike, the reproduction above subsets the font
with no error: 45 KB for `m` and 55 KB for the printable ASCII of Fira Sans
Bold instead of 224 KB, 54 KB instead of 215 KB for Fira Sans Regular, and
`mutool draw` renders the subsets correctly. So do
the documents of GNU TeXmacs set in Fira: the PDF of its Welcome document
went from 740 KB, all fonts whole, to 566 KB, all fonts subset.
