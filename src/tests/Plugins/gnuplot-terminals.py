#!/usr/bin/env python3
# The terminal of the gnuplot sessions of tmpy (plugins/tmpy/graph/gnuplot.py)
# without Ghostscript, with a fake gnuplot which has only some terminals and
# fails as gnuplot does on the others (#709). Run from src/:
#     python3 tests/Plugins/gnuplot-terminals.py
# Exits non-zero on failure. Needs a POSIX shell (not run on Windows).

import io
import os
import sys
import tempfile

here = os.path.dirname(os.path.abspath(__file__))
sys.path.insert(0, os.path.join(here, "..", "..", "plugins"))

FAKE = r"""#!/bin/sh
# a gnuplot with the terminals of $FAKE_TERMINALS
case "$1" in
  -V) echo "gnuplot 6.0 patchlevel 0 (fake)"; exit 0;;
  -e) echo "$FAKE_TERMINALS" >&2; exit 0;;
  -c) term=$(sed -n 's/^set terminal \([a-z]*\).*/\1/p' "$2")
      out=$(sed -n "s/^set output '\(.*\)'$/\1/p" "$2")
      case " $FAKE_TERMINALS " in
        *" $term "*) echo plot > "$out"; exit 0;;
        *) echo "unknown or ambiguous terminal type" >&2; exit 1;;
      esac;;
esac
exit 1
"""

failures = 0

def check(what, got, expected):
    global failures
    if got != expected:
        failures += 1
        print("FAILED %s: got %r, expected %r" % (what, got, expected))
    else:
        print("ok     %s" % what)

def session(terminals, output = None):
    """The default output, the plot file written and what the session sent"""
    os.environ["FAKE_TERMINALS"] = terminals
    from tmpy.graph import gnuplot
    gnuplot.has_ghostscript = lambda: False
    saved = sys.stdout
    sys.stdout = io.StringIO()
    try:
        g = gnuplot.Gnuplot()
        g.greet()
        g.before_evaluate()
        if output is not None:
            g.output = output
        g.evaluate("plot sin(x)")
        written = sorted(os.listdir(g.get_tmp_dir()))
        g.after_evaluate()
        sent = sys.stdout.getvalue()
    finally:
        sys.stdout = saved
    return g.default_output, written, sent

with tempfile.TemporaryDirectory() as tmp:
    bindir = os.path.join(tmp, "bin")
    os.mkdir(bindir)
    with open(os.path.join(bindir, "gnuplot"), "w") as f:
        f.write(FAKE)
    os.chmod(os.path.join(bindir, "gnuplot"), 0o755)
    os.makedirs(os.path.join(tmp, "home", "system", "tmp"))
    os.environ["PATH"] = bindir + os.pathsep + os.environ.get("PATH", "")
    os.environ["TEXMACS_HOME_PATH"] = os.path.join(tmp, "home")

    out, files, sent = session("svg pngcairo png postscript")
    check("pngcairo: output", out, "png")
    check("pngcairo: plot", files, ["gnuplot.png", "gnuplot.txt"])

    out, files, sent = session("svg png postscript")
    check("png (gd) only: output", out, "png")
    check("png (gd) only: plot", files, ["gnuplot.png", "gnuplot.txt"])
    check("png (gd) only: no error", "unknown" in sent, False)

    out, files, sent = session("svg postscript")
    check("no PNG terminal: output", out, "svg")
    check("no PNG terminal: plot", files, ["gnuplot.svg", "gnuplot.txt"])
    check("no PNG terminal: greeting", "are SVG images" in sent, True)

    out, files, sent = session("svg postscript", "png")
    check("no PNG terminal, -output png: message",
          "has no PNG terminal" in sent, True)

sys.exit(1 if failures else 0)
