#!/usr/bin/env python3
###############################################################################
##
## MODULE      : gnuplot.py
## DESCRIPTION : Gnuplot plotting support
## COPYRIGHT   : (C) 2019  Darcy Shen
##
## This software falls under the GNU general public license version 3 or later.
## It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
## in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.

import os
from glob       import glob
from subprocess import Popen, PIPE, STDOUT
from .graph     import Graph
from ..protocol import *
from ..compat   import *

def has_ghostscript():
    """Whether TeXmacs can show EPS images: it converts them with
    Ghostscript (macOS no longer renders PostScript). The same places as
    gs_executable in Plugins/Ghostscript/gs_utilities.cpp"""
    if which("gs") is not None or which("gs.exe") is not None:
        return True
    # a copy in the package of TeXmacs (GS_EXE)
    tm = os.environ.get("TEXMACS_PATH", "")
    for gs in ["gs", "gs.exe"]:
        if tm != "" and os.path.exists(os.path.join(tm, "bin", gs)):
            return True
    # the usual installation on Windows
    return len(glob("C:\\Program Files*\\gs\\gs*\\bin\\gswin*c.exe")) > 0

def gnuplot_terminals(name):
    """The terminals of this gnuplot (GPVAL_TERMINALS, which print writes
    to stderr), or None when they cannot be read"""
    try:
        p = Popen([name, "-e", "print GPVAL_TERMINALS"],
                  stdin=PIPE, stdout=PIPE, stderr=PIPE)
        out, err = p.communicate()
    except OSError:
        return None
    terms = (out + err).decode("utf-8", "replace").split()
    if p.returncode != 0 or len(terms) == 0:
        return None
    return terms

# PNG with cairo, else with libgd: gnuplot can be built without either (#709)
PNG_TERMINALS = [
    ("pngcairo", "set terminal pngcairo enhanced size 1280,960 "
                 "fontscale 2 linewidth 2"),
    ("png",      "set terminal png enhanced size 1280,960 "
                 "fontscale 2 linewidth 2")]

class Gnuplot(Graph):
    def __init__(self, name = "gnuplot"):
        super(Gnuplot, self).__init__()
        self.name = name
        # EPS (vector) plots need Ghostscript in TeXmacs, else a plot
        # would show as "?": PNG then (#662)
        self.has_gs = has_ghostscript()
        self.terminals = gnuplot_terminals(name)
        self.png_terminal = self.find_png_terminal()
        if self.has_gs:
            self.default_output = "eps"
        elif self.png_terminal is not None or not self.has_terminal("svg"):
            self.default_output = "png"
        else:
            # no PNG terminal: SVG, which gnuplot always has (#709)
            self.default_output = "svg"
        self.default_width = "0.8par"

    def has_terminal(self, term):
        # when the list cannot be read, trust that the terminal is there
        return self.terminals is None or term in self.terminals

    def find_png_terminal(self):
        for term, line in PNG_TERMINALS:
            if self.has_terminal(term):
                return line
        return None

    def greet(self):
        if len(self.message) == 0:
            try:
                p = Popen([self.name, "-V"], stdout=PIPE)
                ret, err = p.communicate()
                # WARN: The Version Info is in stderr
                if (p.returncode == 0):
                    self.message = ret.decode()
            except OSError:
                pass
            if not self.has_gs:
                kind = self.default_output.upper()
                self.message += ("Ghostscript (gs) was not found: the plots "
                                 "are " + kind + " images. Install "
                                 "Ghostscript for vector (EPS) plots.\n")
        super(Gnuplot, self).greet()

    def evaluate(self, code):
        if self.output == "eps":
            image = self.get_eps()
            self.pre_code = """
reset
set terminal postscript eps enhanced
set output 
set output '%s'
set size 1,1
set autoscale
""" % (self.get_eps_path())
        elif self.output == "png" and self.png_terminal is None:
            flush_verbatim("This gnuplot has no PNG terminal (pngcairo or "
                           "png): use the output eps or svg.")
            return
        elif self.output == "png":
            image = self.get_png()
            self.pre_code = """
reset
%s
set output 
set output '%s'
set size 1,1
""" % (self.png_terminal, self.get_png_path())
        elif self.output == "svg":
            image = self.get_svg()
            self.pre_code = """
reset
set terminal svg enhanced
set output 
set output '%s'
set size 1,1
set autoscale
""" % (self.get_svg_path())
        else:
            flush_verbatim("Unsupported output type: " + self.output)
            return

        code_path = self.get_tmp_dir() + self.name + ".txt"
        with open(code_path, 'w') as code_file:
            code_file.write(self.pre_code)
            code_file.write(code)

        cmd = [self.name, "-c", code_path]
        p = Popen(cmd, stderr=PIPE)
        out, err = p.communicate()
        if (p.returncode == 0):
          flush_file (image)
        else:
          flush_verbatim (err.decode())
