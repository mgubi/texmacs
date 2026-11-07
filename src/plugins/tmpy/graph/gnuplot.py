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

class Gnuplot(Graph):
    def __init__(self, name = "gnuplot"):
        super(Gnuplot, self).__init__()
        self.name = name
        # EPS (vector) plots need Ghostscript in TeXmacs, else a plot
        # would show as "?": PNG then (#662)
        self.has_gs = has_ghostscript()
        self.default_output = "eps" if self.has_gs else "png"
        self.default_width = "0.8par"

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
                self.message += ("Ghostscript (gs) was not found: the plots "
                                 "are PNG images. Install Ghostscript for "
                                 "vector (EPS) plots.\n")
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
        elif self.output == "png":
            image = self.get_png()
            self.pre_code = """
reset
set terminal pngcairo enhanced size 1280,960 fontscale 2 linewidth 2
set output 
set output '%s'
set size 1,1
""" % (self.get_png_path())
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
