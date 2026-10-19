#!/usr/bin/env python3
# makes math-cursor.script (see math-cursor.scm): for each font, Right
# through the formula, then a click at each step
import os
L = ["# made by math-cursor.py: see math-cursor.scm",
     "wait 8000", "scheme (c5-setup)", "wait 2500"]
for i in range(2):
    L += ["scheme (c5-font %d)" % i, "wait 1500", "scheme (c5-record)", "wait 200"]
    for k in range(26):
        L += ["key Right", "wait 120", "scheme (c5-record)", "wait 120"]
    for j in range(26):
        L += ["scheme (c5-click-press %d)" % j, "wait 150",
              "scheme (c5-click-check %d)" % j, "wait 80"]
    L += ["scheme (c5-font-done %d)" % i, "wait 300"]
L += ["scheme (c5-end)", "wait 800", "snapshot math-cursor"]
here = os.path.dirname(os.path.abspath(__file__))
with open(os.path.join(here, "math-cursor.script"), "w") as f:
    f.write("\n".join(L) + "\n")
