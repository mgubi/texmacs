# The latency of the wheel on macOS (issue #988), from the standard error of
# texmacs.bin run with TEXMACS_NS_DEBUG_DRAW=1 (Cocoa) or
# TEXMACS_VUE_DEBUG_PRESENT=1 (Vue), either way of presenting: for each
# WHEEL line, the first frame presented after it (SURFACE, or PRESENT with
# TEXMACS_*_PRESENT=metal) and the first SHOWN line after that frame.
# usage: python3 latency.py <log>
import sys, re, statistics
ev = None; to_present = []; to_shown = []
for line in open(sys.argv[1]):
    m = re.match(r'WHEEL event ([\d.]+)', line)
    if m:
        if ev is None: ev = [float(m.group(1)), None]
        continue
    m = re.match(r'(?:SURFACE ([\d.]+) wait|PRESENT ([\d.]+) (?:dt|with))', line)
    if m and ev and ev[1] is None:
        ev[1] = float(m.group(1) or m.group(2)); to_present.append(ev[1] - ev[0]); continue
    m = re.match(r'SHOWN ([\d.]+)', line)
    if m and ev and ev[1] is not None and '(' not in line:
        to_shown.append(float(m.group(1)) - ev[0]); ev = None
def show(name, v):
    if not v: print(f"{name}: none"); return
    s = sorted(v)
    print(f"{name}: n={len(v)} median {statistics.median(v):.1f} ms, "
          f"p90 {s[min(len(s) - 1, int(.9 * len(s)))]:.1f} ms, worst {s[-1]:.1f} ms")
show("event -> presented", to_present); show("event -> shown", to_shown)
