# usage: python3 -I pnmdiff.py a.pnm b.pnm
# Prints how many pixels of two images of the same size differ, and the box
# around them (mutool draw writes P5 or P6 images).
import re, sys

def read(p):
    d = open(p, 'rb').read()
    m = re.match(rb'(P[56])\s+(\d+)\s+(\d+)\s+(\d+)\s', d)
    c = 1 if m.group(1) == b'P5' else 3
    return int(m.group(2)), int(m.group(3)), c, d[m.end():]

w, h, c, a = read(sys.argv[1])
_, _, _, b = read(sys.argv[2])
x0 = y0 = 10**9; x1 = y1 = -1; n = 0
for y in range(h):
    ra = a[y*w*c:(y+1)*w*c]; rb = b[y*w*c:(y+1)*w*c]
    if ra == rb: continue
    for x in range(w):
        if ra[x*c:(x+1)*c] != rb[x*c:(x+1)*c]:
            n += 1; x0 = min(x0, x); x1 = max(x1, x); y0 = min(y0, y); y1 = max(y1, y)
print(f"{n} pixels differ" + (f", box x {x0}-{x1}, y {y0}-{y1} of {w}x{h}" if n else ""))
