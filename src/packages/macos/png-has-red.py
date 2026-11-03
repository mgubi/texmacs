#!/usr/bin/env python3
#
# Exits with 0 if one of the PNG files given (8 bits per channel, RGB or
# RGBA, not interlaced, as saved by TEXMACS_NS_SNAPSHOT) has many red
# pixels: the CI checks that the colors of a document (ci-colors.tm, big red
# text) reach the screen. A window without it has less than 200 (the red
# of the flag in the tool bar, the cursor).
#
# Usage: packages/macos/png-has-red.py FILE.png...

import struct, sys, zlib

def pixels (path):
  data= open (path, "rb").read ()
  assert data[:8] == b"\x89PNG\r\n\x1a\n", path
  pos, idat= 8, b""
  while pos < len (data):
    n, kind= struct.unpack (">I4s", data[pos:pos+8])
    chunk= data[pos+8:pos+8+n]
    if kind == b"IHDR":
      w, h, depth, ctype, _, _, inter= struct.unpack (">IIBBBBB", chunk)
      assert depth == 8 and ctype in (2, 6) and inter == 0, path
      bpp= 3 if ctype == 2 else 4
    elif kind == b"IDAT": idat += chunk
    pos += 12 + n
  raw= zlib.decompress (idat)
  stride= w * bpp
  prev= bytearray (stride)
  for y in range (h):
    f= raw[y * (stride + 1)]
    row= bytearray (raw[y * (stride + 1) + 1:(y + 1) * (stride + 1)])
    for i in range (stride):
      a= row[i - bpp] if i >= bpp else 0
      b= prev[i]
      c= prev[i - bpp] if i >= bpp else 0
      if f == 1: row[i]= (row[i] + a) & 255
      elif f == 2: row[i]= (row[i] + b) & 255
      elif f == 3: row[i]= (row[i] + (a + b) // 2) & 255
      elif f == 4:
        p= a + b - c
        pa, pb, pc= abs (p - a), abs (p - b), abs (p - c)
        pr= a if pa <= pb and pa <= pc else (b if pb <= pc else c)
        row[i]= (row[i] + pr) & 255
    yield row, bpp
    prev= row

def has_red (path):
  # NOTE: a file being written (the Vue port saves its windows at each
  # redraw) is not a PNG yet: no red in it, for now
  try:
    return has_red_in (path)
  except Exception:
    return False

def has_red_in (path):
  count= 0
  for row, bpp in pixels (path):
    for i in range (0, len (row), bpp):
      r, g, b= row[i], row[i+1], row[i+2]
      if r > 180 and g < 90 and b < 90:
        count += 1
        if count >= 2000: return True
  return False

if __name__ == "__main__":
  found= [f for f in sys.argv[1:] if has_red (f)]
  print ("red pixels in: " + (", ".join (found) if found else "none"))
  sys.exit (0 if found else 1)
