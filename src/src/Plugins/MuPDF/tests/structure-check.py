# What structure.js says of the PDF of structure.tm, checked (pdf-compare.sh):
# prints "ok", or what is missing.
import sys
t= open (sys.argv[1]).read ().splitlines ()
out= [l for l in t if l.lstrip ().startswith ('outline:')]
deep= [l for l in out if l.startswith ('    outline:')]
dests= [l for l in t if l.startswith ('dest: #')]
links= [l for l in t if l.startswith ('link ')]
bad= []
if len (out) != 7: bad.append ('%d outline entries, not 7' % len (out))
if len (deep) != 1: bad.append ('no third level in the outline')
for n in ['#sec-first', '#sec-deep', '#sec-second', '#on-page-two']:
    if not any (l.startswith ('dest: ' + n + ' ') for l in dests):
        bad.append ('no destination ' + n)
if not any (l.startswith ('dest: #on-page-two -> page 1 ') for l in dests):
    bad.append ('#on-page-two not on the second page')
if len (links) < 4: bad.append ('%d links, not 4' % len (links))
if 'title: Structure test' not in t: bad.append ('no title')
if any ('rror' in l for l in t): bad.append ('structure.js failed')
print ('; '.join (bad) if bad else 'ok')
