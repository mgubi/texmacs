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
if len ([l for l in links if '#nameddest=' in l]) < 4:
    bad.append ('fewer than 4 links into the document')
# the hlink to a web page: registered by the typesetter which printed the
# document, and lost when that one was dropped before the pages were drawn
if not any ('https://www.texmacs.org' in l for l in links):
    bad.append ('no link to https://www.texmacs.org')
if 'title: Structure test' not in t: bad.append ('no title')
if any ('rror' in l for l in t): bad.append ('structure.js failed')
if any (l.startswith ('outline-') for l in t): bad.append ('an outline entry without a proper /Parent')
if any (l.startswith ('warning') for l in t): bad.append ('MuPDF repairs the file: ' + [l for l in t if l.startswith ('warning')][0])
print ('; '.join (bad) if bad else 'ok')
