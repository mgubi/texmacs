# 7. Performance

## 7.1 Setup

- **When:** 2026-11-21 and 22, macOS on Apple silicon, Qt offscreen. Other
  sessions were running (load average 2.5 to 5): the builds were alternated,
  and the comparisons below are between runs made at the same load.
- **Builds**, all of the same Scheme code (the `TeXmacs` folder of this
  branch), each with its own home:
  - Chez: this branch, with `texmacs.boot`;
  - s7 and Guile 1.8: this branch, configured `--with-scheme=s7` and
    `--with-scheme=guile`;
  - femtolisp: this branch, configured `--with-scheme=femtolisp`.

## 7.2 Summary

| Workload | Chez | s7 | femtolisp | Guile 1.8 |
|---|---:|---:|---:|---:|
| Start-up, `-x '(quit-TeXmacs)'` | 0.69 s | 0.88 s | 1.0 s | – |
| First LaTeX export of the change log (cold) | 1.33 s | 1.72 s | 2.21 s * | – |
| One warm LaTeX export of the change log | 80 ms | 300 ms | 181 ms | – |
| All the regression suites | 64 s | 72 s ** | 68 s, 75 s * | 107 s *** |

\* femtolisp with its cache of compiled files empty (a new home): it compiles
each module when it loads it; with the cache filled, 68 s.
\*\* s7 runs two more suites (45).
\*\*\* measured at a higher load, not repeated.

What this shows:

- **The Scheme code runs several times faster on Chez.** A warm LaTeX export
  is 3.7 times faster than on s7; in an earlier profile of the same export,
  the time spent in Scheme was about 24 ms on Chez against 189 ms on s7. The
  rest is C++ (typesetting, conversion), the same for every Scheme.
- **Start-up is faster** than on s7: the boot image is loaded into the heap
  at once, where the other Schemes read or load their modules.
- **The test suites differ little** (about 15%): most of their time is C++.

## 7.3 How

- **Start-up:** `/usr/bin/time` of `texmacs.bin -x '(quit-TeXmacs)'`, five
  rounds, fonts cached.
- **LaTeX export:** a script which exports
  `doc/about/changes/change-log.en.tm` to LaTeX once (cold), then 8 times
  (warm), timed with `texmacs-time`; three rounds per build. The outputs of
  Chez and s7 differ only in the order of the `\usepackage` list.
- **Suites:** `(run-all-tests)` from the command line, timed with
  `/usr/bin/time`.
- Every run was watched for memory (the peaks: about 2 GB for the suites on
  every Scheme, 250 MB for start-up).

## 7.4 Why the gain is smaller than on Scheme benchmarks

On pure Scheme benchmarks (such as the
[R7RS benchmarks](https://ecraven.github.io/r7rs-benchmarks/)) a native
compiler like Chez is far ahead of interpreters. TeXmacs gains less for two
reasons: most of its time is in C++, and its Scheme code crosses into C++
often, each glue call converting its strings (the glue costs about 2% of a
LaTeX export). The Scheme part itself is about 8 times faster than on s7.
