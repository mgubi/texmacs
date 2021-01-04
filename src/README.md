# GNU TeXmacs (experimental branch for S7 Scheme)

January 2021.

This is *experimental code*. Use at your own risk. In particular do not assume that ./configure do the right job, it might not.
This version of TeXmacs uses [S7 scheme](https://cm-gitlab.stanford.edu/bil/s7.git) instead of Guile. Currenlty seems to run fine, however it still has to be considered not ready for production. The main changes are in the `src/Scheme` and in the `TeXmacs/progs` directories. In the future I will have scheme code which works with both Guile and S7.

At this point performance is good. TeXmacs/S7 compiles the full user manual in 20 sec while TeXmacs/Guile-1.8 in 15 sec. However there should be some margin for improvement. S7 is much faster than Guile 1.8 in standard benchmarks. 

For the standard r7rs benchmarks [here](https://github.com/ecraven/r7rs-benchmarks) I got the following results on my MacBook Air (2019). 

| test    | S7      | Guile1.8.8  | Guile3.0.4    |
|---------|---------|---------|-----------|
| browse  | 24.27   | 80.32   | 12.060597 |
| deriv   | 25.194  | 61.39   | 18.581995 |
| destruc | 52.077  | TIMELIM | 7.143701  |
| diviter | 9.685   | 77.85   | 15.453743 |
| divrec  | 11.803  | 78.55   | 17.41294  |
| puzzle  | 27.716  | 191.35  | 18.086531 |
| triangl | 33.931  | 98.16   | 8.519252  |
| tak     | 12.925  | 134.2   | 4.757643  |
| takl    | 20.968  | TIMELIM | 9.456034  |
| ntakl   | 17.073  | TIMELIM | 9.516082  |
| cpstak  | 103.358 | 221.03  | 59.444873 |
| ctak    | 44.139  | TIMELIM | TIMELIM   |
| fib     | 10.218  | 195.78  | 12.090909 |
| fibc    | 25.799  | TIMELIM | TIMELIM   |
| fibfp   | 1.885   | 45.98   | 22.001634 |
| sum     | 6.637   | 281.63  | 6.866215  |
| sumfp   | 2.499   | 105.1   | 42.058511 |
| fft     | 32.198  | TIMELIM | 7.685201  |
| mbrot   | 24.403  | TIMELIM | 50.086067 |
| mbrotZ  | 18.556  | TIMELIM | 67.011491 |
| nucleic | 19.946  | 67.46   | 15.347245 |
| pi      | NO      | TIMELIM | 0.564552  |
| pnpoly  | 17.981  | TIMELIM | 24.886723 |
| ray     | 20.455  | TIMELIM | 18.51229  |
| simplex | 46.344  | TIMELIM | 13.895531 |
| ack     | 10.572  | TIMELIM | 8.413945  |
| array1  | 11.483  | 160.88  | 9.241778  |
| string  | 1.714   | 1.82    | 1.872806  |
| sum1    | 0.47    | 1.63    | 4.427402  |
| cat     | 1.187   | TIMELIM | 28.396944 |
| tail    | 1.188   | TIMELIM | 9.821691  |
| wc      | 8.266   | 57.91   | 16.963138 |
| read1   | 406     | 0.95    | 5.804979  |
| compiler | 41.155  | TIMELIM | 5.149011  |
| conform | 51.031  | TIMELIM | 10.508732 |
| dynamic | 22.736  | 69.58   | 7.374259  |
| earley  | TIMELIM | TIMELIM | 9.489885  |
| graphs  | 127.611 | TIMELIM | 23.026826 |
| lattice | 139.275 | 292.7   | 15.937364 |
| matrix  | 72.073  | TIMELIM | 9.881781  |
| maze    | 23.258  | TIMELIM | 4.70391   |
| mazefun | 19.51   | 129.61  | 9.664338  |
| nqueens | 55.11   | TIMELIM | 19.372148 |
| paraffins | 31.424  | TIMELIM | 4.24542   |
| parsing | 39.443  | TIMELIM | 10.687959 |
| peval   | 29.677  | 98.91   | 15.644764 |
| primes  | 7.73    | 39.33   | 7.521318  |
| quicksort | 93.996  | TIMELIM | 13.252736 |
| scheme  | 71.462  | TIMELIM | 15.142413 |
| slatex  | 32.069  | 48.96   | 45.047143 |
| chudnovski | NO      | TIMELIM | 0.306648  |
| nboyer  | 39.274  | 151.42  | 5.10214   |
| sboyer  | 31.537  | 168.81  | 4.755798  |
| gcbench | 20.54   | TIMELIM | 3.511493  |
| mperm   | 173.33  | TIMELIM | 10.650118 |
| equal   | 781     | TIMELIM | TIMELIM   |
| bv2string | 10.782  | TIMELIM | 4.489627  |
|  | | | |




Below follows the standard README.md for TeXmacs

-----------------------
# GNU TeXmacs
[![Join the chat at https://gitter.im/texmacs/Lobby](https://badges.gitter.im/texmacs/Lobby.svg)](https://gitter.im/texmacs/Lobby?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge&utm_content=badge)

[GNU TeXmacs](https://texmacs.org) is a free wysiwyw (what you see is what you want) editing platform with special features for scientists. The software aims to provide a unified and user friendly framework for editing structured documents with different types of content (text, graphics, mathematics, interactive content, etc.). The rendering engine uses high-quality typesetting algorithms so as to produce professionally looking documents, which can either be printed out or presented from a laptop.

The software includes a text editor with support for mathematical formulas, a small technical picture editor and a tool for making presentations from a laptop. Moreover, TeXmacs can be used as an interface for many external systems for computer algebra, numerical analysis, statistics, etc. New presentation styles can be written by the user and new features can be added to the editor using the Scheme extension language. A native spreadsheet and tools for collaborative authoring are planned for later.

TeXmacs runs on all major Unix platforms and Windows. Documents can be saved in TeXmacs, Xml or Scheme format and printed as Postscript or Pdf files. Converters exist for TeX/LaTeX and Html/Mathml. 

## Documentation
GNU TeXmacs is self-documented. You may browse the manual in the `Help` menu or browse the online [one](https://www.texmacs.org/tmweb/manual/web-manual.en.html).

For developer, see [this](./COMPILE) to compile the project.

## Contributing
Please report any [new bugs](https://www.texmacs.org/tmweb/contact/bugs.en.html) and [suggestions](https://www.texmacs.org/tmweb/contact/wishes.en.html) to us. It is also possible to [subscribe](https://www.texmacs.org/tmweb/help/tmusers.en.html) to the <texmacs-users@texmacs.org> mailing list in order to get or give help from or to other TeXmacs users.

You may contribute patches for TeXmacs using the [patch manager](http://savannah.gnu.org/patch/?group=texmacs) on Savannah or using the [pull request](https://github.com/texmacs/texmacs/pulls) on Github. Since we are using SVN on Savannah, PRs won't be directly accepted on Github. We will `git apply` the patch into SVN repo if the PR is accepted. And we will close the PR and change the title to `[SVN] xxx` after applying the PR.
