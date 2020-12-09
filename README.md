
# A port of GNU TeXmacs to Guile 2.x and 3.x 


This is a port of the current [GNU TeXmacs](www.texmacs.org) to the version 2.x (and 3.x) of [GNU Guile](www.gnu.org/software/guile/). It is still unofficial and it will not be merged soon into the main repository since we want to wait the release of TeXmacs 2.1 before doing big changes. 

However, if you are curious and want to help to test it you can find it here:

https://github.com/mgubi/texmacs/tree/guile3/src

It works with Guile 1.6.8, 1.8.8, 2.2.7 and 3.0.1 out of the box. In case of Guile 2/3 you have to configure with --enable-guile2 .

Guile 2/3 are based on a VM so the scheme source code should be compiled first. In Guile 2/3 autocompilation is on by default and the results are cached somewhere in your machine, usually in `$HOME/.cache`. Compilation is a slow process, especially in Guile 2 and less so in Guile 3 so have patience. 

To turn off compilation and run in interpreted mode you should run TeXmacs with `GUILE_AUTOCOMPILE=0` set in the program's environment. One expects then Guile 2 to be slower than Guile 1.8 and Guile 3 to have same or superior speed. 

It would be nice if somebody come up with some standard test which could allow us to measure the performances of the various implementations.

I have to improve some code but I do not see any bug at the moment, so I would be glad if you discover some. :) 

Max
