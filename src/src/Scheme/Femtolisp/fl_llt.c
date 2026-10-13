/******************************************************************************
* MODULE     : fl_llt.c
* DESCRIPTION: the library llt of femtolisp (aggregate compilation)
* COPYRIGHT  : (C) 2026  Massimiliano Gubinelli
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#ifndef NDEBUG
#define NDEBUG
#endif

#include "femtolisp/llt/bitvector.c"
#include "femtolisp/llt/hashing.c"
#include "femtolisp/llt/socket.c"
#include "femtolisp/llt/timefuncs.c"
#include "femtolisp/llt/ptrhash.c"
#include "femtolisp/llt/utf8.c"
#include "femtolisp/llt/ios.c"
#include "femtolisp/llt/dirpath.c"
#include "femtolisp/llt/htable.c"
#include "femtolisp/llt/bitvector-ops.c"
#include "femtolisp/llt/int2str.c"
#include "femtolisp/llt/dump.c"
#include "femtolisp/llt/random.c"
#include "femtolisp/llt/lltinit.c"
