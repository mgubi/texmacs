/******************************************************************************
* MODULE     : tm_configure.hpp (WebAssembly)
* DESCRIPTION: System dependent macros of the browser build
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#ifndef TM_CONFIGURE_H
#define TM_CONFIGURE_H

#define STD_SETENV
#define TEXMACS_VERSION "2.1.5"
#define TEXMACS_SOURCES "/texmacs"
#define HOST_OS "emscripten"
#define HOST_VENDOR "unknown"
#define HOST_CPU "wasm32"
#define BUILD_USER "wasm"
#define BUILD_DATE __DATE__
#define TM_DEVEL "TeXmacs-2.1.5"
#define TM_STABLE "TeXmacs-2.1.5"
#define TM_DEVEL_RELEASE "TeXmacs-2.1.5-1"
#define TM_STABLE_RELEASE "TeXmacs-2.1.5-1"

#endif // defined TM_CONFIGURE_H
