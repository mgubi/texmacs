/******************************************************************************
* MODULE     : ts_captures.hpp
* DESCRIPTION: the nodes captured by a query, with the properties of #set!
* COPYRIGHT  : (C) 2026  Massimiliano Gubinelli
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#ifndef TS_CAPTURES_H
#define TS_CAPTURES_H
#include <map>
#include <string>
#include <unordered_map>

// the properties of a pattern (#set! key value)
typedef std::map<std::string,std::string> ts_properties;
// capture name -> node id (TSNode.id) -> properties
typedef std::unordered_map<std::string,
                           std::unordered_map<const void*, ts_properties> >
        ts_node_captures;

#endif // TS_CAPTURES_H
