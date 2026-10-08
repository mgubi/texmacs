
/******************************************************************************
* MODULE     : zip_files.hpp
* DESCRIPTION: reading and writing zip archives
* COPYRIGHT  : (C) 2026  Massimiliano Gubinelli
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#ifndef ZIP_FILES_H
#define ZIP_FILES_H
#include "string.hpp"
#include "array.hpp"

// The archives of the office formats (.docx, .odt) are zip files of a few
// XML files and images. This is all the zip which they need, with no
// library: the entries are read when they are stored or deflated, and they
// are written stored. No encryption, no zip64, no archives in several parts.

// an archive as a string
bool          zip_is_archive (string zip);
array<string> zip_entries (string zip);
bool          zip_read (string zip, string name, string& data);
string        zip_write (array<string> names, array<string> datas);

// all the entries at once, for the glue: name, data, name, data...
array<string> zip_unpack (string zip);

// the two halves on their own
bool          inflate_string (string in, int start, int size, int out_size,
                              string& out);
unsigned int  crc32_string (string s);

#endif // defined ZIP_FILES_H
