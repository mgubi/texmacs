/******************************************************************************
 * MODULE     : pdf_hummus_make_attachment.hpp
 * DESCRIPTION: Interface for embedding text files into pdf files
 * COPYRIGHT  : (C) 2023 Tangdouer
 *******************************************************************************
 * This software falls under the GNU general public license version 3 or later.
 * It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
 * in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
 ******************************************************************************/

#ifndef PDF_HUMMUS_MAKE_ATTACHMENT_H
#define PDF_HUMMUS_MAKE_ATTACHMENT_H

#include "hashmap.hpp"
#include "iterator.hpp"
#include "list.hpp"
#include "string.hpp"
#include "tm_ostream.hpp"
#include "url.hpp"

#ifdef PDF_RENDERER
/**

Embed attachments into pdf in Embedded File Streams format
@param pdf_path The path of the PDF file where attachments need to be embedded.
@param attachment_path A list that specifies the paths of attachments to be
embedded.
@param out_path The path of the new PDF file with the embedded attachments.
@return Returns true if the embedding is successful, false otherwise.
*/
bool pdf_hummus_make_attachments (url pdf_path, array<url> attachment_path,
                                  url out_path);
#else
/*
 * when the pdf plugin is not enabled, you can still include the pdf headers files.
 * in that case the pdf functions will alaways return an error.
 */
#ifdef MUPDF_RENDERER
// MuPDF can do it too, and does when the Hummus renderer is not built in
bool mupdf_pdf_make_attachments (url pdf_path, array<url> attachments,
                                 url out_path);
inline bool pdf_hummus_make_attachments (url pdf_path, array<url> attachment_path,
                                         url out_path) {
  return mupdf_pdf_make_attachments (pdf_path, attachment_path, out_path);
}
#else
inline bool pdf_hummus_make_attachments (url pdf_path, array<url> attachment_path,
                                         url out_path) {
  (void) pdf_path; (void) attachment_path; (void) out_path;
  return false;
}
#endif
#endif

#endif // ifdef PDF_HUMMUS_MAKE_ATTACHMENT_H
