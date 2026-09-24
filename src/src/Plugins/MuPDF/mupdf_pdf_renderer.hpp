
/******************************************************************************
* MODULE     : mupdf_pdf_renderer.hpp
* DESCRIPTION: Renderer which writes a PDF document using MuPDF
* COPYRIGHT  : (C) 2026  Massimiliano Gubinelli
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#ifndef MUPDF_PDF_RENDERER_H
#define MUPDF_PDF_RENDERER_H

#include "renderer.hpp"
#include "url.hpp"

// A printer renderer which produces a PDF file through MuPDF, as an
// alternative to pdf_hummus_renderer. Prototype: see the notes in
// docs/pdf-output-with-mupdf.md for what it does and what it does not.
renderer mupdf_pdf_renderer (url pdf_file_name, int dpi, int nr_pages= 1,
                             string page_type= "a4", bool landscape= false,
                             double paper_w= 21.0, double paper_h= 29.7);

#endif // MUPDF_PDF_RENDERER_H
