
/******************************************************************************
* MODULE     : free_type.cpp
* DESCRIPTION: Interface with Free Type II
* COPYRIGHT  : (C) 2003  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "config.h"
#include "free_type.hpp"
#include "dyn_link.hpp"

#ifdef USE_FREETYPE

static bool ft_initialized= false;
static bool ft_error      = true;

FT_Library ft_library;

FT_Error (*ft_init_freetype)  (FT_Library     *alibrary);
FT_Error (*ft_new_face)       (FT_Library     library,
			       const char*    filepathname,
			       FT_Long        face_index,
			       FT_Face*       aface);
FT_Error (*ft_new_memory_face) (FT_Library library,
            const FT_Byte* file_base,
            FT_Long        file_size,
            FT_Long        face_index,
            FT_Face*       aface);
FT_Error (*ft_select_charmap) (FT_Face        face,
			       FT_Encoding    encoding);
FT_Error (*ft_set_char_size)  (FT_Face        face,
			       FT_F26Dot6     char_width,
			       FT_F26Dot6     char_height,
			       FT_UInt        horz_resolution,
			       FT_UInt        vert_resolution);
FT_UInt  (*ft_get_char_index) (FT_Face        face,
			       FT_ULong       charcode);
FT_Error (*ft_load_glyph)     (FT_Face        face,
			       FT_UInt        glyph_index,
			       FT_Int         load_flags);
FT_Error (*ft_render_glyph)   (FT_GlyphSlot   slot,
			       FT_Render_Mode render_mode);
FT_Error (*ft_get_kerning)    (FT_Face        face,
                               FT_UInt        left_glyph,
                               FT_UInt        right_glyph,
                               FT_UInt        kern_mode,
                               FT_Vector      *akerning);
FT_Error (*ft_done_face)      (FT_Face        face);
FT_Error (*ft_select_size)    (FT_Face        face,
                               FT_Int         strike_index)= NULL;
FT_Error (*ft_load_sfnt_table) (FT_Face       face,
                                FT_ULong      tag,
                                FT_Long       offset,
                                FT_Byte*      buffer,
                                FT_ULong*     length)= NULL;
#ifdef FT_HAS_COLOR_LAYERS
FT_Bool  (*ft_get_color_glyph_layer) (FT_Face           face,
                                      FT_UInt           base_glyph,
                                      FT_UInt*          aglyph_index,
                                      FT_UInt*          acolor_index,
                                      FT_LayerIterator* iterator)= NULL;
FT_Error (*ft_palette_data_get) (FT_Face          face,
                                 FT_Palette_Data* apalette)= NULL;
FT_Error (*ft_palette_select) (FT_Face        face,
                               FT_UShort      palette_index,
                               FT_Color**     apalette)= NULL;
#endif

typedef FT_Error (*glyph_renderer) (FT_GlyphSlot, FT_Render_Mode);

bool
ft_initialize () {
  if (ft_initialized) return ft_error;
  ft_initialized= true;
#ifdef LINKED_FREETYPE
  ft_init_freetype = FT_Init_FreeType;
  ft_new_face      = FT_New_Face;
  ft_new_memory_face = FT_New_Memory_Face;
  ft_select_charmap= FT_Select_Charmap;
  ft_set_char_size = FT_Set_Char_Size;
  ft_get_char_index= FT_Get_Char_Index;
  ft_load_glyph    = FT_Load_Glyph;
  ft_render_glyph  = (glyph_renderer) ((void*) FT_Render_Glyph);
  ft_get_kerning   = FT_Get_Kerning;
  ft_done_face     = FT_Done_Face;
  ft_select_size   = FT_Select_Size;
  ft_load_sfnt_table= FT_Load_Sfnt_Table;
#ifdef FT_HAS_COLOR_LAYERS
  ft_get_color_glyph_layer= FT_Get_Color_Glyph_Layer;
  ft_palette_data_get= FT_Palette_Data_Get;
  ft_palette_select= FT_Palette_Select;
#endif
  if (ft_init_freetype (&ft_library)) return true;
  if (DEBUG_AUTO) debug_automatic << "With linked TrueType support\n";
#else
  int status= debug_off ();
  (void) symbol_install ("/usr/lib/libfreetype.so", "FT_Init_FreeType" ,
			 (pointer&) ft_init_freetype);
  if (ft_init_freetype == NULL) return true;
  (void) symbol_install ("/usr/lib/libfreetype.so", "FT_New_Face"      ,
			 (pointer&) ft_new_face);
  if (ft_new_face == NULL) return true;
  (void) symbol_install ("/usr/lib/libfreetype.so", "FT_New_Memory_Face",
       (pointer&) ft_new_memory_face);
  if (ft_new_memory_face == NULL) return true;
  (void) symbol_install ("/usr/lib/libfreetype.so", "FT_Select_Charmap",
			 (pointer&) ft_select_charmap);
  if (ft_select_charmap == NULL) return true;
  (void) symbol_install ("/usr/lib/libfreetype.so", "FT_Set_Char_Size" ,
			 (pointer&) ft_set_char_size);
  if (ft_set_char_size == NULL) return true;
  (void) symbol_install ("/usr/lib/libfreetype.so", "FT_Get_Char_Index",
			 (pointer&) ft_get_char_index);
  if (ft_get_char_index == NULL) return true;
  (void) symbol_install ("/usr/lib/libfreetype.so", "FT_Load_Glyph"    ,
			 (pointer&) ft_load_glyph);
  if (ft_load_glyph == NULL) return true;
  (void) symbol_install ("/usr/lib/libfreetype.so", "FT_Render_Glyph"  ,
			 (pointer&) ft_render_glyph);
  if (ft_render_glyph == NULL) return true;
  (void) symbol_install ("/usr/lib/libfreetype.so", "FT_Get_Kerning"   ,
       (pointer&) ft_get_kerning);
  if (ft_get_kerning == NULL) return true;
  (void) symbol_install ("/usr/lib/libfreetype.so", "FT_Done_Face"     ,
       (pointer&) ft_done_face);
  if (ft_done_face == NULL) return true;
  // optional: without them no font of bitmaps, nor of the size of the
  // emoji of Apple
  (void) symbol_install ("/usr/lib/libfreetype.so", "FT_Select_Size",
       (pointer&) ft_select_size);
  (void) symbol_install ("/usr/lib/libfreetype.so", "FT_Load_Sfnt_Table",
       (pointer&) ft_load_sfnt_table);
#ifdef FT_HAS_COLOR_LAYERS
  // optional: without them the color fonts are drawn in one color
  (void) symbol_install ("/usr/lib/libfreetype.so", "FT_Get_Color_Glyph_Layer",
       (pointer&) ft_get_color_glyph_layer);
  (void) symbol_install ("/usr/lib/libfreetype.so", "FT_Palette_Data_Get",
       (pointer&) ft_palette_data_get);
  (void) symbol_install ("/usr/lib/libfreetype.so", "FT_Palette_Select",
       (pointer&) ft_palette_select);
  if (ft_palette_data_get == NULL || ft_palette_select == NULL)
    ft_get_color_glyph_layer= NULL;
#endif
  debug_on (status);
  if (ft_init_freetype (&ft_library)) return true;
  if (DEBUG_AUTO) debug_automatic << "Installed TrueType support\n";
#endif
  ft_error= false;
  return false;
}

bool
ft_present () {
  return !ft_initialize ();
}

#else

bool ft_initialize () { return true; }
bool ft_present () { return false; }

#endif
