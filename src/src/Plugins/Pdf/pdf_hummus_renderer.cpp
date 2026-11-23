
/******************************************************************************
* MODULE     : pdf_hummus_renderer.cpp
* DESCRIPTION: Renderer for printing pdf graphics using the PDFHummus library
* COPYRIGHT  : (C) 2012 Massimiliano Gubinelli
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "pdf_hummus_renderer.hpp"
#include "pdf_tags.hpp"

extern bool hummus_write_cidset;  // PDFWriter/DescendentFontWriter.cpp
extern int hummus_missing_objects;  // PDFWriter/PDFDocumentHandler.cpp
// the PDF pictures get their fonts embedded (a tagged PDF, PDF/UA)
static bool hummus_embed_fonts= false;
#include "Metafont/tex_files.hpp"
#include "Metafont/load_tfm.hpp"
#include "Metafont/tex_unicode.hpp"
#include "Freetype/tt_file.hpp"
#include "file.hpp"
#include "image_files.hpp"
#include "analyze.hpp"
#include "iterator.hpp"
#include "merge_sort.hpp"
#include "scheme.hpp"
#include "sys_utils.hpp"
#include "convert.hpp"
#include "converter.hpp"
#include "font.hpp"
#include "ntuple.hpp"
#include "link.hpp"
#include "frame.hpp"
#include "Ghostscript/gs_utilities.hpp" // for gs_prefix
#include "wencoding.hpp"

#ifdef QT_CORE_LIB
#include <QtCore>
#endif
#ifdef QTTEXMACS
#include "Qt/qt_utilities.hpp"
#include "Qt/qt_picture.hpp"
#include <QImage>
#endif

#include "PDFWriter/PDFWriter.h"
#include "PDFWriter/PDFPage.h"
#include "PDFWriter/PageContentContext.h"
#include "PDFWriter/DictionaryContext.h"
#include "PDFWriter/PDFImageXObject.h"
#include "PDFWriter/PDFStream.h"
#include "PDFWriter/PDFDocumentCopyingContext.h"
#include "PDFWriter/InputByteArrayStream.h"
#include "PDFWriter/ProcsetResourcesConstants.h"
#include "PDFWriter/OutputStreamTraits.h"
#include "PDFWriter/XObjectContentContext.h"
#include "PDFWriter/PDFFormXObject.h"
#include "PDFWriter/InfoDictionary.h"
#include "PDFWriter/PDFPageInput.h"
#include "PDFWriter/PDFTiledPattern.h"
#include "PDFWriter/TiledPatternContentContext.h"
#include "PDFWriter/PDFUsedFont.h"
#include "PDFWriter/PDFParser.h"
#include "PDFWriter/PDFDictionary.h"
#include "PDFWriter/PDFArray.h"
#include "PDFWriter/PDFName.h"
#include "PDFWriter/PDFInteger.h"
#include "PDFWriter/PDFStreamInput.h"
#include "PDFWriter/PDFIndirectObjectReference.h"
#include "PDFWriter/PDFObjectCast.h"
#include "PDFWriter/PDFLiteralString.h"
#include "PDFWriter/PDFHexString.h"
#include "PDFWriter/PDFTextString.h"
#include "PDFWriter/FreeTypeWrapper.h"
#include "PDFWriter/FreeTypeFaceWrapper.h"
#include "PDFWriter/CFFEmbeddedFontWriter.h"
#include "Freetype/pdf_font_subst.hpp"
#include <map>

static int hummus_glyph_unicode (PDFUsedFont* f, unsigned int gid);
 
/******************************************************************************
 * pdf_hummus_renderer
 ******************************************************************************/

static EPDFVersion ePDFVersion= ePDFVersion14;

typedef triple<int,int,int> rgb;
typedef quartet<string,int,SI,SI> dest_data;
typedef quintuple<string,int,SI,SI,int> outline_data;

class pdf_image;
class pdf_raw_image;
class t3font;
class pdf_pattern;

class pdf_hummus_renderer_rep : public renderer_rep {
  
  static const int default_dpi= 72; // PDF initial coordinate system corresponds to 72 dpi
  bool		started;  // initialisation is OK
  url       pdf_file_name;
  int       dpi;
  int       nr_pages;
  string    page_type;
  bool      landscape;
  double    paper_w;
  double    paper_h;

  ObjectIDType initial_GState_id; // GSstate with default values

  int       page_num;
  bool      inText;
  color     fg, bg;
  SI        lw;
  double    current_width;
  int       clip_level;
  
  pencil   pen;
  brush    bgb, fgb;

  string    cfn;
  PDFUsedFont* cfid;
  double fsize;
  double prev_text_x, prev_text_y;
  
  double width, height;
  
  
  hashmap<string,PDFUsedFont*> native_fonts;
  hashset<string> not_native_fonts;
  hashset<string> EuropeanComputerModern_fonts;
  hashset<string> T2A_fonts;
  hashmap<string,pdf_raw_image> pdf_glyphs;
  hashmap<tree,pdf_image> image_pool;
  array<pdf_image> graft_images;     // the uses of grafted pictures
  hashset<string> untagged_files;    // the pictures of no structure
  hashmap<tree,pdf_image> pattern_image_pool;
  hashmap<tree,pdf_pattern> pattern_pool;
  hashmap<unsigned long long int,url> picture_cache;
  array<url> temp_images;
  
  hashmap<int,ObjectIDType> alpha_id;
  hashmap<int,ObjectIDType> page_id;
  int t3font_registry_id;
  hashmap<string,t3font> t3font_list;
  
  // link annotation support
  hashmap<ObjectIDType,string> annot_list;

  // embedded videos: file object of each video, and the files to embed
  hashmap<string,ObjectIDType> video_file_id;
  hashmap<ObjectIDType,url> video_files;
  hashmap<ObjectIDType,int> video_annot_page; // /P is only known at the end
  list<dest_data> dests;
  ObjectIDType destId;
  hashmap<string,int> label_id;
  int label_count;
  hashmap<string,string> metadata;

  // a tagged PDF (docs/pdf-tagging.md): the structure, once there is a
  // tagger, the annotations of the structure (links, videos), and the
  // objects which the catalog names
  pdf_tags tags;
  array<ObjectIDType> tag_annot_ids;
  ObjectIDType struct_tree_id, xmp_id;

  // outline support
  ObjectIDType outlineId;
  list<outline_data> outlines;
  
  
  PDFWriter pdfWriter;
  PDFPage* page;
  PageContentContext* contentContext;
  
  // geometry
  
  double to_x (SI x) {
    x += ox;
    if (x>=0) x= x/pixel; else x= (x-pixel+1)/pixel;
    return x;
  };

  double to_y (SI y) {
    y += oy;
    if (y>=0) y= y/pixel; else y= (y-pixel+1)/pixel;
    return y;
  };
  
  // various internal routines

  void init_page_size ();
  void select_stroke_color (color c);
  void select_fill_color (color c);
  void select_alpha (int a);
  
  void register_pattern_image (brush br, SI pixel);
  void select_stroke_pattern (brush br);
  void select_fill_pattern (brush br);

  void select_line_width (SI w);
  void compile_glyph (scheme_tree t);
  void begin_text ();
  void end_text ();
  void begin_actual_text (string s);
  void end_actual_text ();
  void draw_invisible (int c, font_glyphs fn, SI x, SI y, double hscale);
  
  void begin_page();
  void end_page();
  
  int get_label_id(string label);

  // various internal routines
  void flush_images();
  void flush_patterns();
  void flush_glyphs();
  void flush_dests();
  void flush_outlines();
  void flush_fonts();
  PDFImageXObject *create_pdf_image_raw (string raw_data, SI width, SI height, ObjectIDType imageXObjectID);
  void make_pdf_font (string fontname);
  void draw_bitmap_glyph (int ch, font_glyphs fn, SI x, SI y);
  void  image (url u, double w, double h, SI x, SI y, int alpha);
  
  void bezier_arc (SI x1, SI y1, SI x2, SI y2, int alpha, int delta, bool filled);

  // hooks for the Hummus library

  class DestinationsWriter : public DocumentContextExtenderAdapter
  {
    pdf_hummus_renderer_rep *ren;
  public:
    DestinationsWriter(pdf_hummus_renderer_rep *_ren) : ren(_ren) {}
    virtual ~DestinationsWriter(){}
    
    // IDocumentContextExtender implementation
    virtual PDFHummus::EStatusCode OnCatalogWrite(CatalogInformation* inCatalogInformation,
                                                  DictionaryContext* inCatalogDictionaryContext,
                                                  ObjectsContext* inPDFWriterObjectContext,
                                                  PDFHummus::DocumentContext* inDocumentContext)
    {
      ren->on_catalog_write (inCatalogInformation, inCatalogDictionaryContext, inPDFWriterObjectContext, inDocumentContext);
      return eSuccess;
    }
    virtual PDFHummus::EStatusCode OnPageWrite(PDFPage* inPage,
                                               DictionaryContext* inPageDictionaryContext,
                                               ObjectsContext* inPDFWriterObjectContext,
                                               PDFHummus::DocumentContext* inDocumentContext)
    {
      (void) inPage; (void) inPDFWriterObjectContext; (void) inDocumentContext;
      ren->on_page_write (inPageDictionaryContext);
      return eSuccess;
    }
    virtual PDFHummus::EStatusCode OnFormXObjectWrite (
                     ObjectIDType inFormXObjectID,
                     ObjectIDType inFormXObjectResourcesDictionaryID,
                     DictionaryContext* inFormDictionaryContext,
                     ObjectsContext* inPDFWriterObjectContext,
                     PDFHummus::DocumentContext* inDocumentContext)
    {
      (void) inFormXObjectResourcesDictionaryID;
      (void) inPDFWriterObjectContext; (void) inDocumentContext;
      ren->on_form_write (inFormXObjectID, inFormDictionaryContext);
      return eSuccess;
    }
  };
  
  void on_page_write (DictionaryContext* page_dict);
  void on_form_write (ObjectIDType id, DictionaryContext* form_dict);
  hashmap<int,int> form_struct_parents;   // the grafted pictures' forms
  array<ObjectIDType> graft_form_ids;     // the form of each graft
  pdf_tags tg ();
  void tag_mark (int kind);
  void tag_close ();
  void write_structure ();
  PDFHummus::EStatusCode on_catalog_write (CatalogInformation* inCatalogInformation,
                                           DictionaryContext* inCatalogDictionaryContext,
                                           ObjectsContext* inPDFWriterObjectContext,
                                           PDFHummus::DocumentContext* inDocumentContext);
  
  
  void recurse (ObjectsContext& objectsContext, int parent_ls,
		list<outline_data>& it, ObjectIDType parentId,
                ObjectIDType& firstId, ObjectIDType& lastId, int &count);

  
public:
  pdf_hummus_renderer_rep (url pdf_file_name, int dpi, int nr_pages,
                           string ptype, bool landsc, double paper_w, double paper_h);
  ~pdf_hummus_renderer_rep ();
  bool is_printer ();
  bool is_started ();
  bool supports_tagging ();
  void next_page ();
  
  void set_transformation (frame fr);
  void reset_transformation ();
  
  void  set_clipping (SI x1, SI y1, SI x2, SI y2, bool restore= false);
  
  pencil get_pencil ();
  brush get_background ();
  void  set_pencil (pencil pen2);
  void  set_brush (brush b2);
  void  set_background (brush b2);

  void  clear_device (SI x1, SI y1, SI x2, SI y2) {
    (void) x1; (void) y1; (void) x2; (void) y2; };

  void  draw (int char_code, font_glyphs fn, SI x, SI y);
  void  line (SI x1, SI y1, SI x2, SI y2);
  void  lines (array<SI> x, array<SI> y);
  void  clear (SI x1, SI y1, SI x2, SI y2);
  void  fill (SI x1, SI y1, SI x2, SI y2);
  void  arc (SI x1, SI y1, SI x2, SI y2, int alpha, int delta);
  void  fill_arc (SI x1, SI y1, SI x2, SI y2, int alpha, int delta);
  void  polygon (array<SI> x, array<SI> y, bool convex=true);
  
  void draw_picture (picture p, SI x, SI y, int alpha);
  void draw_scalable (scalable im, SI x, SI y, int alpha);
  renderer shadow (picture& pic, SI x1, SI y1, SI x2, SI y2);
  void fetch (SI x1, SI y1, SI x2, SI y2, renderer ren, SI x, SI y);
  void new_shadow (renderer& ren);
  void delete_shadow (renderer& ren);
  void get_shadow (renderer ren, SI x1, SI y1, SI x2, SI y2);
  void put_shadow (renderer ren, SI x1, SI y1, SI x2, SI y2);
  void apply_shadow (SI x1, SI y1, SI x2, SI y2);

  /************************ subroutines hyperlinks ***************************/
  
  void anchor (string label, SI x1, SI y1, SI x2, SI y2);
  void href (string label, SI x1, SI y1, SI x2, SI y2);
  void embed_video (url u, int ms, bool repeat, SI x1, SI y1, SI x2, SI y2);
  void flush_videos ();
  void toc_entry (string kind, string title, SI x, SI y);
  void set_metadata (string kind, string val);
  void flush_metadata ();
};

/******************************************************************************
 * local utilities
 ******************************************************************************/


static void
write_indirect_obj(ObjectsContext&  objectsContext, ObjectIDType destId, string payload) {
  objectsContext.StartNewIndirectObject(destId);
  c_string buf (payload);
  objectsContext.StartFreeContext()->Write((unsigned char *)(char*)buf, N(payload));
  objectsContext.EndFreeContext();
  objectsContext.EndIndirectObject();
}

void pdf_image_info (url image, int& w, int& h, PDFRectangle& cropBox, double (&tMat)[6], PDFPageInput& pageInput);
  

/******************************************************************************
* constructors and destructors
******************************************************************************/

pdf_hummus_renderer_rep::pdf_hummus_renderer_rep (
  url pdf_file_name2, int dpi2, int nr_pages2,
  string page_type2, bool landscape2, double paper_w2, double paper_h2):
    renderer_rep (false),
    pdf_file_name (pdf_file_name2), dpi (dpi2),
    nr_pages (nr_pages2), page_type (page_type2),
    landscape (landscape2), paper_w (paper_w2), paper_h (paper_h2),
    page_num(0),
    inText (false),
    fg (-1), bg (-1),
    lw (-1),
    pen (black), bgb (white), fgb (black),
    cfn (""), cfid (NULL),
    native_fonts (NULL),
    t3font_registry_id(-1),
    video_file_id (0), video_files (url_none ()), video_annot_page (0),
    destId(0),
    label_count(0),
    outlineId(0)
{
  width = default_dpi * paper_w / 2.54;
  height= default_dpi * paper_h / 2.54;
  tags= NULL; struct_tree_id= 0; xmp_id= 0;

  // setup library

  EStatusCode status;
  ePDFVersion= ePDFVersion14; // PDF 1.4 for alpha
#ifdef USE_GS
  string version= pdf_version ();
#else
  string version= get_preference ("texmacs->pdf:version");
#endif
  if (version == "1.5") ePDFVersion= ePDFVersion15;
  if (version == "1.6") ePDFVersion= ePDFVersion16;
  if (version == "1.7") ePDFVersion= ePDFVersion17;
  // LogConfiguration log (true, true, "PDFWriterLog.txt");
  LogConfiguration log= LogConfiguration::DefaultLogConfiguration();
  bool compress= true;
  PDFCreationSettings settings (compress, true); //, EncryptionOptions("user", 4, "owner"));
  // A relative name stands for the current directory, as in save_string
  url target= pdf_file_name;
  if (!is_rooted_name (target)) target= resolve (target, "");
    status = pdfWriter.StartPDF(as_charp(concretize (target)), ePDFVersion, log, settings);
	if (status != PDFHummus::eSuccess) {
		convert_error << "failed to start PDF " << pdf_file_name << "\n";
		started=false;
	} else {
		started=true;
		pdfWriter.GetDocumentContext().AddDocumentContextExtender (new DestinationsWriter(this));

		// create a graphic state with default values
		initial_GState_id= pdfWriter.GetObjectsContext()
		  .GetInDirectObjectsRegistry().AllocateNewObjectID();
		ObjectsContext& objectsContext = pdfWriter.GetObjectsContext();
		objectsContext.StartNewIndirectObject(initial_GState_id);
		std::stringstream buf;
		buf << "<< /Type /ExtGState\r\n";
		buf << "/LW 1.0\r\n";
      		buf << "/LC 0\r\n";
      		buf << "/LJ 0\r\n";
      		buf << "/ML 10.0\r\n";
      		//buf << "/D [[] 0]\r\n"; // useless
      		buf << "/RI /RelativeColorimetric\r\n";
      		buf << "/OP false\r\n";
      		buf << "/op false\r\n";
		buf << "/FL 1.0\r\n";
		buf << "/SA false\r\n";
		buf << "/BM /Normal\r\n";
		buf << "/SMask /None\r\n";
		buf << "/CA 1.0\r\n";
		buf << "/ca 1.0\r\n";
		buf << "/AIS false\r\n";
		buf << "/TK true\r\n";
		buf << ">>\r\n";
		objectsContext.StartFreeContext()
		  ->Write((const IOBasicTypes::Byte* )(buf.str().c_str()),buf.str().size());
		objectsContext.EndFreeContext();
		objectsContext.EndIndirectObject();

		// start real work
		begin_page();
	}
}

pdf_hummus_renderer_rep::~pdf_hummus_renderer_rep () {
  if (!started) return; // no cleanup to do
  end_page();
  
  hummus_embed_fonts= (tagger != NULL);
  flush_images();
  hummus_embed_fonts= false;
  flush_patterns();
  flush_glyphs();
  flush_dests();
  flush_outlines();
  flush_fonts();
  flush_metadata();
 
  {
    // flush alphas
    iterator<int> it = iterate(alpha_id);
    ObjectsContext& objectsContext = pdfWriter.GetObjectsContext();
    while (it->busy()) {
      int a = it->next();
      double da = ((double) a)/1000.0;
      objectsContext.StartNewIndirectObject(alpha_id(a));
      {
        std::stringstream buf;
        buf << "<< /Type /ExtGState /CA "<< da << "  /ca "<< da << " >>\r\n";
        objectsContext.StartFreeContext()->Write((const IOBasicTypes::Byte* )(buf.str().c_str()),buf.str().size());
        objectsContext.EndFreeContext();
      }
      objectsContext.EndIndirectObject();
    }
  }
  
  {
    // flush annotations
    iterator<ObjectIDType> it = iterate(annot_list);
    ObjectsContext& objectsContext = pdfWriter.GetObjectsContext();
    while (it->busy()) {
      ObjectIDType id = it->next();
      string dict= annot_list (id);
      if (video_annot_page->contains (id))
        dict= replace (dict, "@PAGE@",
                       as_string (page_id (video_annot_page [id])) * " 0 R");
      write_indirect_obj(objectsContext, id, dict);
    }
  }
  
  flush_videos ();
  write_structure ();

  // (the fonts are written now: no CIDSet in a tagged PDF)
  hummus_write_cidset= (tagger == NULL);
  EStatusCode status = pdfWriter.EndPDF();
  hummus_write_cidset= true;
  if (tags != NULL) { tm_delete (tags); tags= NULL; }
  if (status != PDFHummus::eSuccess) {
    convert_error << "Failed in end PDF\n";
  }

  // remove temporary pictures
  for (int i=0; i<N(temp_images); i++)
    if (!is_none (temp_images[i]) && exists(temp_images[i]))
      remove (temp_images[i]);
}

/******************************************************************************
* Tagged PDF (docs/pdf-tagging.md, the structure in pdf_tags)
******************************************************************************/

bool
pdf_hummus_renderer_rep::supports_tagging () {
  return true;
}

// the structure, made when the tagger is known (print_doc gives it after
// the renderer is made)
pdf_tags
pdf_hummus_renderer_rep::tg () {
  if (tags == NULL) tags= tm_new<pdf_tags_rep> (tagger);
  return tags;
}

// The drawing which follows is of the box being drawn: of kind 0 (text),
// 1 (lines, fills) or 2 (images)
void
pdf_hummus_renderer_rep::on_form_write (ObjectIDType id,
                                        DictionaryContext* form_dict) {
  // the form of a grafted picture: its key in the parent tree
  if (!form_struct_parents->contains ((int) id)) return;
  form_dict->WriteKey ("StructParents");
  form_dict->WriteIntegerValue (form_struct_parents [(int) id]);
}

void
pdf_hummus_renderer_rep::tag_mark (int kind) {
  if (tagger == NULL || contentContext == NULL) return;
  string ops= tg ()->mark (tag_ip, kind, page_num);
  if (N(ops) == 0) return;
  // a block of marked content may not cross a text object
  end_text ();
  c_string cops (ops);
  contentContext->WriteFreeCode (std::string ((char*) cops));
}

void
pdf_hummus_renderer_rep::tag_close () {
  if (tags == NULL || contentContext == NULL) return;
  string ops= tags->close ();
  if (N(ops) == 0) return;
  end_text ();
  c_string cops (ops);
  contentContext->WriteFreeCode (std::string ((char*) cops));
}

// the page: its marked contents are entry page_num of the parent tree, and
// its annotations are visited in the order of the structure (PDF/UA)
void
pdf_hummus_renderer_rep::on_page_write (DictionaryContext* page_dict) {
  if (tagger == NULL) return;
  page_dict->WriteKey ("StructParents");
  page_dict->WriteIntegerValue (page_num);
  page_dict->WriteKey ("Tabs");
  page_dict->WriteNameValue ("S");
}

static string escape_string (string s);   // below

// a text string of a PDF: literal when it is printable ASCII, which is
// read as it is, UTF-16 otherwise
static string
pdf_text_string (string s) {
  for (int i=0; i<N(s); i++)
    if (((unsigned char) s[i]) < 32 || ((unsigned char) s[i]) > 126)
      return utf8_to_pdf_hex_string (s);
  return "(" * escape_string (s) * ")";
}

// The structure tree, the parent tree and the XMP metadata, whose objects
// the catalog names (on_catalog_write)
void
pdf_hummus_renderer_rep::write_structure () {
  if (tags == NULL || N(tags->elems) == 0) return;
  ObjectsContext& oc= pdfWriter.GetObjectsContext ();
  IndirectObjectsReferenceRegistry& reg= oc.GetInDirectObjectsRegistry ();
  int n= N(tags->elems);
  array<ObjectIDType> ids (n);
  for (int e=0; e<n; e++) ids[e]= reg.AllocateNewObjectID ();
  struct_tree_id= reg.AllocateNewObjectID ();
  ObjectIDType pt_id= reg.AllocateNewObjectID ();
  xmp_id= reg.AllocateNewObjectID ();
  array<array<int> > kids= tags->children ();
  for (int e=0; e<n; e++) {
    pdf_tag_elem& el= tags->elems[e];
    string d;
    d << "<< /Type /StructElem /S /" << el.role << " /P "
      << as_string ((int) (el.parent < 0? struct_tree_id: ids[el.parent]))
      << " 0 R";
    if (N(el.alt) > 0) d << " /Alt " << pdf_text_string (el.alt);
    if (el.role == "Note") d << " /ID (note-" << as_string (e) << ")";
    if (N(el.akeys) > 0) {
      // the attributes of a cell (its spans), owner Table
      d << " /A << /O /Table";
      for (int j=0; j<N(el.akeys); j++)
        d << " /" << el.akeys[j] << " " << as_string (el.avals[j]);
      d << " >>";
    }
    d << "\r\n /K [";
    for (int i=0; i<N(kids[e]); i++) {
      pdf_tag_kid& kd= tags->kids[kids[e][i]];
      if (kd.kind == 0) d << " " << as_string ((int) ids[kd.id]) << " 0 R";
      else if (kd.kind == 1 && page_id->contains (kd.page))
        d << " << /Type /MCR /Pg " << as_string ((int) page_id [kd.page])
          << " 0 R /MCID " << as_string (kd.id) << " >>";
      else if (kd.kind == 2 && page_id->contains (kd.page) &&
               kd.id < N(tag_annot_ids))
        d << " << /Type /OBJR /Obj " << as_string ((int) tag_annot_ids[kd.id])
          << " 0 R /Pg " << as_string ((int) page_id [kd.page]) << " 0 R >>";
      else if (kd.kind == 3 && page_id->contains (kd.page) &&
               kd.stream < N(graft_form_ids))
        // the marked content of a grafted picture, in its form
        d << " << /Type /MCR /Pg " << as_string ((int) page_id [kd.page])
          << " 0 R /Stm " << as_string ((int) graft_form_ids[kd.stream])
          << " 0 R /MCID " << as_string (kd.id) << " >>";
    }
    d << " ] >>\r\n";
    write_indirect_obj (oc, ids[e], d);
  }
  // the parent tree: for each page its elements by MCID, then each
  // annotation, under keys above those of the pages
  string pt= "<< /Nums [";
  array<array<int> >& pm= tags->page_mcids;
  for (int p=0; p<N(pm); p++) {
    pt << "\r\n " << as_string (p) << " [";
    for (int m=0; m<N(pm[p]); m++)
      pt << " " << as_string ((int) ids[pm[p][m]]) << " 0 R";
    pt << " ]";
  }
  for (int i=0; i<N(tags->kids); i++)
    if (tags->kids[i].kind == 2)
      pt << "\r\n " << as_string (pdf_tag_annot_key0 + tags->kids[i].id)
         << " " << as_string ((int) ids[tags->kids[i].parent]) << " 0 R";
  // the forms of the grafted pictures: their elements by MCID
  array<array<int> >& sm= tags->stream_mcids;
  for (int k=0; k<N(sm); k++) {
    pt << "\r\n " << as_string (pdf_tag_stream_key0 + k) << " [";
    for (int m=0; m<N(sm[k]); m++)
      if (sm[k][m] >= 0) pt << " " << as_string ((int) ids[sm[k][m]]) << " 0 R";
      else pt << " null";
    pt << " ]";
  }
  pt << " ] >>\r\n";
  write_indirect_obj (oc, pt_id, pt);
  string root;
  root << "<< /Type /StructTreeRoot /K " << as_string ((int) ids[0])
       << " 0 R /ParentTree " << as_string ((int) pt_id)
       << " 0 R /ParentTreeNextKey "
       << as_string (N(sm) > 0? pdf_tag_stream_key0 + N(sm):
                            pdf_tag_annot_key0 + N(tag_annot_ids))
       << " >>\r\n";
  write_indirect_obj (oc, struct_tree_id, root);
  // the XMP metadata, with the part of PDF/UA (an uncompressed stream)
  string title= cork_to_utf8 (tagger->title ());
  if (N(title) == 0 && metadata->contains ("title"))
    title= cork_to_utf8 (metadata ["title"]);
  if (N(title) == 0) title= as_string (tail (pdf_file_name));
  string author= (metadata->contains ("author")?
                  cork_to_utf8 (metadata ["author"]): string (""));
  string xmp= pdf_tags_xmp (title, author,
                            "TeXmacs " * string (TEXMACS_VERSION) * " + Hummus 4.0");
  string pre;
  pre << "<< /Type /Metadata /Subtype /XML /Length " << as_string (N(xmp))
      << " >>\r\nstream\r\n";
  string post= "\r\nendstream\r\n";
  oc.StartNewIndirectObject (xmp_id);
  IByteWriter* w= oc.StartFreeContext ();
  w->Write ((unsigned char*) &(pre[0]), N(pre));
  w->Write ((unsigned char*) &(xmp[0]), N(xmp));
  w->Write ((unsigned char*) &(post[0]), N(post));
  oc.EndFreeContext ();
  oc.EndIndirectObject ();
}

bool
pdf_hummus_renderer_rep::is_printer () {
  // debug_convert << "is_printer\n";
  return true;
}

bool
pdf_hummus_renderer_rep::is_started () {
  return started;
}

void
pdf_hummus_renderer_rep::next_page () {
  end_page();
  begin_page();
}

void
pdf_hummus_renderer_rep::begin_page() {
  //EStatusCode status;

  page = new PDFPage();
  page->SetMediaBox(PDFRectangle(0,0,width,height));
  contentContext = pdfWriter.StartPageContentContext(page);
  if (NULL == contentContext) {
    //status = PDFHummus::eFailure;
    convert_error << "Failed to create content context for page\n";
  } else {
    fg  = -1;
    bg  = -1;
    lw  = -1;
    current_width = -1.0;
    cfn= "";
    cfid = NULL;
    inText = false;
    clip_level = 0;
    
      // outmost save of the graphics state
    contentContext->q();
      // set scaling suitable for dpi (pdf default is 72)
    contentContext->cm((double)default_dpi / dpi, 0, 0, (double)default_dpi / dpi, 0, 0);
    
    set_origin (0, paper_h*dpi*pixel/2.54);
    set_clipping (0, (int) ((-dpi*pixel*paper_h)/2.54), (int) ((dpi*pixel*paper_w)/2.54), 0);
  }
}

void
pdf_hummus_renderer_rep::end_page(){
  
  if (!page) return;
  EStatusCode status;

  tag_close ();
  end_text ();
  

  // reset set_clipping calls in order to have well formed PDF.
  while (clip_level--)
    contentContext->Q();
  
  // outmost restore for the graphics state (see begin_page)
  contentContext->Q();

  status = pdfWriter.EndPageContentContext(contentContext);
  if (status != PDFHummus::eSuccess) {
    convert_error << "Failed to end page content context\n";
  }
  
  EStatusCodeAndObjectIDType res = pdfWriter.GetDocumentContext().WritePageAndRelease(page);
  status = res.first;
  if (status != PDFHummus::eSuccess) {
    convert_error << "Failed to write page and release\n";
  }

  page_id (page_num) = res.second;
  if (tags != NULL) tags->end_page ();
  page_num++;
}

void
pdf_hummus_renderer_rep::begin_text () {
  if (!inText) {
    contentContext->BT();
    inText = true;
    prev_text_x = to_x(0);
    prev_text_y = to_y(0);
    contentContext->Tm(1,0,0,1, prev_text_x, prev_text_y);
  }
}


void
pdf_hummus_renderer_rep::end_text () {
  if (inText) {
    contentContext->ET();
    inText = false;
  }
}

// The span is in the text object, as the glyphs it covers (a drawing of
// glyphs does not end the text object)
void
pdf_hummus_renderer_rep::begin_actual_text (string s) {
  tag_mark (0);
  if (tagger != NULL) tg ()->span++;   // no new block in the span
  // (marked content is of PDF 1.2, its ActualText of 1.5, which the
  // readers take whatever the version of the file)
  begin_text ();
  c_string hex (utf8_to_pdf_hex_string (s));
  contentContext->WriteFreeCode ("/Span << /ActualText " +
                                 std::string ((char*) hex) + " >> BDC ");
}

void
pdf_hummus_renderer_rep::end_actual_text () {
  if (tagger != NULL && tg ()->span > 0) tg ()->span--;
  contentContext->WriteFreeCode (" EMC\r\n");
}

void
pdf_hummus_renderer_rep::draw_invisible (int c, font_glyphs fn, SI x, SI y,
                                         double hscale) {
  tag_mark (0);
  begin_text ();
  contentContext->Tr (3);
  if (hscale != 1.0) contentContext->Tz ((int) (100.0 * hscale + 0.5));
  draw (c, fn, x, y);
  if (hscale != 1.0) contentContext->Tz (100);
  contentContext->Tr (0);
}

/******************************************************************************
 * Transformed rendering
 ******************************************************************************/

void
pdf_hummus_renderer_rep::set_transformation (frame fr) {
  ASSERT (fr->linear, "only linear transformations have been implemented");
  
  end_text ();

  SI cx1, cy1, cx2, cy2;
  get_clipping (cx1, cy1, cx2, cy2);
  rectangle oclip (cx1, cy1, cx2, cy2);
  frame cv= scaling (point (pixel, pixel),
                     point (-ox, -oy));
  frame tr= invert (cv) * fr * cv;
  point o = tr (point (0.0, 0.0));
  point ux= tr (point (1.0, 0.0)) - o;
  point uy= tr (point (0.0, 1.0)) - o;
  // debug_convert << "Set transformation " << o << ", " << ux << ", " << uy << "\n";
  double tx= o[0];
  double ty= o[1];
  contentContext->q();
  contentContext->cm(ux[0],ux[1],uy[0],uy[1],tx,ty);
  
  rectangle nclip= fr [oclip];
  renderer_rep::clip (nclip->x1, nclip->y1, nclip->x2, nclip->y2);
}

void
pdf_hummus_renderer_rep::reset_transformation () {
  end_text ();
  renderer_rep::unclip ();
  contentContext->Q();
}

/******************************************************************************
* Clipping
******************************************************************************/

void
pdf_hummus_renderer_rep::set_clipping (SI x1, SI y1, SI x2, SI y2, bool restore) {
  renderer_rep::set_clipping (x1, y1, x2, y2, restore);

  end_text();
  
  outer_round (x1, y1, x2, y2);
  if (restore) {
    // debug_convert << "restore clipping\n";
    if (clip_level > 0) { contentContext->Q(); clip_level--; }
    cfn= "";
  }
  else {
    // debug_convert << "set clipping\n";
    contentContext->q(); clip_level++;
    double xx1= to_x (min (x1, x2));
    double yy1= to_y (min (y1, y2));
    double xx2= to_x (max (x1, x2));
    double yy2= to_y (max (y1, y2));
    contentContext->re(xx1, yy1, xx2-xx1, yy2-yy1);
    contentContext->W();
    contentContext->n();
  }
}

/******************************************************************************
 * Images
 ******************************************************************************/

class pdf_image_rep : public concrete_struct
{
public:
  url u;
  int w,h;
  ObjectIDType id;
  bool graft;            // a tagged picture whose structure is grafted:
  hashset<int> keep;     // the MCIDs of its elements (pdf_tags_rep::graft)
  
  pdf_image_rep(url _u, ObjectIDType _id)
    : u(_u), id(_id), graft (false)
  { image_size (u, w, h); } 
  ~pdf_image_rep() {}

  bool flush_jpg (PDFWriter& pdfw, url image);
#ifndef PDFHUMMUS_NO_PNG
  bool flush_png (PDFWriter& pdfw, url image);
#endif
  // (the uses of a grafted picture share the copy of its file, shared:
  // its copying context and its file, by the name of the picture)
  void flush (PDFWriter& pdfw, hashmap<string,pointer>* shared= NULL,
              hashmap<string,string>* shared_file= NULL,
              array<url>* to_remove= NULL);
  EStatusCode merge (PDFDocumentCopyingContext* cc, PDFWriter& pdfw,
                     url temp);

  bool flush_for_pattern (PDFWriter& pdfw);
}; // class pdf_image_ref

class pdf_image {
  CONCRETE_NULL(pdf_image);
  pdf_image (url _u, ObjectIDType _id):
    rep (tm_new<pdf_image_rep> (_u,_id)) {};
};

CONCRETE_NULL_CODE(pdf_image);

/******************************************************************************
 * Tiled patterns
 ******************************************************************************/

class pdf_pattern_rep : public concrete_struct {
public:
  pdf_image im;
  SI w, h, sx, sy;
  double scale_x, scale_y;
  ObjectIDType id;
  
  pdf_pattern_rep (pdf_image _im, SI _w, SI _h, SI _sx, SI _sy,
		   double _scale_x, double _scale_y, ObjectIDType _id)
    : im (_im), w (_w), h (_h), sx (_sx), sy (_sy),
      scale_x (_scale_x), scale_y (_scale_y), id (_id) {}
  ~pdf_pattern_rep () {}

  void flush (PDFWriter& pdfw) {
    const double matrix[]= { scale_x, 0, 0, scale_y, (double) sx, (double) sy };
    DocumentContext& documentContext= pdfw.GetDocumentContext();
    PDFTiledPattern* tiledPattern= documentContext.StartTiledPattern
      (1, // int inPaintType,
       2, // int inTilingType,
       PDFRectangle(0, 0, w, h),
       w, // double inXStep,
       h, // double inYStep,
       id,
       matrix);
    TiledPatternContentContext* tiledPatternContentContext =
      tiledPattern->GetContentContext();
    std::string imageName=
      tiledPattern->GetResourcesDictionary().AddImageXObjectMapping (im->id);
    tiledPatternContentContext->q();
    tiledPatternContentContext->cm(w, 0, 0, h, 0, 0);
    tiledPatternContentContext->Do(imageName);
    tiledPatternContentContext->Q();
    PDFHummus::EStatusCode st=
      documentContext.EndTiledPatternAndRelease (tiledPattern);
    if (st != PDFHummus::eSuccess)
      convert_error << "Cannot flush tiled pattern "
                    << im->u << "\n"; }
};

class pdf_pattern {
  CONCRETE_NULL(pdf_pattern);
  pdf_pattern (pdf_image _im,  SI _w, SI _h, SI _sx, SI _sy,
	       double _scale_x, double _scale_y, ObjectIDType _id):
    rep (tm_new<pdf_pattern_rep> (_im, _w, _h, _sx, _sy,
				  _scale_x, _scale_y, _id)) {};
};

CONCRETE_NULL_CODE(pdf_pattern);

void
pdf_hummus_renderer_rep::register_pattern_image (brush br, SI pixel) {
  // debug_convert << "register_pattern_image\n";
  if (is_nil (br) || br->get_type () != brush_pattern) {
    convert_warning << "register_pattern_image: "
                    << "brush with pattern expected\n";
    return;
  }
  tree p= br->get_pattern ();
  // debug_convert << p << "\n";
  if (pattern_pool->contains(p)) return;

  url u;
  SI w, h;
  tree eff;
  get_pattern_data (u, w, h, eff, br, pixel);
  tree key= tuple (u->t, as_string (w), as_string (h), eff);
  
  pdf_image image_pdf;
  if (pattern_image_pool->contains(key))
    image_pdf= pattern_image_pool[key];
  else {
    // debug_convert << "Insert pattern image\n";
    url temp= url_temp (".png");
#ifdef QTTEXMACS
    QImage* pim = get_image (u, w, h, eff, pixel);
    if (pim == NULL) {
      convert_error << "Cannot read image file '" << u << "'"
		    << " with get_image" << LF;
      return;
    }
    if (w != pim->width () || h != pim->height ()) {
      convert_error << "Invalid image size '" << u << "'"
		    << " after get_image" << LF;
      return;
    }
    pim->save (utf8_to_qstring (concretize (temp)), "PNG");
#else
    // the tile through the pictures of the interface (Cocoa)
    picture pic= load_picture (u, w, h, eff, pixel);
    if (is_nil (pic) || pic->get_width () != w || pic->get_height () != h) {
      convert_error << "Cannot read image file '" << u << "'"
		    << " with load_picture" << LF;
      return;
    }
    save_picture (temp, pic);
#endif
    temp_images << temp;
    ObjectIDType image_id= pdfWriter.GetObjectsContext()
      .GetInDirectObjectsRegistry().AllocateNewObjectID();
    image_pdf= pdf_image (temp, image_id);
    pattern_image_pool(key) = image_pdf;
  }
  // debug_convert << "  insert pattern\n";
  ObjectIDType id= pdfWriter.GetObjectsContext()
    .GetInDirectObjectsRegistry().AllocateNewObjectID();
  // debug_convert << "pdf_pattern " << ox << ", " << oy
  //  		   << ", " << pixel << ", " << shrinkf
  //		   << ", " << zoomf << LF;
  // debug_convert << "            " << to_x(0) << ", " << to_y(0) << LF;
  // debug_convert << "            " << w << ", " << h << LF;
  pdf_pattern p_pdf (image_pdf, w, h,
		     width + to_x(0), height, // FIXME ???
		     ((double) default_dpi) / dpi,
		     ((double) default_dpi) / dpi, id);
  pattern_pool(p) = p_pdf;
}

/******************************************************************************
 * Graphic state management
 ******************************************************************************/

void
pdf_hummus_renderer_rep::select_alpha (int a) {
  if (!alpha_id->contains(a)) {
    ObjectIDType temp = pdfWriter.GetObjectsContext().GetInDirectObjectsRegistry().AllocateNewObjectID();
    alpha_id(a) = temp;
  }
  std::string name = page->GetResourcesDictionary().AddExtGStateMapping(alpha_id(a));
  contentContext->gs(name);
}

void
pdf_hummus_renderer_rep::select_stroke_color (color c) {;
  int r, g, b, a;
  get_rgb_color (c, r, g, b, a);
  r= ((r*1000)/255);
  g= ((g*1000)/255);
  b= ((b*1000)/255);
  a= ((a*1000)/255);
  double dr= ((double) r) / 1000.0;
  double dg= ((double) g) / 1000.0;
  double db= ((double) b) / 1000.0;
  contentContext->RG(dr, dg, db); // stroke color;
  select_alpha(a);
}

void
pdf_hummus_renderer_rep::select_fill_color (color c) {;
  int r, g, b, a;
  get_rgb_color (c, r, g, b, a);
  r= ((r*1000)/255);
  g= ((g*1000)/255);
  b= ((b*1000)/255);
  a= ((a*1000)/255);
  double dr= ((double) r) / 1000.0;
  double dg= ((double) g) / 1000.0;
  double db= ((double) b) / 1000.0;
  contentContext->rg(dr, dg, db); // non-stroking color
  select_alpha(a);
}

void
pdf_hummus_renderer_rep::select_stroke_pattern (brush br) {
  if (is_nil(br) || br->get_type () != brush_pattern) return;
  tree p_tree= br->get_pattern ();
  register_pattern_image (br, brushpx == -1 ? pixel : brushpx);
  if (!pattern_pool->contains (p_tree)) {
    convert_error << "select_stroke_pattern: "
                  << "cannot find registered pattern\n";
    return;
  }
  pdf_pattern p= pattern_pool[p_tree];
  std::string patternName=
    page->GetResourcesDictionary().AddPatternMapping(p->id);
  contentContext->CS("Pattern");
  contentContext->SCN((double*) NULL, 0, patternName);
}

void
pdf_hummus_renderer_rep::select_fill_pattern (brush br) {
  if (is_nil(br) || br->get_type () != brush_pattern) return;
  tree p_tree= br->get_pattern ();
  register_pattern_image (br, brushpx==-1? pixel: brushpx);
  if (!pattern_pool->contains (p_tree)) {
    convert_error << "select_fill_pattern: "
                  << "cannot find registered pattern\n";
    return;
  }
  pdf_pattern p= pattern_pool[p_tree];
  std::string patternName=
    page->GetResourcesDictionary().AddPatternMapping(p->id);
  contentContext->cs("Pattern");
  contentContext->scn((double*) NULL, 0, patternName);
  select_alpha ((1000*br->get_alpha ())/255);
}

void
pdf_hummus_renderer_rep::select_line_width (SI w) {
  double pw = w /pixel;
  //if (pw < 1) pw= 1;
  if (pw != current_width) {
    contentContext->w(pw);
    current_width = pw;
  }
}

pencil
pdf_hummus_renderer_rep::get_pencil () {
  // debug_convert << "get_color\n";
  return pen;
}

brush
pdf_hummus_renderer_rep::get_background () {
  // debug_convert << "get_background\n";
  return bgb;
}

void
pdf_hummus_renderer_rep::set_pencil (pencil pen2) {
  // debug_convert << "set_pencil\n";
  pen= pen2;
  lw= pen->get_width ();
  select_line_width (lw);
  color c= pen->get_color ();
  fg= c;
  select_fill_color (c);
  select_stroke_color (c);
  if (pen->get_type () == pencil_brush) {
    // debug_convert << "pencil has brush type" << LF;
    brush br= pen->get_brush ();
    fgb= br;
    select_fill_pattern (br);
    select_stroke_pattern (br);
  }
  if (pen->get_cap () == cap_round)
    contentContext->J(1); // round cap
  else
    contentContext->J(2); // square cap
  contentContext->j(1); // round join
}

void
pdf_hummus_renderer_rep::set_brush (brush br) {
  // debug_convert << "set_brush\n";
  fgb= br;
  pen= pencil (br);
  //set_pencil (pen);  // FIXME ???
  if (is_nil (br)) return;
  if (br->get_type () == brush_none) {
    //pen = pencil ();
    fgb = brush ();
  }
  else {
    select_fill_color (pen->get_color ());
    select_stroke_color (pen->get_color ());
  }
  if (br->get_type () == brush_pattern) {
    tree p_tree= br->get_pattern ();
    register_pattern_image (br, brushpx == -1 ? pixel : brushpx);
    if (!pattern_pool->contains (p_tree)) {
      convert_error << "select_stroke_pattern: "
		    << "cannot find registered pattern\n";
      return;
    }
    select_fill_pattern (br);
    select_stroke_pattern (br);
  }
  //select_alpha (br->get_alpha ());
}
void
pdf_hummus_renderer_rep::set_background (brush b) {
  // debug_convert << "set_background\n";
  //if (bgb==b) return;
  bgb= b;
  bg= b->get_color ();
}

/******************************************************************************
 * Raw images
 ******************************************************************************/

// obsolete support for direct placement of virtual glyphs without Type3 fonts.

static string
load_virtual_glyph (glyph gl) {
  string buf;
  int i, j;
  for (j= 0; j < gl->height; j++)
    for (i= 0; i < gl->width; i++) {
      if (gl->get_x (i, j) > 0) buf << (char)0;
      else buf << (char)255;
    }
 return buf;
}

static const std::string scType = "Type";
static const std::string scXObject = "XObject";
static const std::string scSubType = "Subtype";

static const std::string scImage = "Image";
static const std::string scWidth = "Width";
static const std::string scHeight = "Height";
static const std::string scColorSpace = "ColorSpace";
static const std::string scDeviceGray = "DeviceGray";
static const std::string scDeviceRGB = "DeviceRGB";
static const std::string scDeviceCMYK = "DeviceCMYK";
static const std::string scDecode = "Decode";
static const std::string scBitsPerComponent = "BitsPerComponent";
static const std::string scFilter = "Filter";
static const std::string scDCTDecode = "DCTDecode";
static const std::string scLength = "Length";


static void
create_pdf_image_raw (PDFWriter& pdfw, string raw_data, SI width, SI height, ObjectIDType imageXObjectID)
{
  
	PDFImageXObject* imageXObject = NULL;
	//EStatusCode status = PDFHummus::eSuccess;

  ObjectsContext& objectsContext = pdfw.GetObjectsContext();
  objectsContext.StartNewIndirectObject(imageXObjectID);
  do {
    {
      // write stream dictionary
      DictionaryContext* imageContext = objectsContext.StartDictionary();
      // type
      imageContext->WriteKey(scType);
      imageContext->WriteNameValue(scXObject);
      // subtype
      imageContext->WriteKey(scSubType);
      imageContext->WriteNameValue(scImage);
      // Width
      imageContext->WriteKey(scWidth);
      imageContext->WriteIntegerValue(width);
      // Height
      imageContext->WriteKey(scHeight);
      imageContext->WriteIntegerValue(height);
      // Bits Per Component
      imageContext->WriteKey(scBitsPerComponent);
      imageContext->WriteIntegerValue(8);
      // Color Space and Decode Array if necessary
      imageContext->WriteKey(scColorSpace);
      imageContext->WriteNameValue(scDeviceGray);
      // Length
      imageContext->WriteKey("Length");
      imageContext->WriteIntegerValue(N(raw_data));
      objectsContext.EndDictionary(imageContext);
    }
    {
      // write stream
      objectsContext.WriteKeyword("stream");
      {
        c_string buf (raw_data);
        objectsContext.StartFreeContext()->Write((unsigned char*)(char *)buf, N(raw_data));
        objectsContext.EndFreeContext();
      }
      objectsContext.EndLine();
      objectsContext.WriteKeyword("endstream");
    }
    objectsContext.EndIndirectObject();
    imageXObject = new PDFImageXObject(imageXObjectID, KProcsetImageB);
  } while(false);
 
  if (imageXObject == NULL)
    convert_error <<  "pdf_hummus, failed to include glyph" << LF;
  else delete imageXObject;
}

class pdf_raw_image_rep : public concrete_struct
{
public:
  string data;
  int w,h;
  ObjectIDType id;
  
  pdf_raw_image_rep (string _data, int _w, int _h, ObjectIDType _id)
   : data(_data), w(_w), h(_h), id(_id) {}
  pdf_raw_image_rep () {}
  
  void flush(PDFWriter& pdfw) {
    // debug_convert << "flushing :" << id << LF;
    create_pdf_image_raw (pdfw, data, w, h, id);
  }
}; // pdf_raw_image_rep

class pdf_raw_image {
  CONCRETE_NULL(pdf_raw_image);
  pdf_raw_image (string _data, int _w, int _h, ObjectIDType _id):
  rep (tm_new<pdf_raw_image_rep> (_data,_w,_h,_id)) {};
};

CONCRETE_NULL_CODE(pdf_raw_image);

void
pdf_hummus_renderer_rep::flush_glyphs ()
{
  // flush all images
  iterator<string> it = iterate (pdf_glyphs);
  while (it->busy()) {
    pdf_raw_image im = pdf_glyphs [it->next()];
    im->flush (pdfWriter);
  }
}

void
pdf_hummus_renderer_rep::draw_bitmap_glyph (int ch, font_glyphs fn, SI x, SI y)
{
  (void) x; (void) y;
  // use bitmap (to be improved)
  string fontname = fn->res_name;
  string char_name (fontname * "-" * as_string ((int) ch));
  if (!pdf_glyphs->contains(char_name)) {
    glyph gl= fn->get(ch);
    // debug_convert << "draw bitmap glyph " << (double)gl->width / 8 << " " << (double)gl->height / 8 << "\n";
    if (is_nil (gl)) return;
    string buf= load_virtual_glyph (gl);
    ObjectIDType imageXObjectID  = pdfWriter.GetObjectsContext().GetInDirectObjectsRegistry().AllocateNewObjectID();
    pdf_glyphs (char_name) = pdf_raw_image (buf, gl->width, gl->height, imageXObjectID);
  }
}


/******************************************************************************
 * Type 3 fonts
 ******************************************************************************/

static int
t3font_font_chunk (int global_glyph) {
  if (global_glyph <= 255) return 0;
  return global_glyph / 255;
}

static int
t3font_get_local_glyph (int global_glyph, int font_chunk, string font_name) {
  int chunk = t3font_font_chunk (global_glyph);
  if (chunk != font_chunk)
    convert_error << "pdf_hummus_renderer, in t3font support, glyph "
		  << as_string (global_glyph) << " out of chunk "
		  << as_string (font_chunk)
		  << " of font " << font_name << LF;
  if (global_glyph <= 255)
    return global_glyph;
  return (global_glyph % 255) + 1;
}

static int
t3font_get_global_glyph (int local_glyph, int font_chunk, string font_name) {
  if (local_glyph > 255)
    convert_error << "pdf_hummus_renderer, in t3font support, local glyph "
		  << as_string (local_glyph)
		  << " is out of the range 0,...,255 "
		  << "in font " << font_name << LF;
  if (font_chunk == 0)
    return local_glyph;
  return 255 * font_chunk + local_glyph - 1;
}

class t3font_rep : public concrete_struct {
public:
  font_glyphs fn;
  int font_chunk;
  ObjectIDType fontId;
  ObjectsContext &objectsContext;
  hashmap<int, int> used_chars;
  int firstchar;
  int lastchar;
  int b0,b1,b2,b3; // glyph bounding box
  bool first_glyph;
  
  t3font_rep (font_glyphs _fn, int _font_chunk,
	      ObjectsContext &_objectsContext)
    : fn (_fn), font_chunk (_font_chunk),
      objectsContext (_objectsContext), first_glyph (true) {
    fontId = objectsContext.GetInDirectObjectsRegistry()
               .AllocateNewObjectID(); }  
  void update_bbox (int llx, int lly, int urx, int ury);
  void add_glyph (int ch) {  used_chars (ch) = 1; }
  void write_char (glyph gl, ObjectIDType inCharID);
  void write_definition (int& registry_id);
};

class t3font {
  CONCRETE_NULL(t3font);
  t3font (font_glyphs _fn, int _font_chunk,
	  ObjectsContext &_objectsContext)
    : rep (tm_new<t3font_rep> (_fn, _font_chunk,_objectsContext)) {};
};

CONCRETE_NULL_CODE(t3font);

void
t3font_rep::update_bbox (int llx, int lly, int urx, int ury) {
  if (first_glyph) {
    b0 = llx; b1 = lly; b2 = urx; b3 = ury;
    first_glyph = false;
  } else {
    if (llx < b0) b0 = llx;
    if (lly < b1) b1 = lly;
    if (urx > b2) b2 = urx;
    if (ury > b3) b3 = ury;
  }
}

void
t3font_rep::write_char (glyph gl, ObjectIDType inCharID) {
  int llx, lly, urx, ury, cwidth, cheight, lwidth;
  llx = -gl->xoff;
  lly = gl->yoff-gl->height+1;
  urx = gl->width-gl->xoff+1;
  ury = gl->yoff+1;
  cwidth = gl->width;
  cheight = gl->height;
  lwidth = gl->lwidth;
  
  objectsContext.StartNewIndirectObject(inCharID);
  // write char stream
  PDFStream *charStream = objectsContext.StartPDFStream(NULL, true);
  string data;
  if (is_nil (gl)) {
    // write d0 command
    data  << "0 0 d0\r\n";
  } else if (cwidth <= 0 || cheight <= 0) {
    // a glyph without ink: its advance only, since an image of width or
    // height 0 is not valid PDF
    data << as_string (lwidth) << " 0 d0\r\n";
  } else {
    update_bbox (llx, lly, urx, ury);
    data << as_string (lwidth) << " 0 ";
    data << as_string (llx) << " " << as_string (lly) << " "
	 << as_string (urx) << " " << as_string (ury) << " d1\r\n";
    data << "q\r\n";
    data  << as_string ((double)(cwidth)) << " 0 0 "
	  << as_string ((double)(cheight)) << " "
	  << as_string ((double)(llx)) << " "
	  << as_string ((double)(lly)) << " cm\r\n";
    data << "BI\r\n/W " << as_string (cwidth)
	 << "\r\n/H " << as_string (cheight) << "\r\n";
    data << "/BPC 1 /F /AHx /D [0.0 1.0] /IM true\r\nID\r\n";
    static const char* hex_string= "0123456789ABCDEF";
    string hex_code;
    int i, j, count= 0, cur= 0;
    for (j= 0; j < cheight; j++)
      for (i= 0; i < ((cwidth+7) & (-8)); i++) {
	cur= cur << 1;
	if ((i < cwidth) && (gl->get_x(i,j) == 0)) cur++;
	count++;
	if (count == 4) {
	  hex_code << hex_string[cur];
	  cur  = 0;
	  count= 0;
	}
      }
    data << hex_code;
    data << ">\r\nEI\r\nQ\r\n"; // ">" is the EOD char for ASCIIHex
  }
  c_string buf (data);
  charStream->GetWriteStream()->Write((unsigned char *)(char*)buf, N(data));
  objectsContext.EndPDFStream(charStream); // It does the EndIndirectObject()
  delete charStream;
}

void
t3font_rep::write_definition (int& registry_id) {
  array <int> glyph_list;
  array<ObjectIDType> charIds;
  // order used glyphs
  iterator<int> it = iterate (used_chars);
  while (it->busy())
    glyph_list << t3font_get_local_glyph (it->next(), font_chunk, fn->res_name);
  merge_sort(glyph_list);
  if (N(glyph_list) > 0) {
    firstchar = glyph_list [0];
    lastchar = glyph_list [N(glyph_list)-1];
  }
  else {
    convert_warning << "pdf_hummus_renderer, unexpected empty t3 font "
      << fn->res_name << LF;
    firstchar = 255;
    lastchar = 0;
  }
  // write glyphs definitions
  for (int i = 0; i < N(glyph_list); ++i) {
    int ch = t3font_get_global_glyph (glyph_list[i],
				      font_chunk, fn->res_name);
    glyph gl = fn->get (ch);
    ObjectIDType temp=
      objectsContext.GetInDirectObjectsRegistry().AllocateNewObjectID();
    charIds << temp;
    write_char (gl, temp);
  }
  ObjectIDType tounicodeId = 0;
  // create font dictionary
  string dict;
  dict << "<<\r\n";
  dict << "\t/Type /Font\r\n";
  dict << "\t/Resources << /ProcSet [/PDF /ImageB] >>\r\n";
  dict << "\t/Subtype /Type3\r\n\t/FontBBox [ "
       << as_string (b0) << " " << as_string (b1) << " "
       << as_string (b2) << " " << as_string (b3) << "]\r\n";
  dict << "\t/FontMatrix [" << as_string (1/100.0) << " 0 0 "
       << as_string (1/100.0) << " 0 0 ]\r\n";
  dict << "\t/FirstChar " << as_string (firstchar)
       << "\r\n\t/LastChar " << as_string (lastchar) << "\r\n";
  dict << "\t/Widths [ ";
  if (N(glyph_list) > 0)
    for (int i = firstchar; i <= lastchar ; ++i) {
      int ch = t3font_get_global_glyph (i, font_chunk, fn->res_name);
      if (used_chars->contains (ch))
	dict << as_string ((double)(fn->get (ch)->lwidth))
	     << " ";
      else dict << "0 ";
    }
  // Write CharProcs
  dict << "]\r\n\t/CharProcs <<\r\n";
  for (int i = 0; i < N(glyph_list); ++i) {
    int ch = glyph_list[i];
    ObjectIDType temp = charIds[i];
    dict << "\t\t/ch" << as_string (ch) << " "
	 << as_string (temp) << " 0 R\r\n";
  }
  if (N(glyph_list) > 0)
    dict << "\t\t/.notdef " << as_string (charIds[0]) << " 0 R\r\n";
  dict << "\t>>\r\n";
  // Encodings
  if (N(glyph_list) > 0) {
    tounicodeId =
      objectsContext.GetInDirectObjectsRegistry().AllocateNewObjectID();
    dict << "\t/ToUnicode " << as_string(tounicodeId) << " 0 R\r\n";
  }
  dict << "\t/Encoding <<\r\n";
  dict << "\t\t/Type /Encoding\r\n";
  dict << "\t\t/Differences [";
  if (N(glyph_list) > 0)
    for (int i = firstchar, previousEncoding = firstchar; i <= lastchar; ++i) {
      int ch = t3font_get_global_glyph (i, font_chunk, fn->res_name);
      if (used_chars->contains (ch)) {
        if (previousEncoding + 1 != i) {
          dict << "\r\n\t\t\t" << as_string(i) << " ";
        }
        dict << "/ch" << as_string(i) << " ";
        previousEncoding = i;
      }
    }
  dict << " ]\r\n\t>>\r\n>>\r\n";
  write_indirect_obj (objectsContext, fontId, dict);
  if (N(glyph_list) == 0)
    return;
  // Cmap
  registry_id++;
  string registry = "TeXmacs-type3-ToUnicode-CMap-" * as_string (registry_id);
  string cmap;
  cmap << "/CIDInit /ProcSet findresource begin 12 dict\r\n"
    << "begin begincmap /CIDSystemInfo <<\r\n"
    << "  /Registry (" << registry
    << ") /Ordering (Custom) /Supplement 0 >>\r\n"
    << "def /CMapName /" << registry << " def /CMapType 2 def\r\n";
  cmap << "1 begincodespacerange <" << as_hexadecimal (firstchar, 2)
       << "> <" << as_hexadecimal (lastchar, 2)
       << "> endcodespacerange\r\n";
#if 1
  cmap << "1 beginbfrange\r\n";
  int ch= t3font_get_global_glyph (firstchar, font_chunk, fn->res_name);
  cmap << "\t<" << as_hexadecimal (firstchar, 2) << "> "
       << "<" << as_hexadecimal (lastchar, 2) << "> "
       << "<" << as_hexadecimal (ch, 4) << ">\r\n";
  cmap << "endbfrange\r\n";
#else
  // Left for testing purpose
  cmap << as_string (N(glyph_list))
       << " beginbfchar\r\n";
  for (int i= 0; i < N(glyph_list); i++) {
    int lch= glyph_list[i];
    int ch= t3font_get_global_glyph (lch, font_chunk, fn->res_name);
    cmap << "\t<" << as_hexadecimal (lch, 2)
	 << "> <" << as_hexadecimal (ch, 4) << ">\r\n";
  }
  cmap << "endbfchar\r\n";
#endif
  cmap << "endcmap CMapName currentdict /CMap defineresource\r\n"
       << "pop end end\r\n";
  objectsContext.StartNewIndirectObject(tounicodeId);
  PDFStream* cmapStream = objectsContext.StartPDFStream (NULL,true);
  OutputStreamTraits outputTraits(cmapStream->GetWriteStream ());
  c_string buf (cmap);
  InputByteArrayStream reader ((IOBasicTypes::Byte*)(char*)buf, N(cmap));
  EStatusCode status = outputTraits.CopyToOutputStream (&reader);
  if (status != PDFHummus::eSuccess) {
    delete cmapStream;
    convert_error << "pdf_hummus_renderer, internal error at line "
		  << as_string (__LINE__) << LF;
  }
  objectsContext.EndPDFStream (cmapStream); // It does EndIndirectObject();
  delete cmapStream;
}

void
pdf_hummus_renderer_rep::flush_fonts()
{
  // flush t3 fonts
  iterator<string> it = iterate(t3font_list);
  while (it->busy()) {
    string name = it->next();
    t3font f = t3font_list[name];
    f->write_definition(t3font_registry_id);
  }
}

/******************************************************************************
 * Text handling
 ******************************************************************************/

static hashset<string>
pdf_font_issues () {
  static const url u ("$TEXMACS_PATH/fonts/pdf-font-issues.scm");
  static bool loaded= false;
  static hashset<string> set;
  if (loaded)
    return set;
  string s;
  if (exists (u) && !load_string (u, s, false)) {
    tree t= block_to_scheme_tree (s);
    for (int i=0; i<N(t); i++)
      if (is_func (t[i], TUPLE) && (N(t[i]) >= 1)) {
        tree key= t[i][0];
	if (is_string (key))
	  set->insert (as_string (key));
      }
  }
  loaded= true;
  return set;
}

static bool
no_font_issues (url u) {
  string h= as_standard_string (tail (u));
  return !pdf_font_issues ()->contains (h);
}



void
pdf_hummus_renderer_rep::make_pdf_font (string fontname)
{
  int pos= search_forwards (":", fontname);
  string fname= (pos==-1? fontname: fontname (0, pos));
  url u = url_none ();
  {
    //debug_convert << " try freetype " << LF;
    u = tt_font_find (fname);
    //debug_convert << fname << " " << u << LF;
  }
  if (!is_none (u)) {
    int pos= search_forwards (".", fontname);
    string rname= (pos==-1? fontname: fontname (0, pos));
    //double fsize= font_size (fn->res_name);
    
    //char *_rname = as_charp(fname);
    PDFUsedFont* font;
    {
      //debug_convert << "GetFontForFile "  << u  << LF;
      c_string _u (concretize (u));
      font = pdfWriter.GetFontForFile((char*)_u);
    }
    
    if (font != NULL && no_font_issues (u)) {
      native_fonts (fontname)= font;
      std::string _ps_name= font->GetFreeTypeFont()->GetPostscriptName();
      string ps_name (_ps_name.c_str ());
      if (starts (ps_name, "EuropeanComputerModern"))
	EuropeanComputerModern_fonts->insert (fontname);
      if (is_t2a_font_file (u))
	T2A_fonts->insert (fontname);
      return;
    }
    else {
      convert_warning << "pdf_hummus_renderer, font: " << fname
		      << " in file " << u << " cannot be loaded. "
		      << "It is converted to bitmap type 3 font." << LF;
    }
  }
  not_native_fonts->insert (fontname);
}

static double
font_size (string name) {
  int pos= search_backwards (".", name);
  int szpos= pos-1;
  while ((szpos>0) && is_numeric (name[szpos-1])) szpos--;
  double size= as_double (name (szpos, pos));
  if (size == 0) size= 10;
  int end= pos+1;
  while (end < N(name) && is_numeric (name[end])) end++;
  double dpi= as_double (name (pos+1, end));
  double mag= (size) * (dpi/72.0);
  return mag;
}

static bool
match_font_base_name (string fontname, string basename) {
  if (!starts (fontname, basename))
    return false;
  int i= N(basename);
  while (i < N(fontname)) {
    if (fontname[i] == ':') break;
    if (fontname[i] < '0' || fontname[i] > '9')
      return false;
    i++;
  }
  return true;
}

static bool
requires_hack_notdef_for_tex_font (string fontname) {
  // This fix is necessary for avoiding bugs in certain Pdf viewers,
  // such as old versions of Preview under MacOS (<= 10.6.*).
  // It is expected to be removed in near future.
  static hashmap<string,bool> remember;
  if (remember->contains(fontname))
    return remember[fontname];
  bool r=
    match_font_base_name (fontname, "cmbsy") ||
    match_font_base_name (fontname, "cmmib") ||
    match_font_base_name (fontname, "cmb") ||
    match_font_base_name (fontname, "cmbx") ||
    match_font_base_name (fontname, "cmbxsl") ||
    match_font_base_name (fontname, "cmbxti") ||
    match_font_base_name (fontname, "cmbcsc") ||
    match_font_base_name (fontname, "cmdunh") ||
    match_font_base_name (fontname, "cmex") ||
    match_font_base_name (fontname, "cmexb") ||
    match_font_base_name (fontname, "cmff") ||
    match_font_base_name (fontname, "cmfi") ||
    match_font_base_name (fontname, "cmfib") ||
    match_font_base_name (fontname, "cminch") ||
    match_font_base_name (fontname, "cmitt") ||
    match_font_base_name (fontname, "cmmi") ||
    match_font_base_name (fontname, "cmmib") ||
    match_font_base_name (fontname, "cmr") ||
    match_font_base_name (fontname, "cmsl") ||
    match_font_base_name (fontname, "cmsltt") ||
    match_font_base_name (fontname, "cmss") ||
    match_font_base_name (fontname, "cmssbx") ||
    match_font_base_name (fontname, "cmssdc") ||
    match_font_base_name (fontname, "cmssi") ||
    match_font_base_name (fontname, "cmssq") ||
    match_font_base_name (fontname, "cmssqi") ||
    match_font_base_name (fontname, "cmsy") ||
    match_font_base_name (fontname, "cmtcsc") ||
    match_font_base_name (fontname, "cmtex") ||
    match_font_base_name (fontname, "cmti") ||
    match_font_base_name (fontname, "cmtt") ||
    match_font_base_name (fontname, "cmu") ||
    match_font_base_name (fontname, "cmvtt") ||
    match_font_base_name (fontname, "euex") ||
    match_font_base_name (fontname, "eufb") ||
    match_font_base_name (fontname, "eufm") ||
    match_font_base_name (fontname, "eurb") ||
    match_font_base_name (fontname, "eurm") ||
    match_font_base_name (fontname, "eusb") ||
    match_font_base_name (fontname, "eusm") ||
    match_font_base_name (fontname, "msam") ||
    match_font_base_name (fontname, "msbm");
  remember(fontname)= r;
  return r;
}
     
void
pdf_hummus_renderer_rep::draw (int ch, font_glyphs fn, SI x, SI y) {
  //debug_convert << "draw \"" << (char)ch << "\" " << ch << " "
  //		<< fn->res_name << "\n";
  glyph gl= fn->get(ch);
  if (is_nil (gl)) return;
  tag_mark (0);
  string fontname = fn->res_name;
  int fontchunk= t3font_font_chunk (ch);
  string fontchunkname= fontname * string ("-chunk") * as_string (fontchunk);

  if (ch == 0 && requires_hack_notdef_for_tex_font (fontname)) {
    draw (161, fn, x, y);
    return;
  }
  string char_name (fontname * "-" * as_string (ch));
  pdf_raw_image glyph;
  
  if (cfn != fontname && cfn != fontchunkname) {
    if (!native_fonts->contains (fontname) &&
	!not_native_fonts->contains (fontname))
      make_pdf_font (fontname);
    if (not_native_fonts->contains (fontname)) {
      fontname= fontchunkname;
      if (!t3font_list->contains (fontname)) {
	//cout << "create t3font for chunk " << fontchunk
	//     << " of " << fn->res_name << LF;
	t3font f (fn, fontchunk, pdfWriter.GetObjectsContext());
	t3font_list (fontchunkname) = f;
      }
    }
    //debug_convert << "CHANGE FONT" << LF;
    begin_text ();
    fsize = font_size (fontname);
    if (native_fonts->contains (fontname)) {
      cfid = native_fonts (fontname);
      contentContext->Tf (cfid, fsize);
    } else {
      cfid = NULL;
      std::string name = page->GetResourcesDictionary()
	.AddFontMapping (t3font_list (fontname)->fontId);
      // pk fonts are encoded in t3 fonts as bitmaps.
      // they cannot be scaled and are encoded in such a way that
      // they should be rendered at size 100 (conventional value)
      // to give the correct result (see the Font Matrix defined
      // in t3font_rep::write_definition).
      contentContext->TfLow (name, 100);
    }
    cfn = fontname;
  }
  else
    cfid= native_fonts (fontname);
  begin_text ();
  contentContext->Td (to_x(x) - prev_text_x, to_y(y) - prev_text_y);
  prev_text_x = to_x(x);
  prev_text_y = to_y(y);
  //debug_convert << "char " << ch << "index " << gl->index
  //              << " " << x << " " << y << " font " << cfn  << LF;
  if (cfid == NULL)
    t3font_list(cfn)->add_glyph (ch);
  GlyphUnicodeMappingList glyphs;
  if (cfid != NULL &&
      EuropeanComputerModern_fonts->contains (cfn) &&
      gl->index >= 27 && gl->index <= 31) {
    ch += 0xfb00 - 27;
  }
  int gl_index;
  if (cfid != NULL)
    gl_index= gl->index;
  else
    gl_index= t3font_get_local_glyph
      (ch, t3font_list(cfn)->font_chunk,
       t3font_list(cfn)->fn->res_name);
  // a glyph drawn by its number stands for the character the font
  // recorded for it, or for none (the further pieces of an assembly)
  bool by_number= (ch >= 0xc000000);
  int  code= by_number?
    native_glyph_get_unicode (fn->res_name, ch - 0xc000000): ch;
  static const std::string ligature_ff= "/Span << /ActualText (ff) >> BDC ";
  static const std::string ligature_fi= "/Span << /ActualText (fi) >> BDC ";
  static const std::string ligature_fl= "/Span << /ActualText (fl) >> BDC ";
  static const std::string ligature_ffi= "/Span << /ActualText (ffi) >> BDC ";
  static const std::string ligature_ffl= "/Span << /ActualText (ffl) >> BDC ";
  if (ch >= 0xfb00 && ch <= 0xfb04 && ePDFVersion >= ePDFVersion15) {
    if (ch == 0xfb00) contentContext->WriteFreeCode (ligature_ff);
    if (ch == 0xfb01) contentContext->WriteFreeCode (ligature_fi);
    if (ch == 0xfb02) contentContext->WriteFreeCode (ligature_fl);
    if (ch == 0xfb03) contentContext->WriteFreeCode (ligature_ffi);
    if (ch == 0xfb04) contentContext->WriteFreeCode (ligature_ffl);
    if (cfid != NULL) {
      glyphs.push_back(GlyphUnicodeMapping(gl_index, ch));
      contentContext->Tj (glyphs);
    }
    else {
      std::string buf; buf.push_back (gl_index);
      contentContext->TjLow (buf);
    }
    contentContext->WriteFreeCode (" EMC\r\n");
  }
  else {
    if (cfid != NULL) {
      if (by_number && code <= 0 && tagger != NULL)
        // a tagged PDF maps every glyph drawn to a character (PDF/UA): those
        // of no character known (#943) to that of the name of the glyph,
        // or which the font maps to it, as readers read them; else U+FFFD
        glyphs.push_back (GlyphUnicodeMapping (gl_index,
                            hummus_glyph_unicode (cfid, gl_index)));
      else if (by_number && code <= 0)
        glyphs.push_back (GlyphUnicodeMapping (gl_index, ULongVector ()));
      else {
        int uc= code;
        if (EuropeanComputerModern_fonts->contains (cfn))
          uc= tex_glyph_unicode (ch, false);
        else if (T2A_fonts->contains (cfn))
          uc= tex_glyph_unicode (ch, true);
        else if (!by_number) {
          array<int> table= tex_math_unicode (cfn);
          if (ch >= 0 && ch < N(table) && table[ch] >= 0) uc= table[ch];
          // a tagged PDF: a code of no character in the table (a font of
          // none: stmary, wasy), the character of its glyph, as a reader
          // reads it (a code below U+0020 is a control otherwise)
          else if (tagger != NULL) {
            // (code 161 is the glyph of code 0, requires_hack_notdef)
            int g= hummus_glyph_unicode (cfid, ch == 161? 0: gl_index);
            if (g != 0xfffd) uc= g;
            // (a font of a table: its code is no character, which a
            // reader would take for one, as MuPDF does)
            else if (ch == 161 || N(table) > 0) uc= 0xfffd;
          }
        }
        // (PDF/UA forbids 0, U+FEFF and U+FFFE in the map, and the
        // controls are no characters)
        if (tagger != NULL && (uc < 0x20 || uc == 0xfeff || uc == 0xfffe))
          uc= 0xfffd;
        glyphs.push_back (GlyphUnicodeMapping (gl_index, uc));
      }
      contentContext->Tj(glyphs);
    }
    else {
      std::string buf; buf.push_back (gl_index);
      contentContext->TjLow (buf);
    }
  }    
}

/******************************************************************************
 * Graphics primitives
 ******************************************************************************/

void
pdf_hummus_renderer_rep::line (SI x1, SI y1, SI x2, SI y2) {
  tag_mark (1);
  // debug_convert << "line\n";
  end_text ();
  contentContext->m(to_x (x1), to_y (y1));
  contentContext->l(to_x (x2), to_y (y2));
  contentContext->S();
}

void
pdf_hummus_renderer_rep::lines (array<SI> x, array<SI> y) {
  tag_mark (1);
  // debug_convert << "lines\n";
  end_text ();
  int i, n= N(x);
  if ((N(y) != n) || (n<1)) return;
  end_text ();
  contentContext->q();
  if (pen->get_cap () == cap_round ||
      (x[N(x)-1] == x[0] && y[N(y)-1] == y[0]))
    contentContext->J(1); // round cap
  else
    contentContext->J(2); // square cap
  contentContext->j(1); // round join
  contentContext->m(to_x (x[0]), to_y (y[0]));
  for (i=1; i<n; i++) {
    contentContext->l(to_x (x[i]), to_y (y[i]));
  }
  contentContext->S();
  contentContext->Q();
}

void
pdf_hummus_renderer_rep::clear (SI x1, SI y1, SI x2, SI y2) {
  tag_mark (1);
  end_text ();
  double xx1= to_x (min (x1, x2));
  double yy1= to_y (min (y1, y2));
  double xx2= to_x (max (x1, x2));
  double yy2= to_y (max (y1, y2));
  // debug_convert << "clear" << xx1 << " " << yy1 << " " << xx2 << " " << yy2 << LF;
  contentContext->q();
  select_fill_color (bg);
  select_fill_pattern (bgb);
  contentContext->re(xx1, yy1, xx2-xx1, yy2-yy1);
  contentContext->h();
  contentContext->f();
  select_fill_color (fg);
  select_fill_pattern (fgb);
  contentContext->Q();
}

void
pdf_hummus_renderer_rep::fill (SI x1, SI y1, SI x2, SI y2) {
  tag_mark (1);
  if ((x1<x2) && (y1<y2)) {
    end_text ();
    double xx1= to_x (min (x1, x2));
    double yy1= to_y (min (y1, y2));
    double xx2= to_x (max (x1, x2));
    double yy2= to_y (max (y1, y2));
    contentContext->re(xx1, yy1, xx2-xx1, yy2-yy1);
    contentContext->h();
    contentContext->f(); // FIXME Winding
  }
}

void
pdf_hummus_renderer_rep::bezier_arc (SI x1, SI y1, SI x2, SI y2, int alpha, int delta, bool filled)
{
  tag_mark (1);
  // PDF can describe only cubic bezier paths, so we have to make up the arc with them
  // Since this is not mathematically exact, we minimize errors by drawing beziers sub-arcs of at most 90??
  end_text ();
  contentContext->q(); // save graphics state

    double xx1 = to_x(x1), yy1 = to_y(y1), xx2 = to_x(x2), yy2 = to_y(y2);
    double cx = (xx1+xx2)/2, cy = (yy1+yy2)/2;
    double rx = (xx2-xx1)/2, ry = (yy2-yy1)/2;
    contentContext->cm(1, 0, 0, 1, cx, cy); // centering
	//we can't apply scale here because in pdf the pen is scaled too

	int i=1+abs(delta)/(90*64); //number of sub-arcs needed 
	if ((abs(delta)%(90*64))==0) i-- ; //correction needed if exact multiple of 90??
	double phi= 2.0*M_PI*(delta)/(i*360.0*64.0); //angular span of each sub-arc
	double a = 2.0*M_PI*(alpha)/(360.0*64.0); //start angle in radians

  // Control points for an arc of radius 1, centered on the x-axis and spanning phi degrees
  // from: http://www.tinaja.com/glib/bezcirc2.pdf
    double sphi = sin(phi/2),	cphi = cos(phi/2);
    double bx0 = cphi,			by0 = -sphi;
    double bx1 = (4.0-bx0)/3.0,	by1 = (1.0-bx0)*(3.0-bx0)/(3.0*by0);
    double bx2 = bx1,			by2 = -by1;
    double bx3 = bx0,			by3 = -by0;
	
	// repeatedly draw rotated and scaled sub-arc
	// cannot use user-space transformations with cm util path is painted (otherwise path is lost)
	// so we perform explicit rotation+scaling calculations
	int k;
	for (k=0; k<i;k++) {
	sphi = sin(phi*(k+0.5)+a); 
    cphi = cos(phi*(k+0.5)+a);
	if (k==0) contentContext->m(rx*(bx0*cphi-by0*sphi),ry* (+bx0*sphi+by0*cphi)); //start point
	contentContext->c(
		rx*(bx1*cphi-by1*sphi), ry*(+bx1*sphi+by1*cphi),
		rx*(bx2*cphi-by2*sphi), ry*(+bx2*sphi+by2*cphi),
		rx*(bx3*cphi-by3*sphi), ry*(+bx3*sphi+by3*cphi));
	}
	
	//paint
    if (filled) {
		// contentContext->l(0.0, 0.0); // for a filled "pie" with vertex at the center
		contentContext->f();
	}	
	else  ( (abs(delta) == 360*64) ? contentContext->s() : contentContext->S()); 
	// here we close the path if it's a full circle 
  contentContext->Q(); // restore the graphics state (undoes centering only)
}

void
pdf_hummus_renderer_rep::arc (SI x1, SI y1, SI x2, SI y2, int alpha, int delta) {
  // debug_convert << "arc\n";
  end_text ();
  bezier_arc(x1, y1, x2, y2, alpha, delta, false);
}

void
pdf_hummus_renderer_rep::fill_arc (SI x1, SI y1, SI x2, SI y2, int alpha, int delta) {
  // debug_convert << "fill_arc\n";
  end_text ();
  bezier_arc(x1, y1, x2, y2, alpha, delta, true);
}

void
pdf_hummus_renderer_rep::polygon (array<SI> x, array<SI> y, bool convex) {
  tag_mark (1);
  // debug_convert << "polygon\n";
  int i, n= N(x);
  if ((N(y) != n) || (n<1)) return;
  end_text ();
  contentContext->m(to_x (x[0]), to_y (y[0]));
  for (i=1; i<n; i++)
    contentContext->l(to_x (x[i]), to_y (y[i]));
  contentContext->h();
  if (convex)
    contentContext->f(); // odd-even
  else
    contentContext->fStar(); // nonzero winding
}

/******************************************************************************
* The fonts which a PDF picture names only (docs/pdf-tagging.md): in a
* tagged PDF, a copy of the picture embeds a font which TeXmacs ships
* instead (pdf_font_subst), written by an incremental update of PDFHummus
******************************************************************************/

static std::string
as_charp_std (string s) {
  return std::string (N(s) == 0? "": &s[0], (size_t) N(s));
}

static string
hummus_name (PDFParser& p, PDFDictionary* d, const char* key) {
  PDFObjectCastPtr<PDFName> n (p.QueryDictionaryObject (d, key));
  return (!n? string (""): string (n->GetValue ().c_str ()));
}

// whether the object of the reference v is met for the first time (direct
// objects are: they make no cycles)
static bool
hummus_first_visit (PDFObject* v, hashset<int>& visited) {
  if (v == NULL || v->GetType () != PDFObject::ePDFObjectIndirectObjectReference)
    return true;
  int id= (int) ((PDFIndirectObjectReference*) v)->mObjectID;
  if (visited->contains (id)) return false;
  visited->insert (id);
  return true;
}

static void
hummus_font_dicts (PDFParser& p, PDFDictionary* res,
                   array<int>& ids, int depth, hashset<int>& visited) {
  if (res == NULL || depth > 8) return;
  PDFObjectCastPtr<PDFDictionary> fd (p.QueryDictionaryObject (res, "Font"));
  if (!!fd) {
    MapIterator<PDFNameToPDFObjectMap> it= fd->GetIterator ();
    while (it.MoveNext ()) {
      PDFObject* v= it.GetValue ();
      // (the fonts of the glyphs of a Type 3 font)
      if (!hummus_first_visit (v, visited)) continue;
      PDFObjectCastPtr<PDFDictionary> f3 (p.QueryDictionaryObject (fd.GetPtr (),
                                            it.GetKey ()->GetValue ()));
      if (!!f3 && hummus_name (p, f3.GetPtr (), "Subtype") == "Type3") {
        PDFObjectCastPtr<PDFDictionary> r3 (p.QueryDictionaryObject (f3.GetPtr (),
                                                                     "Resources"));
        if (!!r3) hummus_font_dicts (p, r3.GetPtr (), ids, depth + 1, visited);
        continue;
      }
      if (v->GetType () != PDFObject::ePDFObjectIndirectObjectReference) {
        convert_warning << "a font of a picture is not an object of its own, "
                        << "it is left as it is" << LF;
        continue;
      }
      int id= (int) ((PDFIndirectObjectReference*) v)->mObjectID;
      bool dup= false;
      for (int i=0; i<N(ids); i++) if (ids[i] == id) dup= true;
      if (!dup) ids << id;
    }
  }
  // the resources of the tiling patterns
  PDFObjectCastPtr<PDFDictionary> pd (p.QueryDictionaryObject (res, "Pattern"));
  if (!!pd) {
    MapIterator<PDFNameToPDFObjectMap> it= pd->GetIterator ();
    while (it.MoveNext ()) {
      if (!hummus_first_visit (it.GetValue (), visited)) continue;
      PDFObjectCastPtr<PDFStreamInput> x (p.QueryDictionaryObject (pd.GetPtr (),
                                            it.GetKey ()->GetValue ()));
      if (!x) continue;
      PDFObjectCastPtr<PDFDictionary> xdict (x->QueryStreamDictionary ());
      if (!xdict) continue;
      PDFObjectCastPtr<PDFDictionary> xr (p.QueryDictionaryObject (xdict.GetPtr (),
                                                                   "Resources"));
      if (!!xr) hummus_font_dicts (p, xr.GetPtr (), ids, depth + 1, visited);
    }
  }
  PDFObjectCastPtr<PDFDictionary> xd (p.QueryDictionaryObject (res, "XObject"));
  if (!xd) return;
  MapIterator<PDFNameToPDFObjectMap> it= xd->GetIterator ();
  while (it.MoveNext ()) {
    if (!hummus_first_visit (it.GetValue (), visited)) continue;
    PDFObjectCastPtr<PDFStreamInput> x (p.QueryDictionaryObject (xd.GetPtr (),
                                          it.GetKey ()->GetValue ()));
    if (!x) continue;
    PDFObjectCastPtr<PDFDictionary> xdict (x->QueryStreamDictionary ());
    if (!xdict || hummus_name (p, xdict.GetPtr (), "Subtype") != "Form")
      continue;
    PDFObjectCastPtr<PDFDictionary> xr (p.QueryDictionaryObject (xdict.GetPtr (),
                                                                 "Resources"));
    if (!!xr) hummus_font_dicts (p, xr.GetPtr (), ids, depth + 1, visited);
  }
}

static ObjectIDType
hummus_write_stream (ObjectsContext& oc, string data, string subtype) {
  ObjectIDType id= oc.StartNewIndirectObject ();
  DictionaryContext* d= oc.StartDictionary ();
  if (N(subtype) > 0) {
    d->WriteKey ("Subtype");
    d->WriteNameValue (as_charp_std (subtype));
  }
  PDFStream* st= oc.StartPDFStream (d);
  st->GetWriteStream ()->Write ((const IOBasicTypes::Byte*) &data[0], N(data));
  oc.EndPDFStream (st);
  delete st;
  return id;
}

// the forms of resources, and those of their resources, by their objects
static void
hummus_form_ids (PDFParser& p, PDFDictionary* res, array<int>& ids,
                 int depth) {
  if (res == NULL || depth > 8) return;
  // the forms, and the tiling patterns, which have content too
  for (int pass=0; pass<2; pass++) {
  PDFObjectCastPtr<PDFDictionary> xd (p.QueryDictionaryObject (res,
                                        pass == 0? "XObject": "Pattern"));
  if (!xd) continue;
  MapIterator<PDFNameToPDFObjectMap> it= xd->GetIterator ();
  while (it.MoveNext ()) {
    PDFObject* v= it.GetValue ();
    if (v->GetType () != PDFObject::ePDFObjectIndirectObjectReference) continue;
    int id= (int) ((PDFIndirectObjectReference*) v)->mObjectID;
    bool dup= false;
    for (int i=0; i<N(ids); i++) if (ids[i] == id) dup= true;
    if (dup) continue;
    PDFObjectCastPtr<PDFStreamInput> x (p.ParseNewObject (id));
    if (!x) continue;
    PDFObjectCastPtr<PDFDictionary> xdict (x->QueryStreamDictionary ());
    if (!xdict || (pass == 0 &&
                   hummus_name (p, xdict.GetPtr (), "Subtype") != "Form"))
      continue;
    ids << id;
    PDFObjectCastPtr<PDFDictionary> xr (p.QueryDictionaryObject (xdict.GetPtr (),
                                                                 "Resources"));
    if (!!xr) hummus_form_ids (p, xr.GetPtr (), ids, depth + 1);
  }
  }
}

// The character of the glyph gid of a font: of its name, or the one which
// the font maps to it; U+FFFD when none
static bool
hummus_usable_unicode (int u) {
  // (not a control, not U+FFFD, not of a private use area)
  return u >= 0x20 && u != 0xfffd && !(u >= 0xe000 && u <= 0xf8ff) &&
         u < 0xf0000;
}

// a character of a compatibility block (the same as another one)
static bool
hummus_compatibility (int u) {
  return (u >= 0x2e80 && u <= 0x2fdf) || (u >= 0xf900 && u <= 0xfaff) ||
         (u >= 0x2f800 && u <= 0x2fa1f) || u == 0xb5 || u == 0x2126 ||
         u == 0x212a || u == 0x212b;
}

static int
hummus_glyph_unicode (PDFUsedFont* f, unsigned int glyph) {
  FreeTypeFaceWrapper* w= (f == NULL? NULL: f->GetFreeTypeFont ());
  if (w == NULL) return 0xfffd;
  FT_Face face= *w;
  if (face == NULL) return 0xfffd;
  // the glyph of FreeType (a Type 1 font of an encoding of its own, a TeX
  // font: PDFHummus numbers its glyphs by their codes)
  unsigned int gid= w->GetGlyphIndexInFreeTypeIndexes (glyph);
  // (a piece is no character: mapsto of cmsy is the bar of the arrows
  // which it begins, and varshortmid)
  if (FT_HAS_GLYPH_NAMES (face)) {
    char pb[32];
    pb[0]= '\0';
    if (!FT_Get_Glyph_Name (face, gid, pb, sizeof (pb)) &&
        strcmp (pb, "mapsto") == 0) return 0xfffd;
  }
  // the characters which the font maps to its glyphs, once for each font
  // (by its face, its names and its number of glyphs, a face being freed
  // and its address used again); the first character of a glyph which is
  // not of a private use area
  static std::map<std::string, std::map<unsigned int, int> > by_face;
  char kb[64];
  snprintf (kb, sizeof (kb), "%p:%ld:", (void*) face, (long) face->num_glyphs);
  std::string key= std::string (kb) +
    (face->family_name? face->family_name: "") + ":" +
    (face->style_name? face->style_name: "");
  if (by_face.find (key) == by_face.end ()) {
    std::map<unsigned int, int>& m= by_face[key];
    // (a Type 1 font: the map which FreeType makes of its glyph names,
    // the aNN of ZapfDingbats included, says nothing: its names say it)
    bool unicode= (face->charmap != NULL && FT_IS_SFNT (face) &&
                   face->charmap->encoding == FT_ENCODING_UNICODE);
    if (unicode) {
      FT_UInt g;
      FT_ULong c= FT_Get_First_Char (face, &g);
      while (g != 0) {
        // (a character of a compatibility block, a Kangxi radical, gives
        // way to the one of the same glyph which is not: 文, not ⽂)
        if (hummus_usable_unicode ((int) c)) {
          std::map<unsigned int, int>::iterator e= m.find (g);
          if (e == m.end ()) m[g]= (int) c;
          else if (hummus_compatibility (e->second) &&
                   !hummus_compatibility ((int) c)) e->second= (int) c;
        }
        c= FT_Get_Next_Char (face, c, &g);
      }
    }
  }
  std::map<unsigned int, int>& m= by_face[key];
  // the character the font maps to the glyph, else that of its name, else
  // (a variant: divides.v1) the one the font maps to the glyph of the name
  // before the dot
  std::map<unsigned int, int>::iterator it= m.find (gid);
  if (it != m.end ()) return it->second;
  if (FT_HAS_GLYPH_NAMES (face)) {
    char buf[128];
    buf[0]= '\0';
    if (!FT_Get_Glyph_Name (face, gid, buf, sizeof (buf)) && buf[0] != '\0') {
      string nm (buf);
      int u= pdf_glyph_name_unicode (nm);
      if (hummus_usable_unicode (u)) return u;
      int dot= search_forwards (".", nm);
      if (dot > 0) {
        c_string base (nm (0, dot));
        FT_UInt g= FT_Get_Name_Index (face, (FT_String*) (char*) base);
        if (g != 0) {
          it= m.find (g);
          if (it != m.end ()) return it->second;
        }
      }
    }
  }
  return 0xfffd;
}

// Whether PDFHummus reads the PDF file u as it is: it parses, and is not
// encrypted (its incremental update and its parser of the structure do
// not decrypt: an encrypted picture made the export crash)
// a name of a PDF file, its #xx escapes decoded
static string
hummus_name_decoded (string s, int i, int j) {
  string r;
  for (int k=i; k<j; k++)
    if (s[k] == '#' && k+2 < j) {
      r << (char) from_hexadecimal (s (k+1, k+3));
      k += 2;
    }
    else r << s[k];
  return r;
}

bool
hummus_pdf_plain (url u) {
  // (by its bytes: the parser of PDFHummus damages its memory on an
  // encrypted file) The key Encrypt of a trailer, or of the dictionary of
  // a cross-reference stream, in their top level: names decoded, strings,
  // comments and the data of the streams skipped
  string s;
  if (load_string (u, s, false)) return false;
  int h= search_forwards ("%PDF-", s);
  if (h < 0 || h > 1024) return false;
  int n= N(s), i= h, depth= 0;
  bool expect_key= false, trailer= false, after_trailer= false;
  bool has_encrypt= false, xref= false;
  string key, last_word;
  while (i < n) {
    char c= s[i];
    if (c == ' ' || c == '\n' || c == '\r' || c == '\t' || c == '\f' ||
        c == '\0') { i++; continue; }
    if (c == '%') { while (i < n && s[i] != '\n' && s[i] != '\r') i++; continue; }
    if (c == '(') {
      int level= 0;
      for (; i < n; i++) {
        if (s[i] == '\\') { i++; continue; }
        if (s[i] == '(') level++;
        else if (s[i] == ')' && --level == 0) { i++; break; }
      }
      if (depth == 1 && !expect_key) expect_key= true;
      continue;
    }
    if (c == '<' && i+1 < n && s[i+1] == '<') {
      if (depth == 0) {
        trailer= (last_word == "trailer");
        has_encrypt= false; xref= false; expect_key= true; key= "";
      }
      depth++; i += 2; continue;
    }
    if (c == '>' && i+1 < n && s[i+1] == '>') {
      depth--; i += 2;
      if (depth == 0 && has_encrypt && (trailer || xref)) return false;
      if (depth == 1) expect_key= true;
      if (depth < 0) depth= 0;
      continue;
    }
    if (c == '<') {   // a hex string
      while (i < n && s[i] != '>') i++;
      i++;
      if (depth == 1) expect_key= true;
      continue;
    }
    if (c == '[') { depth++; i++; continue; }
    if (c == ']') { depth--; i++; if (depth == 1) expect_key= true;
                    if (depth < 0) depth= 0; continue; }
    int start= i;
    i++;
    while (i < n && !(s[i] == ' ' || s[i] == '\n' || s[i] == '\r' ||
                      s[i] == '\t' || s[i] == '\f' || s[i] == '\0' ||
                      s[i] == '(' || s[i] == ')' || s[i] == '<' ||
                      s[i] == '>' || s[i] == '[' || s[i] == ']' ||
                      s[i] == '{' || s[i] == '}' || s[i] == '/' ||
                      s[i] == '%')) i++;
    if (c == '/') {
      string nm= hummus_name_decoded (s, start + 1, i);
      if (depth == 1) {
        if (expect_key) {
          key= nm; expect_key= false;
          if (nm == "Encrypt") has_encrypt= true;
        }
        else {
          if (key == "Type" && nm == "XRef") xref= true;
          expect_key= true;
        }
      }
      continue;
    }
    string w= s (start, i);
    if (depth == 0) {
      last_word= w;
      // (the data of a stream: up to endstream)
      if (w == "stream") {
        int e= search_forwards ("endstream", i, s);
        if (e < 0) return false;   // (damaged: not read)
        i= e + 9;
      }
    }
    else if (depth == 1 && !expect_key) {
      // (a reference "n 0 R" is three words: the value ends at R)
      if (w == "R" || !(N(w) > 0 && is_digit (w[0]))) expect_key= true;
      else {
        int j= i;
        while (j < n && (s[j] == ' ' || s[j] == '\n' || s[j] == '\r')) j++;
        if (!(j < n && is_digit (s[j]))) expect_key= true;
      }
    }
  }
  (void) after_trailer;
  return true;
}

// the MCIDs of the names of the properties of the resources res
static hashmap<string,int>
hummus_props (PDFParser& p, PDFDictionary* res) {
  hashmap<string,int> r (-1);
  if (res == NULL) return r;
  PDFObjectCastPtr<PDFDictionary> pr (p.QueryDictionaryObject (res, "Properties"));
  if (!pr) return r;
  MapIterator<PDFNameToPDFObjectMap> it= pr->GetIterator ();
  while (it.MoveNext ()) {
    std::string k= it.GetKey ()->GetValue ();
    PDFObjectCastPtr<PDFDictionary> d (p.QueryDictionaryObject (pr.GetPtr (), k));
    if (!d) continue;
    PDFObjectCastPtr<PDFInteger> m (p.QueryDictionaryObject (d.GetPtr (), "MCID"));
    if (!m) continue;
    r ("/" * string (k.c_str (), (int) k.size ()))= (int) m->GetValue ();
  }
  return r;
}

// the resources of the first page of a document
static PDFDictionary*
hummus_page_resources (PDFParser& p, PDFDictionary* page) {
  PDFObjectCastPtr<PDFDictionary> res (p.QueryDictionaryObject (page, "Resources"));
  if (!res) {
    PDFObjectCastPtr<PDFDictionary> parent (p.QueryDictionaryObject (page, "Parent"));
    if (!!parent) res= p.QueryDictionaryObject (parent.GetPtr (), "Resources");
  }
  PDFDictionary* r= res.GetPtr ();
  if (r != NULL) r->AddRef ();
  return r;
}

// the decoded content of the stream id, and its dictionary
static bool
hummus_read_stream (PDFParser& p, int id, string& s,
                    RefCountPtr<PDFDictionary>& d) {
  PDFObjectCastPtr<PDFStreamInput> st (p.ParseNewObject (id));
  if (!st) return false;
  d= st->QueryStreamDictionary ();
  if (!d) return false;
  IByteReader* rd= p.StartReadingFromStream (st.GetPtr ());
  if (rd == NULL) return false;
  IOBasicTypes::Byte buf[4096];
  while (rd->NotEnded ()) {
    IOBasicTypes::LongBufferSizeType k= rd->Read (buf, sizeof (buf));
    if (k == 0) break;
    for (IOBasicTypes::LongBufferSizeType j=0; j<k; j++) s << (char) buf[j];
  }
  delete rd;
  return true;
}

// the stream id written again with the content t, and its dictionary but
// its places in a structure tree
static void
hummus_write_content (ObjectsContext& oc, PDFDocumentCopyingContext* cc,
                      int id, RefCountPtr<PDFDictionary>& d, string t) {
  oc.StartModifiedIndirectObject (id);
  DictionaryContext* nd= oc.StartDictionary ();
  MapIterator<PDFNameToPDFObjectMap> it= d->GetIterator ();
  while (it.MoveNext ()) {
    std::string k= it.GetKey ()->GetValue ();
    if (k == "Length" || k == "Filter" || k == "DecodeParms" ||
        k == "StructParents" || k == "StructParent") continue;
    nd->WriteKey (k);
    cc->CopyDirectObjectAsIs (it.GetValue ());
  }
  PDFStream* ps= oc.StartPDFStream (nd);
  if (N(t) > 0)
    ps->GetWriteStream ()->Write ((const IOBasicTypes::Byte*) &t[0], N(t));
  oc.EndPDFStream (ps);
  delete ps;
}

// The streams ids of a picture (those of a page are one content: a token
// may end one and the next begin the other) without its marked content
// and its places in a structure tree (pdf_strip_marked_content), or for a
// grafted picture, keep, with its own marked content
// (pdf_graft_marked_content); written again when there were some
static bool
hummus_strip_streams (PDFParser& p, ObjectsContext& oc,
                      PDFDocumentCopyingContext* cc, array<int> ids,
                      hashset<int>* keep, hashmap<string,int> props) {
  string s;
  array<RefCountPtr<PDFDictionary> > ds;
  bool structured= false;
  for (int k=0; k<N(ids); k++) {
    RefCountPtr<PDFDictionary> d;
    if (!hummus_read_stream (p, ids[k], s, d)) return false;
    s << "\n";
    if (d->Exists ("StructParents") || d->Exists ("StructParent"))
      structured= true;
    ds << d;
  }
  bool changed= false;
  array<bool> open;
  bool ok= true;
  string t= (keep != NULL? pdf_graft_marked_content (s, *keep, props,
                                                     changed, ok):
                           pdf_strip_marked_content (s, open, changed));
  if (!changed && !structured) return false;
  for (int k=0; k<N(ids); k++)
    hummus_write_content (oc, cc, ids[k], ds[k], k == 0? t: string (""));
  return true;
}

// Writes a copy of the picture in to out, with fonts in place of those it
// names only, and without its tags (a picture which is a tagged PDF); false
// when there was nothing to change or when it failed
static bool
hummus_embed_picture_fonts (url in, url out, hashset<int>* keep) {
  if (!hummus_pdf_plain (in)) return false;
  PDFWriter w;
  string sin= concretize (in), sout= concretize (out);
  if (w.ModifyPDF (std::string (&sin[0], N(sin)), ePDFVersion17,
                   std::string (&sout[0], N(sout))) != PDFHummus::eSuccess)
    return false;
  PDFParser& p= w.GetModifiedFileParser ();
  ObjectsContext& oc= w.GetObjectsContext ();
  array<int> ids, contents, forms;
  hashmap<string,int> props (-1);
  {
    RefCountPtr<PDFDictionary> page (p.ParsePage (0));
    if (!page) { w.EndPDF (); return false; }
    PDFObjectCastPtr<PDFDictionary> res (p.QueryDictionaryObject (page.GetPtr (),
                                                                  "Resources"));
    if (!res) {
      PDFObjectCastPtr<PDFDictionary> parent (p.QueryDictionaryObject (page.GetPtr (),
                                                                       "Parent"));
      if (!!parent) res= p.QueryDictionaryObject (parent.GetPtr (), "Resources");
    }
    hashset<int> visited;
    hummus_font_dicts (p, res.GetPtr (), ids, 0, visited);
    hummus_form_ids (p, res.GetPtr (), forms, 0);
    props= hummus_props (p, res.GetPtr ());
    RefCountPtr<PDFObject> c (page->QueryDirectObject ("Contents"));
    if (!!c && c->GetType () == PDFObject::ePDFObjectIndirectObjectReference)
      contents << (int) ((PDFIndirectObjectReference*) c.GetPtr ())->mObjectID;
    else if (!!c && c->GetType () == PDFObject::ePDFObjectArray) {
      PDFArray* a= (PDFArray*) c.GetPtr ();
      for (unsigned long j=0; j < a->GetLength (); j++) {
        RefCountPtr<PDFObject> e (a->QueryObject (j));
        if (e->GetType () == PDFObject::ePDFObjectIndirectObjectReference)
          contents << (int) ((PDFIndirectObjectReference*) e.GetPtr ())->mObjectID;
      }
    }
  }
  int done= 0;
  {
    PDFDocumentCopyingContext* cc= w.CreatePDFCopyingContextForModifiedFile ();
    if (cc != NULL) {
      if (N(contents) > 0 &&
          hummus_strip_streams (p, oc, cc, contents, keep, props))
        done++;
      for (int k=0; k<N(forms); k++) {
        array<int> one;
        one << forms[k];
        if (hummus_strip_streams (p, oc, cc, one, NULL, props)) done++;
      }
      delete cc;
    }
  }
  FreeTypeWrapper ft;
  for (int k=0; k<N(ids); k++) {
    PDFObjectCastPtr<PDFDictionary> f (p.ParseNewObject (ids[k]));
    if (!f) continue;
    string st= hummus_name (p, f.GetPtr (), "Subtype");
    if (st != "Type1" && st != "TrueType" && st != "MMType1") continue;
    PDFObjectCastPtr<PDFDictionary> desc (p.QueryDictionaryObject (f.GetPtr (),
                                                                   "FontDescriptor"));
    if (!!desc && (desc->Exists ("FontFile") || desc->Exists ("FontFile2") ||
                   desc->Exists ("FontFile3"))) continue;
    string base= hummus_name (p, f.GetPtr (), "BaseFont");
    string enc;
    array<string> diffs (256);
    PDFObject* e= p.QueryDictionaryObject (f.GetPtr (), "Encoding");
    if (e != NULL && e->GetType () == PDFObject::ePDFObjectName)
      enc= ((PDFName*) e)->GetValue ().c_str ();
    else if (e != NULL && e->GetType () == PDFObject::ePDFObjectDictionary) {
      PDFDictionary* ed= (PDFDictionary*) e;
      enc= hummus_name (p, ed, "BaseEncoding");
      PDFObjectCastPtr<PDFArray> da (p.QueryDictionaryObject (ed, "Differences"));
      int code= 0;
      for (unsigned long j=0; !!da && j < da->GetLength (); j++) {
        RefCountPtr<PDFObject> it (da->QueryObject (j));
        if (it->GetType () == PDFObject::ePDFObjectInteger)
          code= (int) ((PDFInteger*) it.GetPtr ())->GetValue ();
        else if (it->GetType () == PDFObject::ePDFObjectName &&
                 code >= 0 && code < 256)
          diffs[code++]= ((PDFName*) it.GetPtr ())->GetValue ().c_str ();
      }
    }
    if (e != NULL) e->Release ();
    pdf_font_subst r;
    if (!pdf_font_substitute (base, pdf_encoding_names (base, enc, diffs), r)) {
      convert_warning << "no font to embed for " << base
                      << " (a picture), the PDF is not PDF/UA" << LF;
      continue;
    }
    if (r.missing > 0)
      convert_warning << r.missing << " characters of " << base
                      << " (a picture) have no glyph in " << r.ps_name << LF;
    // the program, subset to the glyphs of the font
    string path= concretize (r.file);
    std::string spath (&path[0], N(path));
    FT_Face face= ft.NewFace (spath, 0);
    if (face == NULL) continue;
    ObjectIDType ff= 0;
    {
      FreeTypeFaceWrapper fw (face, spath, 0, false);
      UIntVector gids;
      for (int j=0; j<N(r.gids); j++) gids.push_back ((unsigned int) r.gids[j]);
      CFFEmbeddedFontWriter cw;
      // (a tag of its own for each subset of the PDF)
      static int tags= 0;
      string sub;
      // (away from those of PDFHummus, AAAAAA+ and on)
      for (int t= 3000000 + tags++, j=0; j<6; j++, t /= 26)
        sub << (char) ('A' + (t % 26));
      sub= sub * "+" * r.ps_name;
      if (cw.WriteEmbeddedFont (fw, gids, "Type1C", as_charp_std (sub), &oc, ff)
          != PDFHummus::eSuccess) ff= 0;
      r.ps_name= sub;
    }
    ft.DoneFace (face);
    if (ff == 0) {
      convert_warning << "PDFHummus could not embed a font for " << base
                      << " (a picture)" << LF;
      continue;
    }
    ObjectIDType tu= hummus_write_stream (oc, pdf_simple_to_unicode (r.unicode), "");
    ObjectIDType did= oc.StartNewIndirectObject ();
    DictionaryContext* d= oc.StartDictionary ();
    d->WriteKey ("Type"); d->WriteNameValue ("FontDescriptor");
    d->WriteKey ("FontName"); d->WriteNameValue (as_charp_std (r.ps_name));
    d->WriteKey ("Flags"); d->WriteIntegerValue (r.flags);
    d->WriteKey ("FontBBox");
    d->WriteRectangleValue (PDFRectangle (r.bbox[0], r.bbox[1], r.bbox[2], r.bbox[3]));
    d->WriteKey ("ItalicAngle"); d->WriteIntegerValue (r.italic);
    d->WriteKey ("Ascent"); d->WriteIntegerValue (r.ascent);
    d->WriteKey ("Descent"); d->WriteIntegerValue (r.descent);
    d->WriteKey ("CapHeight"); d->WriteIntegerValue (r.cap_height);
    d->WriteKey ("StemV"); d->WriteIntegerValue (r.stem_v);
    d->WriteKey ("MissingWidth"); d->WriteIntegerValue (r.missing_width);
    d->WriteKey ("FontFile3"); d->WriteNewObjectReferenceValue (ff);
    oc.EndDictionary (d);
    oc.EndIndirectObject ();
    // the font, in place of the old one: the glyphs of the program by
    // their names, whatever the encoding said
    oc.StartModifiedIndirectObject (ids[k]);
    d= oc.StartDictionary ();
    d->WriteKey ("Type"); d->WriteNameValue ("Font");
    d->WriteKey ("Subtype"); d->WriteNameValue ("Type1");
    d->WriteKey ("BaseFont"); d->WriteNameValue (as_charp_std (r.ps_name));
    d->WriteKey ("FirstChar"); d->WriteIntegerValue (r.first);
    d->WriteKey ("LastChar"); d->WriteIntegerValue (r.last);
    d->WriteKey ("Widths");
    oc.StartArray ();
    for (int j=0; j<N(r.widths); j++) oc.WriteInteger (r.widths[j]);
    oc.EndArray (eTokenSeparatorEndLine);
    d->WriteKey ("Encoding");
    DictionaryContext* ed= oc.StartDictionary ();
    ed->WriteKey ("Type"); ed->WriteNameValue ("Encoding");
    ed->WriteKey ("Differences");
    oc.StartArray ();
    int prev= -2;
    for (int c= 0; c < 256; c++) {
      if (N(r.glyph[c]) == 0) continue;
      if (c != prev + 1) oc.WriteInteger (c);
      oc.WriteName (as_charp_std (r.glyph[c]));
      prev= c;
    }
    oc.EndArray (eTokenSeparatorEndLine);
    oc.EndDictionary (ed);
    d->WriteKey ("FontDescriptor"); d->WriteNewObjectReferenceValue (did);
    d->WriteKey ("ToUnicode"); d->WriteNewObjectReferenceValue (tu);
    oc.EndDictionary (d);
    oc.EndIndirectObject ();
    done++;
  }
  w.EndPDF ();
  if (done == 0) remove (out);
  return done > 0;
}

EStatusCode
pdf_image_rep::merge (PDFDocumentCopyingContext* copyingContext,
                      PDFWriter& pdfw, url temp) {
  int missing= hummus_missing_objects;
  DocumentContext& dc = pdfw.GetDocumentContext();
  PDFPageInput pageInput(copyingContext->GetSourceDocumentParser(),
                         copyingContext->GetSourceDocumentParser()->ParsePage(0));
  EPDFVersion version= (EPDFVersion)(int)(copyingContext->GetSourceDocumentParser()->GetPDFLevel() * 10);
  if (version > ePDFVersion)
    convert_warning << "\"" << concretize (u) << "\" has version " << ((double) version)/10 << "." << LF
                    << "But current PDF version has been set to " << ((double) ePDFVersion)/10
                    << " (see the preference menu)." << LF;
  double tMat[6] ={ 1,0, 0, 1, 0, 0} ;
  PDFRectangle cropBox (0,0,0,0);
  int neww,newh;
  pdf_image_info (temp, neww, newh, cropBox, tMat, pageInput);
  if ((neww != w) && (neww !=0)) {
    //image size changed, resize it to what we wanted
    double r = w/(double)neww;
    for (int j= 0; j < 6; j++) tMat[j] *= r;
  }
  PDFFormXObject *form = dc.StartFormXObject(cropBox, id, tMat, true);
  EStatusCode status = copyingContext->MergePDFPageToFormXObject(form,0);
  if(status == eSuccess) pdfw.EndFormXObjectAndRelease(form);
  if (hummus_missing_objects > missing)
    convert_warning << "the picture " << u << " refers to objects which "
                    << "it lacks (a damaged file): they are null, and what "
                    << "they were may be missing" << LF;
  return status;
}

void
pdf_image_rep::flush (PDFWriter& pdfw, hashmap<string,pointer>* shared,
                      hashmap<string,string>* shared_file,
                      array<url>* to_remove)
{
  string skey= as_string (u);
  if (graft && shared != NULL && (*shared)->contains (skey)) {
    url temp= url_system ((*shared_file)[skey]);
    if (merge ((PDFDocumentCopyingContext*) (*shared)[skey], pdfw, temp)
        == PDFHummus::eFailure)
      convert_error << "pdf_hummus, failed to include image file: "
                    << temp << LF;
    return;
  }
  url name= resolve (u);
  if (is_none (name))
    name= "$TEXMACS_PATH/misc/pixmaps/unknown.ps";
  
  url temp;
  string s= suffix (name);
  //debug_convert << "flushing :" << name << LF;
  if (s == "pdf") {
    // double v= as_double (pdf_version (name));
    // if (10 * v > ((double) ePDFVersion))
    //   convert_warning << "\"" << concretize (name) << "\" has version "
    // 		      << v << "." << LF
    // 		      << "But current PDF version has been set to " << ((double) ePDFVersion)/10
    // 		      << " (see the preference menu)." << LF;
    if (get_preference ("texmacs->pdf:distill inclusion") == "on") {
      temp= url_temp (".pdf");
#ifdef USE_GS
      if (!gs_PDF_EmbedAllFonts (name, temp)) {
	temp= name;
	name= url_none ();
      }
#endif
    }
    else {
      temp= name;
      name= url_none ();
    }
  } 
  else {
    // first try to work out inclusion using our own tools
    // note that we have to return since flush_raster and flush_jpg
    // already build the appopriate Form XObject into the PDF
  
    if ((s == "jpg") || (s == "jpeg")) 
      if (flush_jpg(pdfw, name)) return;
#ifndef PDFHUMMUS_NO_PNG
    if (s == "png")
      if (flush_png(pdfw, name)) return;
#endif
    // other formats we generate a either pdf or png that we'll embbed
    temp= url_temp (".pdf");
    image_to_pdf (name, temp, w, h, 300, false);
    // the 300 dpi setting is the maximum dpi of raster images that will be generated:
    // images that are to dense will de downsampled to keep file small
    // (other are not up-sampled) 
    // dpi DOES NOT apply for vector images that we know how to handle : eps, svg(if inkscape present)
    // 
    // TODO: make the max dpi setting smarter (printer resolution, preference ...)
    if (! exists(temp)) {
#ifndef PDFHUMMUS_NO_PNG
        // nothing worked for pdf, then embed png (if we could display the image, we can use png)
        url temp_png= url_temp (".png");
        image_to_png (name, temp_png, w, h);
        bool done= flush_png (pdfw, temp_png);
        remove (temp_png);
        if (done) return;
#endif
        // the png route failed too: include the placeholder
        convert_error << "pdf_hummus, failed converting " << name << LF;
        copy ("$TEXMACS_PATH/misc/pixmaps/unknown.pdf", temp);
        inform_about_dependencies ();
    }

  }
  // an encrypted picture, which PDFHummus does not read (it damages its
  // memory), is written again by Ghostscript first, or else is the
  // placeholder
  if (!hummus_pdf_plain (temp)) {
    bool done= false;
#ifdef USE_GS
    url clear= url_temp (".pdf");
    if (gs_PDF_EmbedAllFonts (temp, clear) && hummus_pdf_plain (clear)) {
      if (!is_none (name)) remove (temp);
      else name= u;
      temp= clear;
      done= true;
    }
    else remove (clear);
#endif
    if (!done) {
      convert_error << "pdf_hummus, cannot include " << u
                    << " (encrypted or damaged)" << LF;
      if (!is_none (name)) remove (temp);
      name= u;
      temp= url_temp (".pdf");
      copy ("$TEXMACS_PATH/misc/pixmaps/unknown.pdf", temp);
    }
  }
  if (hummus_embed_fonts) {
    url with_fonts= url_temp (".pdf");
    if (hummus_embed_picture_fonts (temp, with_fonts,
                                    graft? &keep: (hashset<int>*) NULL)) {
      if (!is_none (name)) remove (temp);
      else name= u;   // (a temporary file now, which is removed below)
      temp= with_fonts;
    }
  }
  EStatusCode status = PDFHummus::eFailure;

  char* _temp= as_charp(concretize(temp));
  PDFDocumentCopyingContext *copyingContext = pdfw.CreatePDFCopyingContext(_temp);
  tm_delete_array (_temp);
  bool kept= false;
  if(copyingContext) {
    status= merge (copyingContext, pdfw, temp);
    if (graft && shared != NULL) {
      // (kept for the other uses of the picture, which share its fonts)
      (*shared) (skey)= (pointer) copyingContext;
      (*shared_file) (skey)= concretize (temp);
      if (!is_none (name)) *to_remove << temp;
      kept= true;
    }
    else delete copyingContext;
  }
  if(!is_none(name) && !kept) remove (temp);
  // (a picture which cannot be copied, a damaged file: the placeholder, or
  // the form, referred to on the pages, would be missing from the PDF)
  if (status == PDFHummus::eFailure && !kept) {
    url unknown= "$TEXMACS_PATH/misc/pixmaps/unknown.pdf";
    url with_fonts= url_temp (".pdf");
    bool embedded= (hummus_embed_fonts &&
                    hummus_embed_picture_fonts (unknown, with_fonts, NULL));
    if (embedded) unknown= with_fonts;
    char* _unk= as_charp (concretize (unknown));
    PDFDocumentCopyingContext* uc= pdfw.CreatePDFCopyingContext (_unk);
    tm_delete_array (_unk);
    if (uc != NULL) {
      status= merge (uc, pdfw, unknown);
      delete uc;
    }
    if (embedded) remove (with_fonts);
  }
  
  if (status == PDFHummus::eFailure) {
    convert_error << "pdf_hummus, failed to include image file: "
                  << temp << LF;
  }
}

void
hummus_pdf_image_size (url image, int& w, int& h) {
  // (PDFHummus damages its memory on an encrypted file: Ghostscript then)
  if (!hummus_pdf_plain (image)) {
#ifdef USE_GS
    gs_PDFimage_size (image, w, h);
#else
    w= h= 0;
#endif
    return;
  }
  InputFile pdfFile;
  PDFParser* parser= new PDFParser();
  pdfFile.OpenFile(as_charp(concretize(image)));
  EStatusCode status = parser->StartPDFParsing(pdfFile.GetInputStream());
  if (status != PDFHummus::eFailure) {
    PDFPageInput pageInput(parser, parser->ParsePage(0));
    double tMat[6] ={ 1,0, 0, 1, 0, 0};
    PDFRectangle cropBox (0,0,0,0);
    pdf_image_info (image, w, h, cropBox, tMat, pageInput);
    delete(parser);
  }
  else {
    convert_error << "pdf_hummus, failed to get image size for: "
                  <<image << LF;
    w=h=0;              
  }
}

void
pdf_image_info (url image, int& w, int& h, PDFRectangle& cropBox, double (&tMat)[6], PDFPageInput& pageInput) {
  int rot= (pageInput.GetRotate())%360;
  if (rot < 0) rot +=360;
  cropBox=pageInput.GetCropBox();
  PDFRectangle mediaBox=pageInput.GetMediaBox();
  if (!(cropBox.LowerLeftX >= mediaBox.LowerLeftX &&
	cropBox.LowerLeftY >= mediaBox.LowerLeftY &&
	cropBox.UpperRightX <= mediaBox.UpperRightX &&
	cropBox.UpperRightY <= mediaBox.UpperRightY))
    convert_warning << "pdf_image_info, " << image << ": "
                    << "cropbox not included in mediabox\n";
  w= cropBox.UpperRightX-cropBox.LowerLeftX;
  h= cropBox.UpperRightY-cropBox.LowerLeftY;
  if (DEBUG_CONVERT) {
    debug_convert << "hummus_pdf_image_info:"<< LF
      << "image ="<<image<<LF
      << "crop box={"<<cropBox.LowerLeftX<< ", "<<cropBox.UpperRightX 
      << ", "<<cropBox.LowerLeftY<< ", "<<cropBox.UpperRightY<<"}"<< LF
      << "w,h={"<<w<< ", "<<h <<"}"<< LF;
  }
  int z;
  switch  (rot) {
    case 0 :
      tMat[0] = 1; 
      tMat[1] = 0;
      tMat[2] = 0;
      tMat[3] = 1;
      tMat[4] = -cropBox.LowerLeftX;
      tMat[5] = -cropBox.LowerLeftY;
      break;
    case 90 :
      tMat[0] = 0;
      tMat[1] = -1 ;
      tMat[2] = 1;
      tMat[3] = 0;
      tMat[4] = -cropBox.LowerLeftY;
      tMat[5] = cropBox.UpperRightX ;
      z = w;
      w = h;
      h = z;
      break;
    case 180 :
      tMat[0] = -1;
      tMat[1] = 0;
      tMat[2] = 0;
      tMat[3] = -1;
      tMat[4] = cropBox.UpperRightX ;
      tMat[5] = cropBox.UpperRightY ;
      break;
    case 270 :
      tMat[0] = 0;
      tMat[1] = 1 ;
      tMat[2] = -1;
      tMat[3] = 0;
      tMat[4] = cropBox.UpperRightY ;
      tMat[5] = -cropBox.LowerLeftX;
      z = w;
      w = h;
      h = z;
      break;
    default :
      convert_error << "unexpected rotate()="<< rot<<" in image "<<image << LF;
    }
  if (DEBUG_CONVERT) debug_convert << "degrees image rotated :"<< rot << LF
    << "dx,dy={"<<tMat[4]<< ", "<<tMat[5] <<"}"<< LF;
}

#ifndef QTTEXMACS
// the pixels of an image (RGB, and the alpha as a mask), from the top row,
// through the pictures of the interface (Cocoa), as qt_image_data below
static void
picture_image_data (url image, int& w, int&h, string& data, string& mask) {
  picture pic= load_picture (image, 0, 0, tree (""), PIXEL);
  if (is_nil (pic) || pic->get_width () <= 0 || pic->get_height () <= 0) {
    convert_error << "Cannot read image file '" << image << "'"
    << " in picture_image_data" << LF;
    return;
  }
  w= pic->get_width ();
  h= pic->get_height ();
  data= string ((w*h)*3);
  mask= string (w*h);
  int k= 0, l= 0;
  for (int j= 0; j < h; j++)
    for (int i= 0; i < w; i++) {
      // the rows of a picture go up from its bottom
      int r, g, b, a;
      get_rgb_color (pic->get_pixel (i - pic->get_origin_x (),
                                     h - 1 - j - pic->get_origin_y ()),
                     r, g, b, a);
      data[l++]= (char) r;
      data[l++]= (char) g;
      data[l++]= (char) b;
      mask[k++]= (char) a;
    }
}
#endif

#ifdef QTTEXMACS
void
qt_image_data (url image, int& w, int&h, string& data, string& mask) {
  // debug_convert << "in qt_image_data"<<LF; 
  QImage im (utf8_to_qstring (concretize (image)));
  if (im.isNull ()) {
    convert_error << "Cannot read image file '" << image << "'"
    << " in qt_image_data" << LF;
    return;
  }
  w=  im.width ();
  h=  im.height ();    
  data = string ((w*h)*3);
  mask = string (w*h); 
  int i= 0, j= 0, k= 0, l= 0;
  for (j= 0; j < im.height (); j++) {
    for (i=0; i < im.width (); i++) {
      QRgb p= im.pixel (i, j);
      data[l++] = qRed (p);
      data[l++] = qGreen (p);
      data[l++] = qBlue (p);
      mask[k++] = qAlpha (p);
    }
  }
}
#endif

bool
pdf_image_rep::flush_for_pattern (PDFWriter& pdfw) {
  string data, smask;
  int iw = 0, ih =0;
#ifdef QTTEXMACS
  qt_image_data (u, iw, ih, data, smask);
#else
  picture_image_data (u, iw, ih, data, smask);
#endif
  if ((iw==0)||(ih==0)) return false;
  
  ObjectsContext& objectsContext = pdfw.GetObjectsContext();
  objectsContext.StartNewIndirectObject(id);

  DictionaryContext* imageContext = objectsContext.StartDictionary();
  imageContext->WriteKey(scType);
  imageContext->WriteNameValue(scXObject);
  imageContext->WriteKey(scSubType);
  imageContext->WriteNameValue(scImage);
  imageContext->WriteKey(scWidth);
  imageContext->WriteIntegerValue(iw);
  imageContext->WriteKey(scHeight);
  imageContext->WriteIntegerValue(ih);
  imageContext->WriteKey(scBitsPerComponent);
  imageContext->WriteIntegerValue(8);
  imageContext->WriteKey(scColorSpace);
  imageContext->WriteNameValue(scDeviceRGB);
  ObjectIDType smaskId = objectsContext.GetInDirectObjectsRegistry().AllocateNewObjectID();
  imageContext->WriteKey("SMask");
  imageContext->WriteNewObjectReferenceValue(smaskId);
  PDFStream* imageStream = objectsContext.StartPDFStream(imageContext, true);
  OutputStreamTraits outputTraits(imageStream->GetWriteStream());
  c_string buf (data);
  InputByteArrayStream reader((IOBasicTypes::Byte*)(char *)buf, N(data));
  EStatusCode status = outputTraits.CopyToOutputStream(&reader);
  if(status != PDFHummus::eSuccess) {
    delete imageStream;
    return false;
  }
  objectsContext.EndPDFStream(imageStream); // It does EndIndirectObject();
  delete imageStream;

  objectsContext.StartNewIndirectObject(smaskId);
  DictionaryContext* smaskContext = objectsContext.StartDictionary();
  smaskContext->WriteKey(scType);
  smaskContext->WriteNameValue(scXObject);
  smaskContext->WriteKey(scSubType);
  smaskContext->WriteNameValue(scImage);
  smaskContext->WriteKey(scWidth);
  smaskContext->WriteIntegerValue(iw);
  smaskContext->WriteKey(scHeight);
  smaskContext->WriteIntegerValue(ih);
  smaskContext->WriteKey(scBitsPerComponent);
  smaskContext->WriteIntegerValue(8);
  smaskContext->WriteKey(scColorSpace);
  smaskContext->WriteNameValue(scDeviceGray);
  PDFStream* smaskStream = objectsContext.StartPDFStream(smaskContext, true);
  OutputStreamTraits smaskOutputTraits (smaskStream->GetWriteStream());
  c_string buf_smask (smask);
  InputByteArrayStream smaskReader ((IOBasicTypes::Byte*)(char *)buf_smask, N(smask));
  status = smaskOutputTraits.CopyToOutputStream(&smaskReader);
  if(status != PDFHummus::eSuccess) {
    delete smaskStream;
    return false;
  }
  objectsContext.EndPDFStream(smaskStream); // It does EndIndirectObject();
  delete smaskStream;
  return true;
}

bool
pdf_image_rep::flush_jpg (PDFWriter& pdfw, url image) {
  c_string f (concretize (image));
  DocumentContext& documentContext= pdfw.GetDocumentContext();
  PDFImageXObject* imageXObject= documentContext.CreateImageXObjectFromJPGFile ((char*) f);
  if ((void*) imageXObject == NULL) { 
    convert_error << "pdf_hummus, failed to include JPG file " << image << LF;
    return false;
  }
  PDFFormXObject* xobjectForm = pdfw.StartFormXObject(PDFRectangle(0, 0, w, h), id); 
  XObjectContentContext* xobjectContentContext = xobjectForm->GetContentContext();
  xobjectContentContext->q();
  xobjectContentContext->cm(w, 0, 0, h, 0, 0);
  std::string pdfImageName = xobjectForm->GetResourcesDictionary().AddImageXObjectMapping(imageXObject);
  xobjectContentContext->Do(pdfImageName);
  xobjectContentContext->Q();
  EStatusCode status = pdfw.EndFormXObjectAndRelease(xobjectForm);
  delete imageXObject;
  return status == eSuccess;
}

#ifndef PDFHUMMUS_NO_PNG
bool
pdf_image_rep::flush_png (PDFWriter& pdfw, url image) {
  c_string f (concretize (image));
  PNGImageHandler pngHandler;
  InputFile file;
  if (file.OpenFile (std::string ((char*) f)) != PDFHummus::eSuccess) {
    convert_error << "pdf_hummus, failed to include PNG file " << image << LF;
    return false;
  }
  IByteReaderWithPosition* stream= file.GetInputStream ();
  DoubleAndDoublePair dim= pngHandler.ReadImageDimensions (stream);
  double iw= dim.first, ih= dim.second;

  stream->SetPosition (0);
  PDFFormXObject* formXObject= pdfw.CreateFormXObjectFromPNGStream (stream);
  if ((void*) formXObject == NULL) { 
    convert_error << "pdf_hummus, failed to create Form from PNG stream" << LF;
    return false;
  }
  PDFFormXObject* xobjectForm= pdfw.StartFormXObject (PDFRectangle (0, 0, w, h), id); 
  XObjectContentContext* xobjectContentContext = xobjectForm->GetContentContext ();
  xobjectContentContext->q ();
  xobjectContentContext->cm (w / iw, 0, 0, h / ih, 0, 0);
  std::string pdfImageName = xobjectForm->GetResourcesDictionary ()
    .AddFormXObjectMapping (formXObject->GetObjectID ());
  xobjectContentContext->Do (pdfImageName);
  xobjectContentContext->Q ();
  EStatusCode status = pdfw.EndFormXObjectAndRelease (xobjectForm);
  delete formXObject;
  return status == eSuccess;
}
#endif

void
pdf_hummus_renderer_rep::flush_images ()
{
  // flush all images
  iterator<tree> it = iterate (image_pool);
  while (it->busy()) {
    pdf_image im = image_pool[it->next()];
    im->flush(pdfWriter);
  }
  hashmap<string,pointer> shared;
  hashmap<string,string> shared_file;
  array<url> to_remove;
  for (int i=0; i<N(graft_images); i++)
    graft_images[i]->flush (pdfWriter, &shared, &shared_file, &to_remove);
  iterator<string> sit= iterate (shared);
  while (sit->busy ())
    delete (PDFDocumentCopyingContext*) shared [sit->next ()];
  for (int i=0; i<N(to_remove); i++) remove (to_remove[i]);
}

void
pdf_hummus_renderer_rep::flush_patterns ()
{
  // flush all pattern images
  iterator<tree> it = iterate (pattern_image_pool);
  while (it->busy()) {
    pdf_image im = pattern_image_pool[it->next()];
    im->flush_for_pattern(pdfWriter);
  }
  // flush all patterns
  it = iterate (pattern_pool);
  while (it->busy()) {
    pdf_pattern pa = pattern_pool[it->next()];
    pa->flush(pdfWriter);
  }
}


/******************************************************************************
* The structure of a picture which is itself a tagged PDF, to be grafted in
* place of its Figure (docs/pdf-tagging.md, pdf_tags_rep::graft)
******************************************************************************/

struct hummus_graft {
  array<int> parent, owner, val;
  array<string> role, alt;
  int mcids;
  int steps;           // of the walk (a budget)
  bool stm;            // marked content in forms of the picture: no graft
  hashset<int> seen;   // the objects visited (a tree may share or loop)
  hummus_graft (): mcids (0), steps (0), stm (false) {}
};

// whether the raw object v is met for the first time
static bool
hummus_graft_first (PDFObject* v, hummus_graft& g) {
  if (v == NULL || v->GetType () != PDFObject::ePDFObjectIndirectObjectReference)
    return true;
  int id= (int) ((PDFIndirectObjectReference*) v)->mObjectID;
  if (g.seen->contains (id)) return false;
  g.seen->insert (id);
  return true;
}

static string
hummus_text (PDFObject* o) {
  if (o == NULL) return "";
  std::string raw;
  if (o->GetType () == PDFObject::ePDFObjectLiteralString)
    raw= ((PDFLiteralString*) o)->GetValue ();
  else if (o->GetType () == PDFObject::ePDFObjectHexString)
    raw= ((PDFHexString*) o)->GetValue ();
  else return "";
  std::string u= PDFTextString (raw).ToUTF8String ();
  return string (u.c_str (), (int) u.size ());
}

static void
hummus_graft_walk (PDFParser& p, PDFDictionary* rolemap, PDFObject* k,
                   int owner, ObjectIDType pg, ObjectIDType page0,
                   hummus_graft& g, int depth) {
  if (k == NULL || depth > 64 || N(g.parent) >= 4096 || N(g.val) >= 16384 ||
      ++g.steps > 200000)
    return;
  if (k->GetType () == PDFObject::ePDFObjectArray) {
    PDFArray* a= (PDFArray*) k;
    for (unsigned long i=0; i < a->GetLength (); i++) {
      RefCountPtr<PDFObject> raw (a->QueryObject (i));
      if (!hummus_graft_first (raw.GetPtr (), g)) continue;
      RefCountPtr<PDFObject> c (p.QueryArrayObject (a, i));
      hummus_graft_walk (p, rolemap, c.GetPtr (), owner, pg, page0, g,
                         depth + 1);
    }
    return;
  }
  if (k->GetType () == PDFObject::ePDFObjectInteger) {
    int m= (int) ((PDFInteger*) k)->GetValue ();
    if (owner >= 0 && m >= 0 && m < 65536 && pg == page0) {
      g.owner << owner; g.val << (-1-m);
      if (m >= g.mcids) g.mcids= m + 1;
    }
    return;
  }
  if (k->GetType () != PDFObject::ePDFObjectDictionary) return;
  PDFDictionary* d= (PDFDictionary*) k;
  RefCountPtr<PDFObject> pgo (d->QueryDirectObject ("Pg"));
  if (!!pgo && pgo->GetType () == PDFObject::ePDFObjectIndirectObjectReference)
    pg= ((PDFIndirectObjectReference*) pgo.GetPtr ())->mObjectID;
  string type= hummus_name (p, d, "Type");
  if (type == "MCR") {
    // (the marked content of a form of the picture: such a picture is a
    // Figure, its tags taken out)
    if (d->Exists ("Stm")) { g.stm= true; return; }
    RefCountPtr<PDFObject> m (p.QueryDictionaryObject (d, "MCID"));
    hummus_graft_walk (p, rolemap, m.GetPtr (), owner, pg, page0, g,
                       depth + 1);
    return;
  }
  if (type == "OBJR") return;
  string st= hummus_name (p, d, "S");
  if (N(st) == 0) return;
  // the standard type, through the role map
  const char* r= NULL;
  string cur= st;
  for (int j=0; j<8 && r == NULL && N(cur) > 0; j++) {
    c_string cs (cur);
    r= pdf_graft_role ((char*) cs);
    if (r == NULL)
      cur= (rolemap == NULL? string (""):
            hummus_name (p, rolemap, (char*) cs));
  }
  int e= N(g.parent);
  g.parent << owner;
  g.role << string (r == NULL? "NonStruct": r);
  RefCountPtr<PDFObject> alt (p.QueryDictionaryObject (d, "Alt"));
  g.alt << hummus_text (alt.GetPtr ());
  g.owner << owner; g.val << e;
  RefCountPtr<PDFObject> kraw (d->QueryDirectObject ("K"));
  if (!hummus_graft_first (kraw.GetPtr (), g)) return;
  RefCountPtr<PDFObject> kk (p.QueryDictionaryObject (d, "K"));
  hummus_graft_walk (p, rolemap, kk.GetPtr (), e, pg, page0, g, depth + 1);
}

// the structure of the first page of the picture u, if it is tagged
static bool
hummus_graft_read (url u, pdf_tag_graft& r, hashset<int>& keep) {
  url name= resolve (u);
  if (is_none (name) || !hummus_pdf_plain (name)) return false;
  InputFile f;
  string path= concretize (name);
  if (f.OpenFile (std::string (&path[0], N(path))) != PDFHummus::eSuccess)
    return false;
  PDFParser p;
  if (p.StartPDFParsing (f.GetInputStream ()) != PDFHummus::eSuccess)
    return false;
  PDFObjectCastPtr<PDFDictionary> root (p.QueryDictionaryObject (p.GetTrailer (),
                                                                 "Root"));
  if (!root) return false;
  PDFObjectCastPtr<PDFDictionary> str (p.QueryDictionaryObject (root.GetPtr (),
                                                                "StructTreeRoot"));
  if (!str) return false;
  PDFObjectCastPtr<PDFDictionary> rm (p.QueryDictionaryObject (str.GetPtr (),
                                                               "RoleMap"));
  RefCountPtr<PDFObject> k (p.QueryDictionaryObject (str.GetPtr (), "K"));
  hummus_graft g;
  hummus_graft_walk (p, rm.GetPtr (), k.GetPtr (), -1, 0,
                     p.GetPageObjectID (0), g, 0);
  if (g.stm || g.steps > 200000) return false;
  r= pdf_graft_prune (g.parent, g.role, g.alt, g.owner, g.val, g.mcids, keep);
  if (N(r.roles) == 0) return false;
  // (all that the page paints is in marked content: else it would be
  // untagged; the picture is then a Figure)
  RefCountPtr<PDFDictionary> page (p.ParsePage (0));
  if (!page) return false;
  RefCountPtr<PDFObject> c (page->QueryDirectObject ("Contents"));
  array<int> cids;
  if (!!c && c->GetType () == PDFObject::ePDFObjectIndirectObjectReference)
    cids << (int) ((PDFIndirectObjectReference*) c.GetPtr ())->mObjectID;
  else if (!!c && c->GetType () == PDFObject::ePDFObjectArray) {
    PDFArray* a= (PDFArray*) c.GetPtr ();
    for (unsigned long j=0; j < a->GetLength (); j++) {
      RefCountPtr<PDFObject> e (a->QueryObject (j));
      if (e->GetType () == PDFObject::ePDFObjectIndirectObjectReference)
        cids << (int) ((PDFIndirectObjectReference*) e.GetPtr ())->mObjectID;
    }
  }
  string content;
  for (int j=0; j<N(cids); j++) {
    RefCountPtr<PDFDictionary> d;
    if (!hummus_read_stream (p, cids[j], content, d)) return false;
    content << "\n";
  }
  // (all that the content of the graft paints is tagged or an artifact)
  RefCountPtr<PDFDictionary> res (hummus_page_resources (p, page.GetPtr ()));
  bool changed= false, ok= false;
  (void) pdf_graft_marked_content (content, keep, hummus_props (p, res.GetPtr ()),
                                   changed, ok);
  return ok;
}

void
pdf_hummus_renderer_rep::image (
  url u, double w, double h, SI x, SI y, int alpha)
{
  // debug_convert << "pdf renderer, image " << u << ", " << w << " x " << h
  //		<< " + (" << x << ", " << y << ")" << LF;
  tree lookup= tuple (u->t);
  pdf_image im;
  // a tagged picture of no alternative text of the user's has its own
  // structure grafted in place of its Figure (pdf_tags_rep::graft), and a
  // form for each of its uses (an element is in one place of the tree)
  pdf_tag_graft g;
  hashset<int> keep;
  // (not when the pictures are distilled again by Ghostscript, whose copy
  // has other marked content)
  bool may= (tagger != NULL && contentContext != NULL &&
              get_preference ("texmacs->pdf:distill inclusion") != "on" &&
              tg ()->can_graft (tag_ip) &&
              !untagged_files->contains (as_string (u)) &&
              locase_all (suffix (u)) == "pdf");
  if (may && !hummus_graft_read (u, g, keep)) {
    untagged_files->insert (as_string (u));
    may= false;
  }
  if (may) {
    im= pdf_image (u, pdfWriter.GetObjectsContext().GetInDirectObjectsRegistry().AllocateNewObjectID());
    im->graft= true;
    im->keep = keep;
    graft_images << im;
    int stream;
    string ops= tg ()->graft (tag_ip, page_num, g, stream);
    if (N(ops) > 0) {
      end_text ();
      c_string cops (ops);
      contentContext->WriteFreeCode (std::string ((char*) cops));
    }
    form_struct_parents ((int) im->id)= pdf_tag_stream_key0 + stream;
    graft_form_ids << im->id;
  }
  else {
    tag_mark (2);
    im= ( image_pool->contains(lookup) ? image_pool[lookup] : pdf_image() );
    if (is_nil(im)) {
      im = pdf_image(u, pdfWriter.GetObjectsContext().GetInDirectObjectsRegistry().AllocateNewObjectID());
      image_pool(lookup) = im;
    }
  }

  if (is_nil(im)) return;
  
  end_text();
  
  contentContext->q();
  std::string initial_GState_name = page->GetResourcesDictionary()
    .AddExtGStateMapping(initial_GState_id);
  contentContext->gs(initial_GState_name);
  contentContext->cm (((double)w) / ((double)im->w), 0,
		      0, ((double)h) / ((double)im->h),
		      to_x (x), to_y (y));
  std::string pdfFormName = page->GetResourcesDictionary().AddFormXObjectMapping(im->id);
  contentContext->RG(0, 0, 0);
  select_alpha((1000 * alpha) / 255);
  contentContext->Do(pdfFormName);
  //contentContext->re(0,0,im->w,im->h);
  //contentContext->S();
  contentContext->Q();
}

/******************************************************************************
* Applying effects to existing pictures
******************************************************************************/

#ifdef QTTEXMACS
static picture
pdf_raster_picture (picture pic) {
  picture ret= qt_picture (QImage (pic->get_width (), pic->get_height (),
                                   QImage::Format_ARGB32),
                           pic->get_origin_x (), pic->get_origin_y ());
  ret->copy_from (pic);
  return ret;
}
#endif

void
pdf_hummus_renderer_rep::draw_picture (picture p, SI x, SI y, int alpha) {
  tag_mark (2);
  // debug_convert << "pdf renderer, draw_picture " << x << ", " << y
  //		<< " (" << alpha << ")" << LF;
  url temp;
  unsigned long long int key= p->get_unique_id ();
  if (picture_cache->contains (key)) temp= picture_cache[key];
  else {
    // As an improvement one could handle native pictures without conversions
#ifdef QTTEXMACS
    picture q= pdf_raster_picture (p);
    qt_picture_rep* pict= (qt_picture_rep*) q->get_handle ();
    temp= url_temp (".png");
    pict->pict.save (utf8_to_qstring (concretize (temp)), "PNG");
    temp_images << temp;	
#else
    // through the pictures of the interface (Cocoa)
    temp= url_temp (".png");
    save_picture (temp, p);
    temp_images << temp;
#endif
    picture_cache (key)= temp;
  }
  int _pixel= (int) (PIXEL / PICTURE_ZOOM);
  int w= p->get_width (), h= p->get_height ();
  int ox= p->get_origin_x (), oy= p->get_origin_y ();
  image (temp, w, h, x - ox * _pixel, y - oy * _pixel, alpha);
}

void
pdf_hummus_renderer_rep::draw_scalable (scalable im, SI x, SI y, int alpha) {
  // debug_convert << "pdf renderer, draw_scalable "
  //   << im->get_name () << " at " << x << ", " << y
  //   << " (" << alpha << ")" << LF;
  if (im->get_type () != scalable_image ||
      (im->get_type () == scalable_image && im->get_effect () != tree (""))) {
    tag_mark (2);
    renderer_rep::draw_scalable (im, x, y, alpha);
  }
  else {
    // (image marks it, or grafts the structure of a tagged picture)
    url u= im->get_name ();
    rectangle r= im->get_logical_extents ();
    SI w= r->x2 - r->x1, h= r->y2 - r->y1;
    int ox= r->x1, oy= r->y1;
    image (u, ((double) w) / pixel, ((double) h) / pixel,
	   x - ox, y - oy, alpha);
  }
}

/******************************************************************************
* Conversions between strings and std::string
******************************************************************************/

string
std_string_to_string (std::string str) {
  string r;
  for (std::string::iterator it=str.begin(); it!=str.end(); ++it) {
    r << *it;
  }
  return r;
}

// At least in outlines a single paren will break the PDF and no more
// outlines will show up after it. Does it need this for other PDF
// strings?
//
// Jim King, Document Management — Portable Document Format — Part 1:
// PDF 1.7 (Adobe Sys. Inc., First ed. 2008),
// §7.3.4.2, Literal Strings, Table 3, Escape sequences in literal strings
//
// Fixme? This does not handle octal character sequences \nnn. Does it
// ever need to?
//
string
escape_string (string s) {
    int i, n= N(s);
  string r;
  for (i=0; i<n; i++)
    switch (s[i]) {
    case '\n':
      r << '\\' << 'n';
      break;
    case '\r':
      r << '\\' << 'r';
      break;
    case '\t':
      r << '\\' << 't';
      break;
    case '\b':
      r << '\\' << 'b';
      break;
    case '\f':
      r << '\\' << 'f';
      break;
    case '(':
    case ')':
    case '\\':
      r << '\\' << s[i];
      break;
    default:
      r << s[i];
    }
  return r;
}

static PDFTextString
utf8_as_hummus_string (string s) {
  c_string u (s);
  PDFTextString r;
  std::string stds ((char*) u);
  return r.FromUTF8 (stds);
}

static PDFTextString
as_hummus_string (string s) {
  return utf8_as_hummus_string(cork_to_utf8 (s));
}

static string
prepare_text (string s) {
  std::string r= as_hummus_string(s).ToString();
  return std_string_to_string (r);
}

/******************************************************************************
 * hyperlinks
 ******************************************************************************/

PDFHummus::EStatusCode
pdf_hummus_renderer_rep::on_catalog_write (CatalogInformation* inCatalogInformation,
                                         DictionaryContext* inCatalogDictionaryContext,
                                         ObjectsContext* inPDFWriterObjectContext,
                                         PDFHummus::DocumentContext* inDocumentContext)
{
  (void) inCatalogInformation;
  (void) inDocumentContext;
  if (destId) {
    inCatalogDictionaryContext->WriteKey("Dests");
    inCatalogDictionaryContext->WriteNewObjectReferenceValue(destId);
  }
  if (outlineId) {
    inCatalogDictionaryContext->WriteKey("Outlines");
    inCatalogDictionaryContext->WriteNewObjectReferenceValue(outlineId);
  }
  if (struct_tree_id) {
    // a tagged PDF (PDF/UA-1, see write_structure)
    ObjectsContext* oc= inPDFWriterObjectContext;
    DictionaryContext* cat= inCatalogDictionaryContext;
    cat->WriteKey ("StructTreeRoot");
    cat->WriteNewObjectReferenceValue (struct_tree_id);
    cat->WriteKey ("MarkInfo");
    DictionaryContext* mi= oc->StartDictionary ();
    mi->WriteKey ("Marked");
    mi->WriteBooleanValue (true);
    oc->EndDictionary (mi);
    string lang= tagger->language ();
    if (N(lang) > 0) {
      c_string cl (lang);
      cat->WriteKey ("Lang");
      cat->WriteLiteralStringValue (std::string ((char*) cl));
    }
    cat->WriteKey ("ViewerPreferences");
    DictionaryContext* vp= oc->StartDictionary ();
    vp->WriteKey ("DisplayDocTitle");
    vp->WriteBooleanValue (true);
    oc->EndDictionary (vp);
    cat->WriteKey ("Metadata");
    cat->WriteNewObjectReferenceValue (xmp_id);
    if (N (video_files) == 0) {
      cat->WriteKey ("Version");
      cat->WriteNameValue ("1.7");
    }
  }
  if (N (video_files) > 0) {
    // embedded videos are RichMedia annotations: PDF 1.7, Adobe extension
    // level 3 (the catalog may raise the version of the header)
    ObjectsContext* oc= inPDFWriterObjectContext;
    inCatalogDictionaryContext->WriteKey ("Version");
    inCatalogDictionaryContext->WriteNameValue ("1.7");
    inCatalogDictionaryContext->WriteKey ("Extensions");
    DictionaryContext* ext= oc->StartDictionary ();
    ext->WriteKey ("ADBE");
    DictionaryContext* adbe= oc->StartDictionary ();
    adbe->WriteKey ("BaseVersion");
    adbe->WriteNameValue ("1.7");
    adbe->WriteKey ("ExtensionLevel");
    adbe->WriteIntegerValue (3);
    oc->EndDictionary (adbe);
    oc->EndDictionary (ext);
  }
  return PDFHummus::eSuccess;
}

int pdf_hummus_renderer_rep::get_label_id(string label)
{
  if (!(label_id->contains(label))) {
    label_id(label) = label_count;
    label_count++;
  }
  return label_id(label);
}

void
pdf_hummus_renderer_rep::anchor (string label, SI x1, SI y1, SI x2, SI y2)
{
  (void) x2; (void) y1;
  string l = prepare_text (label);
  dests << dest_data(l, page_num, to_x(x1), to_y(y2+20*pixel));
}

void
pdf_hummus_renderer_rep::href (string label, SI x1, SI y1, SI x2, SI y2)
{
  bool preserve= (get_locus_rendering ("locus-on-paper") == "preserve");
  ObjectIDType annotId = pdfWriter.GetObjectsContext().GetInDirectObjectsRegistry().AllocateNewObjectID();
  pdfWriter.GetDocumentContext().RegisterAnnotationReferenceForNextPageWrite(annotId);
  string dict;
  dict << "<<\r\n\t/Type /Annot\r\n\t/Subtype /Link\r\n";
//  dict << "\t/Border [1.92 1.92 0.12[]]\r\n\t/Color [0.75 0.5 1.0]\r\n";
  if (preserve)
    dict << "\t/Border [16 16 1 [3 10]] /Color [0.75 0.5 1.0]\r\n";
  else
    dict << "\t/Border [16 16 0 [3 10]] /Color [0.75 0.5 1.0]\r\n";
  dict << "\t/Rect [" << as_string(((double)default_dpi / dpi)*to_x(x1 - 5*pixel)) << " ";
  dict << as_string(((double)default_dpi / dpi)*to_y(y1 - 10*pixel)) << " ";
  dict << as_string(((double)default_dpi / dpi)*to_x(x2 + 5*pixel)) << " ";
  dict << as_string(((double)default_dpi / dpi)*to_y(y2 + 10*pixel)) << "]\r\n";
  if (starts (label, "#")) {
    dict << "\t/Dest /label" << as_string(get_label_id(prepare_text (label))) << "\r\n";
  }
  else {
    dict << "/A << /S /URI /URI (" << escape_string (prepare_text (label))
         << ") >>\r\n";
  }
  if (tagger != NULL) {
    // PDF/UA: a description, and the place in the structure (the Link of
    // the text, see pdf_tags)
    path pos;
    int e= tg ()->link_element (tag_ip, pos);
    int k= N(tag_annot_ids);
    tag_annot_ids << annotId;
    tg ()->add_annotation (e, k, page_num, pos);
    string l= cork_to_utf8 (starts (label, "#")?
                            "Link to " * label (1, N(label)): label);
    // (never empty: a link of no target)
    if (N(l) == 0 || l == "Link to ") l= "Link";
    dict << "\t/Contents " << pdf_text_string (l) << "\r\n";
    dict << "\t/StructParent " << as_string (pdf_tag_annot_key0 + k) << "\r\n";
  }
  dict << ">>\r\n";
  annot_list (annotId) = dict;
}


/******************************************************************************
* Embedded videos
*
* A video (e.g. an animated gif inserted with <video>) becomes a RichMedia
* annotation (as made by the media9 LaTeX package) over the picture of its
* current frame, whose video asset is an MP4 file embedded in the PDF, made
* from the frames of the image exactly as TeXmacs shows them.  In Okular it
* starts when the page becomes visible, loops when the animation does, and
* a click pauses it; viewers which cannot play videos show the picture.
******************************************************************************/


void
pdf_hummus_renderer_rep::embed_video (url u, int ms, bool repeat,
                                      SI x1, SI y1, SI x2, SI y2) {
  string key= as_string (u) * "|" * as_string (ms);
  ObjectIDType fileId= video_file_id [key];
  if (fileId == 0) {
    url v= video_for_pdf (u, ms, temp_images);
    if (is_none (v)) {
      convert_warning << "Cannot embed video " << u << " in PDF\n";
      return;
    }
    fileId= pdfWriter.GetObjectsContext().GetInDirectObjectsRegistry()
                                         .AllocateNewObjectID();
    video_file_id (key)= fileId;
    video_files (fileId)= v;
  }
  ObjectsContext& oc= pdfWriter.GetObjectsContext();
  ObjectIDType annotId= oc.GetInDirectObjectsRegistry().AllocateNewObjectID();
  pdfWriter.GetDocumentContext().RegisterAnnotationReferenceForNextPageWrite
    (annotId);
  string name= prepare_text (as_string (tail (u)));
  string fname= "video-" * as_string (fileId) * ".mp4";
  double f= ((double) default_dpi) / dpi;
  string filespec;
  filespec << "<< /Type /Filespec /F (" << fname << ") /UF (" << fname
           << ") /EF << /F " << as_string (fileId) << " 0 R >> >>";
  string dict;
  dict << "<<\r\n\t/Type /Annot\r\n\t/Subtype /RichMedia\r\n\t/F 4\r\n";
  dict << "\t/Rect [" << as_string (f * to_x (x1)) << " "
       << as_string (f * to_y (y1)) << " " << as_string (f * to_x (x2))
       << " " << as_string (f * to_y (y2)) << "]\r\n";
  dict << "\t/NM (" << name << ")\r\n\t/Contents (" << name << ")\r\n";
  dict << "\t/P @PAGE@\r\n";
  if (tagger != NULL) {
    // PDF/UA: an Annot element in the element of the picture
    path pos;
    int e= tg ()->annot_element (tag_ip, pos);
    int k= N(tag_annot_ids);
    tag_annot_ids << annotId;
    tg ()->add_annotation (e, k, page_num, pos);
    dict << "\t/StructParent " << as_string (pdf_tag_annot_key0 + k) << "\r\n";
  }
  video_annot_page (annotId)= page_num;
  dict << "\t/RichMediaSettings << /Type /RichMediaSettings\r\n"
       << "\t\t/Activation << /Type /RichMediaActivation /Condition /PV"
       << " /Presentation << /Type /RichMediaPresentation /Style /Embedded"
       << " >> >>\r\n"
       << "\t\t/Deactivation << /Type /RichMediaDeactivation /Condition /PI"
       << " >> >>\r\n";
  dict << "\t/RichMediaContent << /Type /RichMediaContent\r\n"
       << "\t\t/Assets << /Names [ (" << fname << ") " << filespec
       << " ] >>\r\n"
       << "\t\t/Configurations [ << /Type /RichMediaConfiguration"
       << " /Subtype /Video /Instances [ << /Type /RichMediaInstance"
       << " /Subtype /Video /Params << /Type /RichMediaParams /FlashVars ("
       << "source=" << fname << "&loop=" << (repeat? "true": "false")
       << ") >> >> ] >> ] >>\r\n";
  dict << ">>\r\n";
  annot_list (annotId)= dict;
}

void
pdf_hummus_renderer_rep::flush_videos () {
  ObjectsContext& oc= pdfWriter.GetObjectsContext();
  iterator<ObjectIDType> it= iterate (video_files);
  while (it->busy ()) {
    ObjectIDType id= it->next ();
    url v= video_files [id];
    string data;
    if (load_string (v, data, false)) {
      convert_error << "Cannot read video " << v << "\n";
      data= "";
    }
    string pre;
    pre << "<< /Type /EmbeddedFile /Subtype /video#2Fmp4"
        << " /Params << /Size " << as_string (N(data)) << " >>"
        << " /Length " << as_string (N(data)) << " >>\r\nstream\r\n";
    string post= "\r\nendstream\r\n";
    // write the (possibly large) video data without copying it
    oc.StartNewIndirectObject (id);
    IByteWriter* w= oc.StartFreeContext ();
    w->Write ((unsigned char*) &(pre[0]), N(pre));
    if (N(data) > 0) w->Write ((unsigned char*) &(data[0]), N(data));
    w->Write ((unsigned char*) &(post[0]), N(post));
    oc.EndFreeContext ();
    oc.EndIndirectObject ();
  }
}

void
pdf_hummus_renderer_rep::flush_dests()
{
  if (is_nil(dests)) return;

  // flush destinations
  
  string dict;
  dict << "<<\r\n";
  list<dest_data> it = dests;
  while (!is_nil(it)) {
    string label = it->item.x1;
    int dest_page = it->item.x2;
    int dest_x = it->item.x3;
    int dest_y = it->item.x4;
    {
      dict << "\t\t/label" << as_string(get_label_id(label)) << " [ " << as_string(page_id(dest_page)) << " 0 R /XYZ "
           << as_string(((double)default_dpi / dpi)*dest_x) << " " << as_string(((double)default_dpi / dpi)*dest_y) << " null ]\r\n";
    }
    it = it->next;
  }
  dict << ">>\r\n";
  {
    // flush the buffer
    ObjectsContext& objectsContext = pdfWriter.GetObjectsContext();
    destId = objectsContext.GetInDirectObjectsRegistry().AllocateNewObjectID();
    write_indirect_obj(objectsContext, destId, dict);
  }
}

void
pdf_hummus_renderer_rep::toc_entry (string kind, string title, SI x, SI y) {
  (void) kind; (void) title; (void) x; (void) y;
  // debug_convert << kind << ", " << title << "\n";
  int ls= 1;
  if (kind == "toc-strong-1") ls= 1;
  if (kind == "toc-strong-2") ls= 2;
  if (kind == "toc-1") ls= 3;
  if (kind == "toc-2") ls= 4;
  if (kind == "toc-3") ls= 5;
  if (kind == "toc-4") ls= 6;
  if (kind == "toc-5") ls= 7;

  // the title is written as a hexadecimal string, which needs no escapes
  outlines << outline_data(title, page_num, to_x(x), to_y(y+20*pixel), ls);
}

void pdf_hummus_renderer_rep::recurse (ObjectsContext& objectsContext, int parent_ls,
				       list<outline_data>& it, ObjectIDType parentId,
				       ObjectIDType& firstId, ObjectIDType& lastId, int &count)
{
  // weave the tangle of forward/backward references
  // are recurse over substructures
  
  ObjectIDType prevId = 0, curId = 0, nextId = 0;
  
  if (!is_nil(it))
    curId = objectsContext.GetInDirectObjectsRegistry().AllocateNewObjectID();

  firstId = curId;
  while (curId) {
    ObjectIDType subFirstId = 0, subLastId = 0;
    int subCount = 0;

    outline_data oitem = it->item;
    count++;
    it = it->next;
    
    if (!is_nil(it) && ((it->item).x5 > oitem.x5)) {
      // go down in level
      recurse(objectsContext, oitem.x5, it, curId, subFirstId, subLastId, subCount);
    }
    
    // continue at this level or above
    if (!is_nil(it) && ((it->item).x5 <= oitem.x5) && ((it->item).x5 > parent_ls)) {
      nextId = objectsContext.GetInDirectObjectsRegistry().AllocateNewObjectID();
    } else {
      // finished this level, go up.
      nextId = 0;
    }
    
    {
      // write current outline item dictionary
      string dict;
      dict << "<<\r\n"
           << "\t/Title " << utf8_to_pdf_hex_string((oitem).x1) << "\r\n"
           << "\t/Parent " << as_string(parentId) << " 0 R \r\n";
      if (prevId != 0) dict << "\t/Prev " << as_string(prevId) << " 0 R \r\n";
      if (nextId  != 0)  dict << "\t/Next " << as_string(nextId) << " 0 R \r\n";
      if (subCount > 0) {
        dict << "\t/First " << as_string(subFirstId) << " 0 R \r\n"
             << "\t/Last " << as_string(subLastId) << " 0 R \r\n"
             << "\t/Count " << as_string(-subCount) << "\r\n";
      }
      dict << "\t/Dest [ " << as_string(page_id((oitem).x2)) << " 0 R /XYZ "
           << as_string(((double)default_dpi / dpi)*((oitem).x3))
           << " " << as_string(((double)default_dpi / dpi)*((oitem).x4)) << " null ]\r\n"
           << ">>\r\n";
      write_indirect_obj(objectsContext, curId, dict);
    }
    prevId = curId; curId = nextId;
  }
  lastId = prevId;
}

void
pdf_hummus_renderer_rep::flush_outlines()
{
  if (is_nil(outlines)) return;
 
  ObjectsContext& objectsContext= pdfWriter.GetObjectsContext();
  ObjectIDType firstId= 0 , lastId= 0;
  int count = 0;
  list<outline_data> it = outlines;
  
  outlineId = objectsContext.GetInDirectObjectsRegistry().AllocateNewObjectID();
  recurse(objectsContext, 0, it, outlineId, firstId, lastId, count);
  {
    // create top-level outline dictionary
    string dict;
    dict << "<<\r\n";
    dict << "\t/Type /Outlines\r\n";
    dict << "\t/First " << as_string(firstId) << " 0 R \r\n";
    dict << "\t/Last " << as_string(lastId) << " 0 R \r\n";
    dict << "\t/Count " << as_string(count) << "\r\n";
    dict << ">>\r\n";
    write_indirect_obj(objectsContext, outlineId, dict);
  }
}

/******************************************************************************
* shadow rendering is trivial on pdf
******************************************************************************/

void
pdf_hummus_renderer_rep::set_metadata (string kind, string val) {
  metadata (kind)= val;
}

void
pdf_hummus_renderer_rep::flush_metadata () {
  if (N(metadata) == 0) return;
  DocumentContext& documentContext= pdfWriter.GetDocumentContext();
  TrailerInformation& trailerInfo= documentContext.GetTrailerInformation();
  InfoDictionary& info= trailerInfo.GetInfo();
  if (metadata->contains ("title"))
    info.Title= as_hummus_string (metadata ["title"]);
  if (metadata->contains ("author"))
    info.Author= as_hummus_string (metadata ["author"]);
  if (metadata->contains ("subject"))
    info.Subject= as_hummus_string (metadata ["subject"]);
  string creator= "TeXmacs " * string (TEXMACS_VERSION);
  string producer= creator * " + Hummus 4.0";
  info.Creator= utf8_as_hummus_string (creator);
  info.Producer= utf8_as_hummus_string (producer);
  PDFDate date; date.SetToCurrentTime ();
  info.CreationDate= date;
}

/******************************************************************************
* shadow rendering is trivial on pdf
******************************************************************************/

void
pdf_hummus_renderer_rep::fetch (SI x1, SI y1, SI x2, SI y2, renderer ren, SI x, SI y) {
  // debug_convert << "fetch\n";
  (void) x1; (void) y1; (void) x2; (void) y2;
  (void) ren; (void) x; (void) y;
}

void
pdf_hummus_renderer_rep::new_shadow (renderer& ren) {
  // debug_convert << "new_shadow\n";
  (void) ren;
}

void
pdf_hummus_renderer_rep::delete_shadow (renderer& ren) {
  // debug_convert << "delete_shadow\n";
  (void) ren;
}

void
pdf_hummus_renderer_rep::get_shadow (renderer ren, SI x1, SI y1, SI x2, SI y2) {
  // debug_convert << "get_shadow\n";
  (void) ren; (void) x1; (void) y1; (void) x2; (void) y2;
}

void
pdf_hummus_renderer_rep::put_shadow (renderer ren, SI x1, SI y1, SI x2, SI y2) {
  // debug_convert << "put_shadow\n";
  (void) ren; (void) x1; (void) y1; (void) x2; (void) y2;
}

void
pdf_hummus_renderer_rep::apply_shadow (SI x1, SI y1, SI x2, SI y2) {
  // debug_convert << "apply_shadow\n";
   (void) x1; (void) y1; (void) x2; (void) y2;
}

renderer
pdf_hummus_renderer_rep::shadow (picture& pic, SI x1, SI y1, SI x2, SI y2) {
  double old_zoomf= this->zoomf;
  set_zoom_factor (5.0 * PICTURE_ZOOM);
  renderer ren= renderer_rep::shadow (pic, x1, y1, x2, y2);
  set_zoom_factor (old_zoomf);
  return ren;
}


/******************************************************************************
* user interface
******************************************************************************/

renderer
pdf_hummus_renderer (url pdf_file_name, int dpi, int nr_pages,
                     string page_type, bool landscape, double paper_w, double paper_h)
{
  //cout << "Hummus print to " << pdf_file_name << " at " << dpi << " dpi\n";
  page_type= as_string (call ("standard-paper-size", object (page_type)));
  return tm_new<pdf_hummus_renderer_rep> (pdf_file_name, dpi, nr_pages,
			  page_type, landscape, paper_w, paper_h);
}
