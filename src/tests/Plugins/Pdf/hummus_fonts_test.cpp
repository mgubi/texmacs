/******************************************************************************
* MODULE     : hummus_fonts_test.cpp
* DESCRIPTION: the font parsers of the vendored PDFHummus (#961)
* COPYRIGHT  : (C) 2026  Massimiliano Gubinelli
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include <QtTest/QtTest>
#include <new>
#include <string>
#include <cstring>

#include "Pdf/PDFWriter/CFFFileInput.h"
#include "Pdf/PDFWriter/Type1Input.h"
#include "Pdf/PDFWriter/InputStringStream.h"
#include "Pdf/PDFWriter/InputByteArrayStream.h"

class TestHummusFonts: public QObject {
  Q_OBJECT

private slots:
  void test_cff_encodings_info ();
  void test_cff_encoding_ranges ();
  void test_type1_ps_string ();
};

// A custom CFF encoding of no codes (format 1, no ranges: STIX Two,
// Libertinus, EB Garamond) left mEncodingsCount undefined, and
// CFFEmbeddedFontWriter::WriteEncodings read that many codes through
// mEncoding, NULL. The fields are checked on memory which held something
// else before, as the heap does (written through a volatile pointer: an
// ordinary memset before the constructor may be dropped by the compiler,
// GCC's -flifetime-dse).
void
TestHummusFonts::test_cff_encodings_info () {
  alignas (EncodingsInfo) unsigned char mem[sizeof (EncodingsInfo)];
  volatile unsigned char* v= mem;
  for (size_t i=0; i<sizeof (mem); i++) v[i]= 0xAA;
  EncodingsInfo* e= new (mem) EncodingsInfo ();
  QCOMPARE ((int) e->mEncodingsCount, 0);
  QVERIFY (e->mEncoding == NULL);
  QCOMPARE ((long long) e->mEncodingStart, 0LL);
  QCOMPARE ((long long) e->mEncodingEnd, 0LL);
  e->~EncodingsInfo ();
}

// A CFF font (made with fontTools) of the glyphs .notdef A B C X whose
// encoding is of format 1, two ranges: 65 and 2 more (A B C), 88 (X). A
// range holds nLeft+1 codes: the reader counted nLeft, 2 codes for 4.
static unsigned char test_cff[]= {
  0x01, 0x00, 0x04, 0x01, 0x00, 0x01, 0x01, 0x01, 0x08, 0x54, 0x65, 0x73,
  0x74, 0x43, 0x46, 0x46, 0x00, 0x01, 0x01, 0x01, 0x13, 0xf8, 0x1b, 0x02,
  0x8b, 0x8b, 0xef, 0xef, 0x05, 0xc0, 0x0f, 0xc7, 0x10, 0x8b, 0xf7, 0x07,
  0x12, 0xcd, 0x11, 0x00, 0x01, 0x01, 0x01, 0x08, 0x54, 0x65, 0x73, 0x74,
  0x43, 0x46, 0x46, 0x00, 0x00, 0x01, 0x00, 0x22, 0x02, 0x00, 0x39, 0x00,
  0x01, 0x02, 0x41, 0x02, 0x58, 0x00, 0x00, 0x05, 0x01, 0x01, 0x09, 0x11,
  0x19, 0x21, 0x29, 0xf8, 0x88, 0x8b, 0x16, 0xef, 0xef, 0x06, 0x0e, 0xf8,
  0x88, 0x8b, 0x16, 0xef, 0xef, 0x06, 0x0e, 0xf8, 0x88, 0x8b, 0x16, 0xef,
  0xef, 0x06, 0x0e, 0xf8, 0x88, 0x8b, 0x16, 0xef, 0xef, 0x06, 0x0e, 0xf8,
  0x88, 0x8b, 0x16, 0xef, 0xef, 0x06, 0x0e
};

static void
encoding_of (unsigned char* data, unsigned long n, int& count, std::string& codes) {
  InputByteArrayStream in (data, n);
  CFFFileInput cff;
  QCOMPARE ((int) cff.ReadCFFFile (&in), (int) PDFHummus::eSuccess);
  EncodingsInfo* e= cff.mTopDictIndex[0].mEncoding;
  QVERIFY (e != NULL);
  count= e->mEncodingsCount;
  codes.clear ();
  for (int i=0; i<count && e->mEncoding != NULL; i++)
    codes += (char) e->mEncoding[i];
}

void
TestHummusFonts::test_cff_encoding_ranges () {
  int count; std::string codes;
  encoding_of (test_cff, sizeof (test_cff), count, codes);
  QCOMPARE (count, 4);
  QCOMPARE (QString::fromStdString (codes), QString ("ABCX"));
  // the same encoding with no ranges (as in STIX Two): no codes
  unsigned char empty[sizeof (test_cff)];
  memcpy (empty, test_cff, sizeof (test_cff));
  QCOMPARE ((int) empty[60], 1);   // format 1, at the offset of the encoding
  empty[61]= 0;                    // nRanges
  encoding_of (empty, sizeof (empty), count, codes);
  QCOMPARE (count, 0);
}

// A Type 1 font of one segment of clear text (then an empty binary one and
// the end), whose /Notice has escapes: at each escape FromPSString read one
// character more beyond the end of the string
static std::string
pfb_segment (int type, const std::string& data) {
  std::string s;
  s += (char) 0x80;
  s += (char) type;
  unsigned long n= data.size ();
  for (int i=0; i<4; i++) s += (char) ((n >> (8*i)) & 0xff);
  return s + data;
}

void
TestHummusFonts::test_type1_ps_string () {
  std::string clear=
    "%!PS-AdobeFont-1.0: TestFont 001\n"
    "11 dict begin\n"
    "/FontInfo 3 dict dup begin\n"
    "/Notice (a\\(b\\) \\\\c \\101d) readonly def\n"
    "/Copyright () readonly def\n"
    "end readonly def\n"
    "/FontName /TestFont def\n"
    "end\n"
    "currentfile eexec\n";
  std::string pfb= pfb_segment (1, clear) + pfb_segment (2, "") +
                   std::string ("\x80\x03", 2);
  InputStringStream in (pfb);
  Type1Input t1;
  t1.ReadType1File (&in);   // (the font has no Private dictionary)
  QCOMPARE (QString::fromStdString (t1.mFontInfoDictionary.Notice),
            QString ("a(b) \\c Ad"));
  QCOMPARE (QString::fromStdString (t1.mFontInfoDictionary.Copyright),
            QString (""));
  QCOMPARE (QString::fromStdString (t1.mFontDictionary.FontName),
            QString ("TestFont"));
}

QTEST_MAIN(TestHummusFonts)
#include "hummus_fonts_test.moc"
