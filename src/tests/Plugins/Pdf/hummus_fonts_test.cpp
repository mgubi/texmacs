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

class TestHummusFonts: public QObject {
  Q_OBJECT

private slots:
  void test_cff_encodings_info ();
  void test_type1_ps_string ();
};

// A custom CFF encoding of no codes (format 1, no ranges: STIX Two,
// Libertinus, EB Garamond) left mEncodingsCount undefined, and
// CFFEmbeddedFontWriter::WriteEncodings read that many codes through
// mEncoding, NULL. The fields are checked on memory which held something
// else before, as the heap does.
void
TestHummusFonts::test_cff_encodings_info () {
  alignas (EncodingsInfo) unsigned char mem[sizeof (EncodingsInfo)];
  memset (mem, 0xAA, sizeof (mem));
  EncodingsInfo* e= new (mem) EncodingsInfo ();
  QCOMPARE ((int) e->mEncodingsCount, 0);
  QVERIFY (e->mEncoding == NULL);
  QCOMPARE ((long long) e->mEncodingStart, 0LL);
  QCOMPARE ((long long) e->mEncodingEnd, 0LL);
  e->~EncodingsInfo ();
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
