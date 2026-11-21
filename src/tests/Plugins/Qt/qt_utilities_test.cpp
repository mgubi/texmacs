
/******************************************************************************
* MODULE     : qt_utilities_test.cpp
* COPYRIGHT  : (C) 2019  Darcy Shen
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include <QtTest/QtTest>
#include "Qt/qt_utilities.hpp"
#include "converter.hpp"


class TestQtUtilities: public QObject {
  Q_OBJECT

private slots:
  void test_qt_supports();
  void test_widget_text();
};

void TestQtUtilities::test_qt_supports () {
#ifdef QTTEXMACS
  QVERIFY (qt_supports (url ("x.svg")));
  QVERIFY (qt_supports (url ("x.png")));
#endif
}

// The text of labels keeps its tabs and newlines; the value of a field
// comes back as it was read by from_qstring, which writes the macron and
// the dot accent as the Cork bytes 0x09 and 0x0A (#439)
void TestQtUtilities::test_widget_text () {
  QString glyphs= QString::fromUtf8 ("x\xc2\xaf\xcb\x99y");
  string v= from_qstring (glyphs);
  QCOMPARE (N(v), 4);
  QVERIFY (v[1] == '\t' && v[2] == '\n');
  QCOMPARE (to_qstring_value (v), glyphs);
  QCOMPARE (from_qstring (to_qstring_value (v)), v);
  array<string> a;
  a << v;
  QCOMPARE (to_qstringlist_value (a), QStringList (glyphs));
  QCOMPARE (to_qstring ("a\nb\tc"), QString ("a\nb\tc"));
  QCOMPARE (to_qstring ("<alpha>"), QString::fromUtf8 ("\xce\xb1"));
  QCOMPARE (to_qstring_value ("<alpha>"), QString::fromUtf8 ("\xce\xb1"));
}

QTEST_MAIN(TestQtUtilities)
#include "qt_utilities_test.moc"
