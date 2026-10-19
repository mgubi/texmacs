/******************************************************************************
* MODULE     : converter_test.cpp
* DESCRIPTION: Properties of characters and strings
* COPYRIGHT  : (C) 2019 Darcy Shen
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include <QtTest/QtTest>

#include "converter.hpp"

class TestConverter: public QObject {
  Q_OBJECT

private slots:
  void test_utf8_to_cork();
  void test_iconv_replacement();
};

void TestConverter::test_utf8_to_cork() {
  QCOMPARE (as_charp (utf8_to_cork ("中")), "<#4E2D>");
  QCOMPARE (as_charp (utf8_to_cork ("“")), "\x10");
  QCOMPARE (as_charp (utf8_to_cork("”")), "\x11");
}

void TestConverter::test_iconv_replacement() {
  // a character which cannot be converted is replaced by U+FFFD, or by ?
  // when the target encoding has no U+FFFD, and the conversion goes on
  QCOMPARE (as_charp (convert ("caf\xe9 x", "UTF-8", "ISO-8859-1")),
            "caf? x");
  QCOMPARE (as_charp (convert ("a\xe2\x82\xac" "b", "UTF-8", "ISO-8859-1")),
            "a?b");
  QCOMPARE (as_charp (convert ("ab\xc3", "UTF-8", "ISO-8859-1")), "ab?");
  QCOMPARE (as_charp (convert ("a\xe9" "b", "ASCII", "UTF-8")),
            "a\xef\xbf\xbd" "b");
  QCOMPARE (as_charp (convert ("a\xe9" "b", "ASCII", "Cork")), "a<#FFFD>b");
  QCOMPARE (check_encoding ("a\xe9" "b", "UTF-8"), false);
  QCOMPARE (check_encoding ("caf\xc3\xa9", "UTF-8"), true);
}

QTEST_MAIN(TestConverter)
#include "converter_test.moc"
