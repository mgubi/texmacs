
/******************************************************************************
* MODULE     : keyword_parser_test.cpp
* DESCRIPTION: Properties of Keyword Parser
* COPYRIGHT  : (C) 2020-2021  Darcy Shen
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "keyword_parser.hpp"

#include <QtTest/QtTest>
#include "converter.hpp"

class TestKeywordParser: public QObject {
  Q_OBJECT

private slots:
  void test_can_parse();
  void test_phrase();
};

void TestKeywordParser::test_can_parse () {
  keyword_parser_rep keyword_parser= keyword_parser_rep ();
  int pos;
  keyword_parser.put ("key", "group");

  pos= 0;
  QVERIFY (keyword_parser.can_parse ("key group", pos));
}

void TestKeywordParser::test_phrase () {
  // keywords of several words, as "mutable struct" in julia
  keyword_parser_rep keyword_parser= keyword_parser_rep ();
  keyword_parser.put ("struct", "declare_type");
  keyword_parser.put ("mutable struct", "declare_type");
  QVERIFY (keyword_parser.can_parse ("mutable struct S", 0));
  int pos= 0;
  QVERIFY (keyword_parser.parse ("mutable struct S", pos));
  QCOMPARE (pos, 14);
  QVERIFY (keyword_parser.get ("mutable struct") == "declare_type");
  // not the first word alone, nor a phrase followed by more letters
  QVERIFY (!keyword_parser.can_parse ("mutable = 1", 0));
  QVERIFY (!keyword_parser.can_parse ("mutable  struct", 0));
  QVERIFY (!keyword_parser.can_parse ("mutable structs", 0));
  QVERIFY (keyword_parser.can_parse ("mutable struct", 0));
}

QTEST_MAIN(TestKeywordParser)
#include "keyword_parser_test.moc"
