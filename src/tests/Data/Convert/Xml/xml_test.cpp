
/******************************************************************************
* MODULE     : xml_test.cpp
* DESCRIPTION: Tests on TMML
* COPYRIGHT  : (C) 2019-2021  Darcy Shen
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "tm_test.hpp"

// is_xml_name is static in xml.cpp, which the test includes for it; the
// other functions of the file are renamed, so that this copy does not
// clash with that of the main build
#define tm_to_xml_name test_tm_to_xml_name
#define xml_name_to_tm test_xml_name_to_tm
#define old_tm_to_xml_cdata test_old_tm_to_xml_cdata
#define tm_to_xml_cdata test_tm_to_xml_cdata
#define old_xml_cdata_to_tm test_old_xml_cdata_to_tm
#define xml_unspace test_xml_unspace
#include "Xml/xml.cpp"

static void
check_tm_and_xml_name (string tm_name, string xml_name,
                       const char* file, int line) {
  test_check_eq (xml_name_to_tm (xml_name), tm_name,
                 "xml_name_to_tm (" * xml_name * ")", tm_name, file, line);
  test_check_eq (tm_to_xml_name (tm_name), xml_name,
                 "tm_to_xml_name (" * tm_name * ")", xml_name, file, line);
}

#define CHECK_TM_AND_XML_NAME(tm, xml) \
  check_tm_and_xml_name (tm, xml, __FILE__, __LINE__)

static void
test_is_xml_name () {
  CHECK (is_xml_name (':', 0));
  CHECK (is_xml_name ('_', 0));
  for (unsigned char c=0; c<128; c++)
    if (is_alpha (c))
      CHECK_MSG (is_xml_name (c, 0), string ((char) c) * " starts a name");
  CHECK (!is_xml_name ('-', 0));
  CHECK (!is_xml_name ('.', 0));
  CHECK (is_xml_name ('-', 1));
  CHECK (is_xml_name ('.', 1));
  for (unsigned char c=0; c<128; c++)
    if (is_digit (c)) {
      CHECK_MSG (is_xml_name (c, 1), string ((char) c) * " is in a name");
      CHECK_MSG (!is_xml_name (c, 0), string ((char) c) * " starts no name");
    }
}

static void
test_tm_and_xml_name () {
  // Normal characters
  CHECK_TM_AND_XML_NAME ("hello", "hello");
  // The '_' escape itself
  CHECK_TM_AND_XML_NAME ("_f", "_95_f");
  CHECK_TM_AND_XML_NAME ("_38_f", "_95_38_95_f");
  // Valid start char for XML
  CHECK_TM_AND_XML_NAME (":f", ":f");
  // Invalid start char for XML
  CHECK_TM_AND_XML_NAME ("&f", "_38_f");
  CHECK_TM_AND_XML_NAME (".f", "_46_f");
  CHECK_TM_AND_XML_NAME ("-f", "_45_f");
  // Invalid char for XML
  CHECK_TM_AND_XML_NAME ("f!", "f_33_");
}

int
main () {
  RUN (test_is_xml_name);
  RUN (test_tm_and_xml_name);
  return test_report ();
}
