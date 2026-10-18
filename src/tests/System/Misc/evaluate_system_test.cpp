/******************************************************************************
* MODULE     : evaluate_system_test.cpp
* DESCRIPTION: the inputs of a command (passphrases) overwritten once sent
* COPYRIGHT  : (C) 2026  Massimiliano Gubinelli
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include <QtTest/QtTest>
#include "sys_utils.hpp"

class TestEvaluateSystem: public QObject {
  Q_OBJECT

private slots:
  void test_input_wiped ();
};

// evaluate-system gives GnuPG its passphrase on a pipe: the command gets
// it, and the string of the call (made by the glue) is overwritten after
void
TestEvaluateSystem::test_input_wiped () {
#if !defined (OS_MINGW) && !defined (OS_ANDROID)
  array<string> cmd;
  cmd << string ("cat");
  array<int> fd_in; fd_in << 0;
  string secret ("pass-phrase");
  array<string> in; in << secret;
  array<int> fd_out; fd_out << 1;
  array<string> r= evaluate_system (cmd, fd_in, in, fd_out);
  QVERIFY (r[0] == "0");
  QVERIFY (r[1] == "pass-phrase");
  QCOMPARE (N(secret), 11);
  for (int i= 0; i < N(secret); i++) QCOMPARE ((int) secret[i], 0);
#endif
}

QTEST_MAIN(TestEvaluateSystem)
#include "evaluate_system_test.moc"
