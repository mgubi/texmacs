/******************************************************************************
* MODULE     : ai_stream_test.cpp
* DESCRIPTION: the streamed answers of the AI engines, parsed as they come
* COPYRIGHT  : (C) 2026  Massimiliano Gubinelli
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include <QtTest/QtTest>
#include "convert.hpp"
#include "analyze.hpp"

// A stream of server-sent events, as each engine sends it, fed in many
// small pieces (events cut anywhere, also in the middle of a character of
// UTF-8) to ai_stream_feed, which parses each line once, gives what the
// whole stream gives to ai_stream_text, at the end and on the way.

class TestAiStream: public QObject {
  Q_OBJECT

private slots:
  void test_openai ();
  void test_openrouter ();
  void test_claude ();
  void test_gemini ();
  void test_error ();
  void test_new_answer ();
  void test_time ();
};

static string
piece (int k) {
  // a word with a character of two bytes, and a newline from time to time
  return "w" * as_string (k) * (k % 7 == 0? string ("\xc3\xa9\\n"): string (" "));
}

static string
openai_stream (int n, bool openrouter) {
  string s;
  if (openrouter) s << ": OPENROUTER PROCESSING\n\n";
  for (int k= 0; k < n; k++) {
    if (openrouter && k % 50 == 0) s << ": OPENROUTER PROCESSING\n\n";
    s << "data: {\"choices\": [{\"delta\": {\"content\": \"" << piece (k)
      << "\"" << (openrouter && k < 20? string (", \"reasoning\": \"r\""):
                  string ("")) << "}}]}\n\n";
  }
  s << "data: {\"choices\": [], \"usage\": {\"prompt_tokens\": 10, "
    << "\"completion_tokens\": 5, \"cost\": 0.5}}\n\n";
  s << "data: [DONE]\n\n";
  return s;
}

static string
claude_stream (int n) {
  string s= "event: message_start\ndata: {\"type\": \"message_start\", "
    "\"message\": {\"model\": \"claude-x\", \"usage\": {\"input_tokens\": 3}}}\n\n";
  for (int k= 0; k < 20; k++)
    s << "event: content_block_delta\r\ndata: {\"type\": \"content_block_delta\", "
      << "\"delta\": {\"type\": \"thinking_delta\", \"thinking\": \"t" << as_string (k)
      << " \"}}\r\n\r\n";
  for (int k= 0; k < n; k++)
    s << "event: content_block_delta\r\ndata: {\"type\": \"content_block_delta\", "
      << "\"delta\": {\"type\": \"text_delta\", \"text\": \"" << piece (k)
      << "\"}}\r\n\r\n";
  s << "event: message_stop\ndata: {\"type\": \"message_stop\"}\n\n";
  return s;
}

static string
gemini_stream (int n) {
  string s;
  for (int k= 0; k < n; k++)
    s << "data: {\"candidates\": [{\"content\": {\"parts\": [{\"text\": \""
      << piece (k) << "\"}]}}]}\r\n\r\n";
  return s;
}

// the stream fed in pieces of 1 to 37 bytes; each result is checked
// against the whole prefix for some of them, and at the end
static void
check_pieces (string s, string model) {
  ai_stream_state st;
  int i= 0, k= 0;
  string err, reasoning, r;
  while (i < N(s)) {
    int len= 1 + (k * 7919) % 37;
    i= min (N(s), i + len);
    r= ai_stream_feed (st, s (0, i), model, err, reasoning);
    if (k % 97 == 0) {
      string err2, reasoning2;
      string r2= ai_stream_text (s (0, i), model, err2, reasoning2);
      QVERIFY (r == r2);
      QVERIFY (reasoning == reasoning2);
      QVERIFY (err == err2);
    }
    k++;
  }
  string err2, reasoning2;
  string r2= ai_stream_text (s, model, err2, reasoning2);
  QVERIFY (r == r2);
  QVERIFY (reasoning == reasoning2);
  QVERIFY (err == err2);
  QVERIFY (N(r) > 0);
}

void TestAiStream::test_openai () {
  string s= openai_stream (2000, false);
  check_pieces (s, "chatgpt");
  string err, reasoning;
  QVERIFY (starts (ai_stream_text (s, "chatgpt", err, reasoning),
                   "w0\xc3\xa9\nw1 w2 "));
}

void TestAiStream::test_openrouter () {
  string s= openai_stream (2000, true);
  check_pieces (s, "openrouter");
  string err, reasoning;
  (void) ai_stream_text (s, "openrouter", err, reasoning);
  QVERIFY (N(reasoning) == 20);
}

void TestAiStream::test_claude () {
  check_pieces (claude_stream (2000), "claude");
  string err, reasoning;
  string r= ai_stream_text (claude_stream (3), "claude", err, reasoning);
  QVERIFY (r == "w0\xc3\xa9\nw1 w2 ");
  QVERIFY (starts (reasoning, "t0 t1 "));
}

void TestAiStream::test_gemini () {
  check_pieces (gemini_stream (2000), "gemini");
}

void TestAiStream::test_error () {
  string s= openai_stream (30, true);
  s << "data: {\"error\": {\"message\": \"Provider returned error\"}}\n\n";
  check_pieces (s, "openrouter");
  ai_stream_state st;
  string err, reasoning;
  (void) ai_stream_feed (st, s, "openrouter", err, reasoning);
  QVERIFY (err == "Provider returned error");
}

// another answer in the same state (the link resets it, but an output
// which is not the continuation is parsed again from its beginning)
void TestAiStream::test_new_answer () {
  ai_stream_state st;
  string err, reasoning;
  string a= openai_stream (10, false), b= gemini_stream (3);
  (void) ai_stream_feed (st, a, "chatgpt", err, reasoning);
  string r= ai_stream_feed (st, b, "gemini", err, reasoning);
  string r2= ai_stream_text (b, "gemini", err, reasoning);
  QVERIFY (r == r2);
}

// the time of 2000 pieces: each prefix parsed again (as before), and fed
static void
time_both (string s, string model, const char* name) {
  int n= 2000, step= max (1, N(s) / n);
  QElapsedTimer t;
  t.start ();
  long long scanned= 0;
  string err, reasoning;
  for (int i= step; i <= N(s); i += step) {
    (void) ai_stream_text (s (0, i), model, err, reasoning);
    scanned += i;
  }
  qint64 old_ms= t.elapsed ();
  t.restart ();
  ai_stream_state st;
  for (int i= step; i <= N(s); i += step)
    (void) ai_stream_feed (st, s (0, i), model, err, reasoning);
  qint64 new_ms= t.elapsed ();
  printf ("ai_stream %s: %d bytes in 2000 pieces: each prefix parsed "
          "again %lld ms (%lld bytes scanned), fed %lld ms (%d bytes)\n",
          name, N(s), (long long) old_ms, scanned, (long long) new_ms, N(s));
  fflush (stdout);
}

// (only with TM_AI_STREAM_TIME set: parsing each prefix again takes long)
void TestAiStream::test_time () {
  if (qgetenv ("TM_AI_STREAM_TIME").isEmpty ())
    QSKIP ("set TM_AI_STREAM_TIME to measure");
  time_both (openai_stream (2000, true), "openrouter", "OpenRouter");
  time_both (claude_stream (2000), "claude", "Claude");
  time_both (gemini_stream (2000), "gemini", "Gemini");
}

QTEST_MAIN(TestAiStream)
#include "ai_stream_test.moc"
