
/******************************************************************************
* MODULE     : ai.cpp
* DESCRIPTION: interface for AI big language model
* COPYRIGHT  : (C) 2025  Joris van der Hoeven
*                  2026  Gregoire Lecerf
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "convert.hpp"
#include "converter.hpp"
#include "locale.hpp"
#include "wencoding.hpp"
#include "vars.hpp"
#include "drd_std.hpp"
#include "analyze.hpp"
#include "file.hpp"
#include "scheme.hpp"
#include "web_files.hpp"
#include "base64.hpp"

/******************************************************************************
* Various engines
******************************************************************************/

string
ai_engine (string model) {
  if (starts (model, "chatgpt")) return "chatgpt";
  if (starts (model, "gemini")) return "gemini";
  if (starts (model, "ollama")) return "ollama";
  if (starts (model, "open-mistral")) return "mistral";
  if (starts (model, "albert")) return "albert";
  if (starts (model, "claude")) return "claude";
  if (starts (model, "openrouter")) return "openrouter";
  return "unknown";
}

/******************************************************************************
* Useful syntactic subroutines
******************************************************************************/

string
ai_quote (string s) {
  int i, n= N(s);
  string r;
  for (i=0; i<n; i++)
    switch (s[i]) {
    case '\"':
      r << '\\' << s[i];
      break;
    case '\n':
      r << "\\n";
      break;
    case '\'':
      r << "'\\''";
      break;
    case '\\':
      r << '\\' << s[i];
      break;
    default:
      r << s[i];
    }
  return r;
}

string
ai_unquote (string s) {
  int i, n= N(s);
  string r;
  for (i=0; i<n; i++)
    if (s[i] == '\\' && (i+1 < n)) {
      if (s[i+1] == '\\' || s[i+1] == '\"' || s[i+1] == '\'')
	r << s[++i];
      else if (s[i+1] == 'n') {
	r << '\n'; i++;
      }
    }
    else r << s[i];
  return r;
}

/******************************************************************************
* TikZ pictures
******************************************************************************/

static bool
run_pdflatex (url tex) {
  if (!exists_in_path ("pdflatex")) {
    static bool warned= false;
    if (!warned) {
      convert_warning <<
	"pdflatex is not installed: TikZ pictures cannot be rendered" << LF;
      warned= true;
    }
    return false;
  }
  array<string> cmd;
  cmd << string ("pdflatex");
  cmd << string ("-output-directory=") * sys_concretize (head (tex));
  cmd << concretize (tex);
  //cout << cmd << LF;
  array<int> out; out << 1; out << 2;
  array<string> ret= evaluate_system (cmd, array<int> (),
				      array<string> (), out);
  //cout << "ret= " << ret << LF;
  if (ret [0] != "0" || ret[2] != "") {
    convert_warning << "cannot render TikZ picture" << LF;
    convert_warning << ret[1] << LF;
    convert_warning << ret[2] << LF;
    return false;
  }
  return true;
}

static string
replace_tikz_by_pdf (string s) {
  static int counter= 0;
  counter++;
  const string document_class_tag ("\\documentclass");
  const string beg_document_tag ("\\begin{document}");
  const string end_document_tag ("\\end{document}");
  const string beg_tikz_tag ("\\begin{tikzpicture}");
  const string end_tikz_tag ("\\end{tikzpicture}");
  int beg_document_class_pos= search_forwards (document_class_tag, s);
  if (beg_document_class_pos < 0) return s;
  int end_document_class_pos= beg_document_class_pos; 
  while (end_document_class_pos < N(s) &&
	 s[end_document_class_pos] != '}') end_document_class_pos++;
  if (end_document_class_pos == N(s)) return s;
  end_document_class_pos++;
  int beg_document_pos= search_forwards (beg_document_tag, s);
  int end_document_pos= search_forwards (end_document_tag, s);
  if (beg_document_pos < 0 || end_document_pos < 0) return s;
  int beg_tikz_pos= search_forwards (beg_tikz_tag, s);
  int end_tikz_pos= search_forwards (end_tikz_tag, s);
  if (beg_tikz_pos < 0 || end_tikz_pos < 0) return s;
  string r= string ("\\documentclass[border=3pt]{standalone}\n") *
    s (end_document_class_pos, beg_document_pos) *
    string ("\n") *  "\\usepackage{amsfonts}\n" *
    "\\usetikzlibrary{calc}\n" *
    beg_document_tag * string ("\n") *
    s (beg_tikz_pos, end_tikz_pos + N(end_tikz_tag)) *
    string ("\n") * end_document_tag * string ("\n");
  url temp= url_temp_dir ();
  url tex= temp * (as_string (counter) * ".tex");
  save_string (tex, r);
  if (!run_pdflatex (tex)) return s;
  url pdf= temp * (as_string (counter) * ".pdf");
  string pdf_str= replace (as_string (pdf), "\\", "/");
  string aux= s (0, beg_tikz_pos) *
    string ("\n") * "\\includegraphics{" * pdf_str * "}\n" *
    s (end_tikz_pos + N(end_tikz_tag), N(s));
  return replace_tikz_by_pdf (aux);
}

static string
extract_svg (string s) {
  //cout << s << LF;
  string beg_file_tag ("\\begin{filecontents*}");
  string end_file_tag ("\\end{filecontents*}");
  int beg_file_pos= search_forwards (beg_file_tag, s);
  int end_file_pos= search_forwards (end_file_tag, s);
  if (beg_file_pos < 0 || end_file_pos < 0) {
    beg_file_tag= "\\begin{filecontent*}";
    end_file_tag= "\\end{filecontent*}";
    beg_file_pos= search_forwards (beg_file_tag, s);
    end_file_pos= search_forwards (end_file_tag, s);
  }
  if (beg_file_pos < 0 || end_file_pos < 0) return s;
  int beg_name_pos= beg_file_pos + N(beg_file_tag);
  while (beg_name_pos < N(s) && s[beg_name_pos] != '{') beg_name_pos++;
  if (beg_name_pos >= N(s)) return s;
  beg_name_pos++;
  int end_name_pos= search_forwards ("}", beg_name_pos, s);
  if (beg_name_pos < 0 || end_name_pos < 0) return s;
  string file= trim_spaces (s (end_name_pos+1, end_file_pos));
  //cout << "file=" << file << LF;
  string name= trim_spaces (s (beg_name_pos, end_name_pos));
  //cout << "name= " << name << LF;
  url temp= url_temp_dir ();
  url f= temp * name;
  //cout << "f= " << as_string (f) << LF;
  save_string (f, file);
  string ret= s(0, beg_file_pos)
    * s (end_file_pos + N(end_file_tag), N(s));
  ret= replace (ret, "\\includesvg", "\\includegraphics");
  string f_str= replace (as_string (f), "\\", "/");
  ret= replace (ret, "{" * name * "}", "{" * f_str * "}");
  if (ends (name, ".svg")) {
    string b= name (0, N(name) - 4);
    ret= replace (ret, "{" * b * "}", "{" * f_str * "}");
  }
  //cout << "---\n" << ret <<"\n---\n";
  return extract_svg (ret);
}

/******************************************************************************
* History management
******************************************************************************/

// By ID

hashmap<string,string> ai_last_id ("");

void
ai_get_continuation (string& s, string model, string chat) {
  if (chat == "") return;
  //cout << "model= " << model << "\n";
  if (starts (model, "none")) {
    string key= model * "-" * chat;
    if (ai_last_id->contains (key)) {
      string id= ai_last_id[key];
      s= "Please follow up on your last answer with ID " * id * ". " * s;
    }
  }
}

void
ai_set_continuation (string s, string model, string chat) {
  if (chat == "") return;
  if (starts (model, "none")) {
    int pos= search_forwards ("\"id\":\"", s);
    if (pos < 0) return;
    pos += 6;
    int end= search_forwards ("\"", pos, s);
    if (end < 0) return;
    string key= model * "-" * chat;
    ai_last_id (key)= s (pos, end);
  }
}

// For albert, by passing previous prompts and answers

static const int ai_default_history_size= 3;
static hashmap<string,string> ai_current_prompt ("");
static list<string> null_string_list;
static hashmap<string,list<string> > ai_last_prompts (null_string_list);
static hashmap<string,list<string> > ai_last_answers (null_string_list);

static int
ai_get_history_size () {
  // the setting of the plug-ins (in the preferences of Albert)
  string s= get_preference ("albert chat history size");
  if (is_int (s)) return as_int (s);
  return ai_default_history_size;
}

static void
ai_set_current_prompt (string s, string model, string chat) {
  if (chat == "") return;
  string key= model * "-" * chat;
  ai_current_prompt(key)= s;
}

static string
ai_get_current_prompt (string model, string chat) {
  if (chat == "") return "";
  string key= model * "-" * chat;
  return ai_current_prompt[key];
}

static void
ai_set_last_prompt (string s, string model, string chat) {
  if (chat == "") return;
  string key= model * "-" * chat;
  list<string> l (s, ai_last_prompts[key]);
  ai_last_prompts(key)= l;
  const int max_size= ai_get_history_size ();
  if (N(ai_last_prompts[key]) > max_size)
    ai_last_prompts(key)= head (ai_last_prompts[key], max_size); 
}

static list<string>
ai_get_last_prompts (string model, string chat) {
  if (chat == "") return null_string_list;
  string key= model * "-" * chat;
  const int max_size= ai_get_history_size ();
  if (N(ai_last_prompts[key]) > max_size)
    ai_last_prompts[key]= head (ai_last_prompts[key], max_size);
  return ai_last_prompts[key];
}

static void
ai_set_last_answer (string s, string model, string chat) {
  if (chat == "") return;
  string key= model * "-" * chat;
  list<string> l (s, ai_last_answers[key]);
  ai_last_answers(key)= l;
  const int max_size= ai_get_history_size ();
  if (N(ai_last_answers[key]) > max_size)
    ai_last_answers(key)= head (ai_last_answers[key], max_size); 
}

static list<string>
ai_get_last_answers (string model, string chat) {
  if (chat == "") return null_string_list;
  string key= model * "-" * chat;
  const int max_size= ai_get_history_size ();
  if (N(ai_last_answers[key]) > max_size)
    ai_last_answers[key]= head (ai_last_answers[key], max_size);
  return ai_last_answers[key];
}

/******************************************************************************
* Command helpers
******************************************************************************/

static void
get_post_data (string& url, array<string>& headers, tree& data,
	       tree t) {
  url= t[0]->label;
  data= t[2];
  headers= array<string> ();
  for (int i= 0; i < N(t[1]); i++)
    if (is_atomic (t[1][i])) headers << t[1][i]->label;
}

static inline string
shell_quote (string s) {
  return "'" * replace (s, "'", "'\\''") * "'";
}

static string
to_shell_command (tree t) {
  if (is_compound (t, "eval_system", 1) && is_atomic (t[0]))
    return t[0]->label;
  if (is_compound (t, "http_post", 3) && is_atomic (t[0])
      && is_tuple (t[1])) {    
    string url; tree data; array<string> headers;
    get_post_data (url, headers, data, t);
    string cmd= "curl --silent -X POST " * shell_quote (url) * " \\\n";
    for (int i= 0; i+1 < N(headers); i += 2)
      cmd << "  -H " << shell_quote (headers[i])
	  << ":"  << shell_quote (headers[i+1]) << " \\\n";
    cmd << "  --data-binary " << shell_quote (tree_to_json (data));
    return cmd;
  }
  io_error << "as_shell_command, unknown command type: " << t << LF;
  return "";
}

string
ai_eval_command (tree t) {
  // cout << "ai_eval_command, " << t << LF;
  if (is_compound (t, "eval_system", 1) && is_atomic (t[0]))
    return eval_system (t[0]->label);
  if (is_compound (t, "http_post", 3) && is_atomic (t[0])
      && is_tuple (t[1])) {
    string url; tree data; array<string> headers;
    get_post_data (url, headers, data, t);
    return http_post_json (url, headers, data);
  }
  io_error << "ai_eval_command, wrong command: " << t << LF;
  return "";
}

bool
ai_async_eval_command (tree t, object callback) {
  if (is_compound (t, "eval_system", 1) && is_atomic (t[0]))
    return async_eval_system (t[0]->label, callback);
  if (is_compound (t, "http_post", 3) && is_atomic (t[0])
      && is_tuple (t[1])) {
    string url; tree data; array<string> headers; 
    get_post_data (url, headers, data, t);
    return async_http_post_json (url, headers, data, callback);
  }
  io_error << "ai_eval_command, wrong command: " << t << LF;
  return "";
}

/******************************************************************************
* Producing the query command for various engines
******************************************************************************/

// Every engine is asked by an HTTP request with a JSON body (http_post): by
// the request link of its plug-in (request_link.cpp), with Qt, curl or, in
// a web browser, fetch (web_files.cpp). The agent (what the engine is asked
// to be) is the instruction of the system, and the last prompts and answers
// of the chat come before the new prompt. All of them answer the requests of
// a web page (CORS) but Albert; Ollama does when the page is among its
// OLLAMA_ORIGINS.

// In a web browser the answer of a session (ai_latex_request) is streamed
// (server-sent events, see ai_stream_text): the request link shows it as it
// comes (request_link_rep::partial). The other requests (translate,
// correct), and those of the desktop, have the answer at once.
static bool ai_stream= false;

static string
ai_key (string engine, string env) {
  return as_string (call ("ai-api-key", engine, env));
}

static string
ai_model_name (string engine, string fallback) {
  string m= get_preference (engine * " model", fallback);
  if (m == "" || m == "default") m= fallback;
  return m;
}

// the models which draw pictures (PNG, JPEG): those of Gemini whose name
// says "image", gpt-image and dall-e of OpenAI
static bool
ai_image_model (string name) {
  return occurs ("image", name) || starts (name, "dall-e");
}

// their instructions: the text of the LaTeX ones would have them draw in
// TikZ, and their answer is a picture, with a few words
static string ai_image_agent=
  "You are inside GNU TeXmacs, a scientific editor. Draw the picture "
  "which is asked for as an image, and say in a sentence or two what it "
  "shows, in plain text, in the language of the question.";

// the agent is the default instructions (those of the user are kept)
static bool
ai_default_agent (string model, string agent) {
  return agent == as_string (call ("ai-default-instructions", model));
}

// the conversation: (role, text) pairs, the last one the prompt. In a
// session it is the one of the session (ai-session-context in init-ai.scm:
// the questions and the answers of the fields above, as LaTeX), else the
// last prompts and answers kept here.
static array<string>
ai_conversation (string s, string model, string chat, bool history) {
  array<string> v;
  if (history) {
    ai_set_current_prompt (s, model, chat);
    object c= call ("ai-session-context", model, chat);
    if (is_array_string (c)) {
      array<string> a= as_array_string (c);
      for (int i= 0; i+1 < N(a); i += 2)
        v << string ("user") << a[i] << string ("assistant") << a[i+1];
      v << string ("user") << s;
      return v;
    }
    list<string> last_prompts= reverse (ai_get_last_prompts (model, chat));
    list<string> last_answers= reverse (ai_get_last_answers (model, chat));
    while (!is_nil (last_prompts) && !is_nil (last_answers)) {
      v << string ("user") << last_prompts->item;
      v << string ("assistant") << last_answers->item;
      last_prompts= last_prompts->next;
      last_answers= last_answers->next;
    }
  }
  v << string ("user") << s;
  return v;
}

// the messages of the APIs of OpenAI (also those of Mistral, Albert, Ollama)
static tree
openai_messages (string agent, array<string> conv) {
  array<tree> v;
  if (agent != "") v << json_object ("role", "system", "content", agent);
  for (int i= 0; i+1 < N(conv); i += 2)
    v << json_object ("role", conv[i], "content", conv[i+1]);
  return json_array (v);
}

static tree
openai_style_command (string url, array<string> headers, string model_name,
                      string agent, array<string> conv, bool images= false) {
  array<tree> d (tree ("model"), tree (model_name),
                 tree ("messages"), openai_messages (agent, conv));
  if (images) {
    array<tree> m (tree ("image"), tree ("text"));
    d << tree ("modalities") << json_array (m);
  }
  if (ai_stream) d << tree ("stream") << compound ("json-boolean", "true");
  tree data= json_object (d);
  tree h (TUPLE);
  for (int i= 0; i < N(headers); i++) h << headers[i];
  return compound ("http_post", url, h, data);
}

tree
chatgpt_command (string s, string model, string agent,
                 string chat, bool history) {
  string key= ai_key ("chatgpt", "OPENAI_API_KEY");
  string name= ai_model_name ("chatgpt", "gpt-5-mini");
  if (ai_image_model (name)) {
    // a picture (b64_json in the answer): the prompt alone, not streamed
    // (the instructions of the user, if they changed them, come first)
    string prompt= s;
    if (agent != "" && !ai_default_agent (model, agent))
      prompt= agent * "\n\n" * s;
    array<tree> d (tree ("model"), tree (name), tree ("prompt"), tree (prompt));
    d << tree ("n") << compound ("json-number", "1");
    if (starts (name, "dall-e"))
      d << tree ("response_format") << tree ("b64_json");
    tree h (TUPLE);
    h << tree ("Authorization") << tree ("Bearer " * key)
      << tree ("Content-Type") << tree ("application/json");
    return compound ("http_post",
                     "https://api.openai.com/v1/images/generations", h,
                     json_object (d));
  }
  return openai_style_command (
    "https://api.openai.com/v1/chat/completions",
    array<string> ("Authorization", "Bearer " * key,
                   "Content-Type", "application/json"),
    name, agent,
    ai_conversation (s, model, chat, history));
}

tree
mistral_command (string s, string model, string agent,
                 string chat, bool history) {
  string key= ai_key ("open-mistral-7b", "MISTRAL_API_KEY");
  return openai_style_command (
    "https://api.mistral.ai/v1/chat/completions",
    array<string> ("Authorization", "Bearer " * key,
                   "Content-Type", "application/json"),
    ai_model_name ("open-mistral-7b", "mistral-small-latest"), agent,
    ai_conversation (s, model, chat, history));
}

tree
albert_command (string s, string model, string agent,
		string chat, bool history) {
  string key= ai_key ("albert", "ALBERT_API_KEY");
  return openai_style_command (
    "https://albert.api.etalab.gouv.fr/v1/chat/completions",
    array<string> ("Authorization", "Bearer " * key,
                   "Content-Type", "application/json"),
    get_preference (model * " model", model), agent,
    ai_conversation (s, model, chat, history));
}

// the API of Ollama which is that of OpenAI
tree
ollama_command (string s, string model, string agent,
                string chat, bool history) {
  string server= get_preference ("ollama server", "localhost");
  string port  = get_preference ("ollama port", "11434");
  string model_= get_preference ("ollama model", "default");
  if (model_ == "default" || model_ == "")
    model_= as_string (call ("ollama-default-model"));
  return openai_style_command (
    "http://" * server * ":" * port * "/v1/chat/completions",
    array<string> ("Content-Type", "application/json"),
    model_, agent, ai_conversation (s, model, chat, history));
}

// the API of OpenRouter, which is that of OpenAI, for the models of many
// providers (named provider/model); those which draw give their images as
// data URLs (message.images), when asked for them (modalities)
tree
openrouter_command (string s, string model, string agent,
                    string chat, bool history) {
  string key= ai_key ("openrouter", "OPENROUTER_API_KEY");
  string name= ai_model_name ("openrouter", "openrouter/auto");
  bool image= ai_image_model (name);
  if (image && ai_default_agent (model, agent)) agent= ai_image_agent;
  array<string> h ("Authorization", "Bearer " * key,
                   "Content-Type", "application/json");
  // (who asks, which OpenRouter shows with the use of the key)
  h << string ("HTTP-Referer") << string ("https://github.com/mgubi/texmacs")
    << string ("X-Title") << string ("GNU TeXmacs");
  return openai_style_command (
    "https://openrouter.ai/api/v1/chat/completions", h,
    name, agent, ai_conversation (s, model, chat, history), image);
}

// the API of Gemini: the agent is the instruction of the system, the answers
// are those of the "model"
tree
gemini_command (string s, string model, string agent,
                string chat, bool history) {
  string key= ai_key ("gemini", "GEMINI_API_KEY");
  string name= ai_model_name ("gemini", "gemini-2.5-flash");
  array<string> conv= ai_conversation (s, model, chat, history);
  array<tree> contents;
  for (int i= 0; i+1 < N(conv); i += 2) {
    tree part= json_object ("text", conv[i+1]);
    contents << json_object ("role", conv[i] == "user"? "user": "model",
                             "parts", json_array (part));
  }
  bool image= ai_image_model (name);
  if (image && ai_default_agent (model, agent)) agent= ai_image_agent;
  array<tree> d;
  if (agent != "")
    d << tree ("systemInstruction")
      << json_object ("parts", json_array (json_object ("text", agent)));
  d << tree ("contents") << json_array (contents);
  if (image) {
    array<tree> m (tree ("TEXT"), tree ("IMAGE"));
    d << tree ("generationConfig")
      << json_object ("responseModalities", json_array (m));
  }
  return compound ("http_post",
    "https://generativelanguage.googleapis.com/v1beta/models/" * name *
      (ai_stream? string (":streamGenerateContent?alt=sse")
                : string (":generateContent")),
    tuple ("x-goog-api-key", key, "Content-Type", "application/json"),
    json_object (d));
}

// the API of Claude (Anthropic): the agent is the system prompt; a page asks
// with anthropic-dangerous-direct-browser-access, as its key is in the page
tree
claude_command (string s, string model, string agent,
                string chat, bool history) {
  string key= ai_key ("claude", "ANTHROPIC_API_KEY");
  string name= ai_model_name ("claude", "claude-sonnet-5-5");
  array<string> conv= ai_conversation (s, model, chat, history);
  array<tree> msgs;
  for (int i= 0; i+1 < N(conv); i += 2)
    msgs << json_object ("role", conv[i], "content", conv[i+1]);
  array<tree> d;
  d << tree ("model") << tree (name)
    << tree ("max_tokens")
    // (without streaming, Anthropic refuses the requests which may be long)
    << compound ("json-number", ai_stream? string ("32000"): string ("16000"));
  if (agent != "") d << tree ("system") << tree (agent);
  d << tree ("messages") << json_array (msgs);
  if (ai_stream) d << tree ("stream") << compound ("json-boolean", "true");
  tree h (TUPLE);
  h << tree ("x-api-key") << tree (key)
    << tree ("anthropic-version") << tree ("2023-06-01")
    << tree ("anthropic-dangerous-direct-browser-access") << tree ("true")
    << tree ("Content-Type") << tree ("application/json");
  return compound ("http_post", "https://api.anthropic.com/v1/messages", h,
                   json_object (d));
}

tree
ai_command (string s, string model, string agent, string chat, bool history) {
  ai_get_continuation (s, model, chat);
  string engine= ai_engine (model);
  if (engine == "chatgpt")
    return chatgpt_command (s, model, agent, chat, history);
  if (engine == "gemini")
    return gemini_command (s, model, agent, chat, history);
  if (engine == "ollama")
    return ollama_command (s, model, agent, chat, history);
  if (engine == "mistral")
    return mistral_command (s, model, agent, chat, history);
  if (engine == "albert")
    return albert_command (s, model, agent, chat, history);
  if (engine == "claude")
    return claude_command (s, model, agent, chat, history);
  if (engine == "openrouter")
    return openrouter_command (s, model, agent, chat, history);
  return "";
}

// the instructions of the engine (its system prompt): those of the user for
// it, else the default ones, which tell how to write LaTeX which TeXmacs
// takes well (ai-instructions in init-ai.scm); Albert adds its agent
static string
ai_latex_agent_description (string model) {
  string engine= ai_engine (model);
  string r= as_string (call ("ai-instructions", model));
  if (engine == "albert")
    r << "\n" << as_string (call ("ai-agents-get-interlocutor", object (engine)));
  return r;
}

string
ai_latex_command (string s, string model, string chat) {
  string agent= ai_latex_agent_description (model);
  tree t= ai_command (s, model, agent, chat, true);
  return to_shell_command (t);
}

string
ai_latex_request (string s, string model, string chat) {
  string agent= ai_latex_agent_description (model);
#ifdef __EMSCRIPTEN__
  ai_stream= true;
#endif
  tree t= ai_command (s, model, agent, chat, true);
  ai_stream= false;
  return tree_to_scheme (t);
}

/******************************************************************************
* Extracting the output for various engines
******************************************************************************/

// the text of an answer, from its JSON
static string
json_text (tree t) {
  return is_atomic (t)? t->label: string ("");
}

// the message of an error which the engine gave instead of an answer (or
// what it sent, when it is not JSON: no network, a refused request)
static string
ai_error_text (string val, tree t) {
  if (val == "") return ""; // not yet, or no answer (said by the link)
  tree e= json_get (t, "error");
  if (is_func (e, ATTR)) {
    string m= json_text (json_get (e, "message"));
    if (m != "") return "Error: " * m;
  }
  if (is_atomic (e) && e->label != "") return "Error: " * e->label;
  string m= json_text (json_get (t, "message"));
  if (m == "") m= json_text (json_get (t, "detail")); // Mistral
  if (m != "") return "Error: " * m;
  if (N(val) > 500) val= val (0, 500) * "...";
  return "Error: unexpected answer: " * val;
}

// a picture of an answer, in its text: as an image of Markdown with a data
// URL (which ai_set_aside makes an image of TeXmacs)
static string
ai_image_text (string mime, string data) {
  if (mime == "") mime= "image/png";
  return "\n\n![](data:" * mime * ";base64," * data * ")\n\n";
}

// the images of a message, or of a piece of it (OpenRouter: images, each
// with an image_url whose url is a data URL)
static string
openai_images (tree m) {
  tree im= json_get (m, "images");
  if (!is_func (im, TUPLE)) return "";
  string r;
  for (int i= 0; i < N(im); i++) {
    string u= json_text (json_get (json_get (im[i], "image_url"), "url"));
    if (starts (u, "data:image/")) r << "\n\n![](" << u << ")\n\n";
  }
  return r;
}

static string
openai_style_output (tree t) {
  tree c= json_get (t, "choices");
  tree pics= json_get (t, "data"); // an answer of images/generations
  if (is_func (pics, TUPLE) && N(pics) > 0 &&
      json_text (json_get (pics[0], "b64_json")) != "") {
    string r= json_text (json_get (pics[0], "revised_prompt"));
    string fmt= json_text (json_get (t, "output_format"));
    for (int i= 0; i < N(pics); i++)
      r << ai_image_text (fmt == "jpeg"? string ("image/jpeg"):
                          fmt == "webp"? string ("image/webp"):
                          string ("image/png"),
                          json_text (json_get (pics[i], "b64_json")));
    return r;
  }
  if (!is_func (c, TUPLE) || N(c) == 0) return "";
  tree m= json_get (c[0], "message");
  return json_text (json_get (m, "content")) * openai_images (m);
}

static string
gemini_style_output (tree t) {
  tree c= json_get (t, "candidates");
  if (!is_func (c, TUPLE) || N(c) == 0) return "";
  tree parts= json_get (json_get (c[0], "content"), "parts");
  if (!is_func (parts, TUPLE)) return "";
  string r;
  for (int i= 0; i < N(parts); i++) {
    tree in= json_get (parts[i], "inlineData");
    if (is_func (in, ATTR))
      r << ai_image_text (json_text (json_get (in, "mimeType")),
                          json_text (json_get (in, "data")));
    else r << json_text (json_get (parts[i], "text"));
  }
  return r;
}

static string
claude_style_output (tree t) {
  tree c= json_get (t, "content");
  if (!is_func (c, TUPLE)) return "";
  string r;
  for (int i= 0; i < N(c); i++)
    if (json_text (json_get (c[i], "type")) == "text")
      r << json_text (json_get (c[i], "text"));
  return r;
}

// replaces the narrow no-break spaces (U+202F, in UTF-8 e2 80 af) by spaces
static string
ai_plain_spaces (string r) {
  string s;
  for (int i= 0; i < N(r); i++) {
    if (i+2 < N(r) &&
	(unsigned char) r[i]   == 0xe2 &&
	(unsigned char) r[i+1] == 0x80 &&
	(unsigned char) r[i+2] == 0xaf) {
      s << ' '; i += 2; continue;
    }
    s << r[i];
  }
  return s;
}

// A streamed answer (server-sent events): lines "data: {...}", each with a
// piece of the text (OpenAI, Mistral, Ollama, Albert: choices[0].delta;
// Claude: the delta of a content_block_delta; Gemini: as an answer). The
// text of the complete lines so far; err, the message of an error event.
static bool
ai_is_stream (string s) {
  int i= 0;
  while (true) {
    while (i < N(s) && (s[i] == ' ' || s[i] == '\n' || s[i] == '\r')) i++;
    // the comments of the stream (OpenRouter: ": OPENROUTER PROCESSING",
    // while the model has not begun to answer)
    if (i < N(s) && s[i] == ':') {
      while (i < N(s) && s[i] != '\n') i++;
      continue;
    }
    break;
  }
  return test (s, i, "data:") || test (s, i, "event:");
}

string
ai_stream_text (string s, string model, string& err) {
  string engine= ai_engine (model);
  string r;
  err= "";
  int i= 0, n= N(s);
  while (i < n) {
    int e= i;
    while (e < n && s[e] != '\n') e++;
    if (e >= n) break; // an incomplete line: later
    string line= s (i, e);
    i= e + 1;
    if (N(line) > 0 && line[N(line)-1] == '\r') line= line (0, N(line) - 1);
    if (!starts (line, "data:")) continue;
    string d= line (5, N(line));
    while (N(d) > 0 && d[0] == ' ') d= d (1, N(d));
    if (d == "" || d == "[DONE]") continue;
    tree t= http_from_json (d);
    tree er= json_get (t, "error");
    if (is_func (er, ATTR)) err= json_text (json_get (er, "message"));
    else if (is_atomic (er) && er->label != "") err= er->label;
    if (engine == "claude") {
      tree delta= json_get (t, "delta");
      if (json_text (json_get (delta, "type")) == "text_delta")
        r << json_text (json_get (delta, "text"));
    }
    else if (engine == "gemini") r << gemini_style_output (t);
    else {
      tree c= json_get (t, "choices");
      if (is_func (c, TUPLE) && N(c) > 0) {
        tree delta= json_get (c[0], "delta");
        r << json_text (json_get (delta, "content")) << openai_images (delta);
      }
    }
  }
  return r;
}

static string ai_short_images (string s);

string
ai_output (string s, string model, string chat) {
  ai_set_continuation (s, model, chat);
  string engine= ai_engine (model);
  tree t= http_from_json (s);
  string r;
  if (ai_is_stream (s)) {
    string err;
    r= ai_stream_text (s * "\n", model, err);
    if (r == "" && err != "") return "Error: " * err;
  }
  else if (engine == "gemini") r= gemini_style_output (t);
  else if (engine == "claude") r= claude_style_output (t);
  else if (engine != "unknown") r= openai_style_output (t);
  if (r == "") return ai_error_text (s, t);
  if (N(ai_get_current_prompt (model, chat)) > 0) {
    ai_set_last_prompt (ai_get_current_prompt (model, chat), model, chat);
    ai_set_last_answer (ai_short_images (r), model, chat);
  }
  if (engine == "albert") {
    r= replace_tikz_by_pdf (r);
    r= extract_svg (r);
  }
  return ai_plain_spaces (r);
}

string
un_escape_cr (string s) {
  int i, n= N(s);
  string r;
  for (i=0; i<n; )
    if (test (s, i, "\\n")) {
      if (test (s, i, "\\nabla")) r << s[i++];
      else if (test (s, i, "\\ncong")) r << s[i++];
      else if (test (s, i, "\\nearrow")) r << s[i++];
      else if (test (s, i, "\\neq")) r << s[i++];
      else if (test (s, i, "\\new")) r << s[i++];
      else if (test (s, i, "\\ngeq")) r << s[i++];
      else if (test (s, i, "\\nleq")) r << s[i++];
      else if (test (s, i, "\\nmid")) r << s[i++];
      else if (test (s, i, "\\noindent")) r << s[i++];
      else if (test (s, i, "\\not")) r << s[i++];
      else if (test (s, i, "\\nsim")) r << s[i++];
      else if (test (s, i, "\\nsub")) r << s[i++];
      else if (test (s, i, "\\nsup")) r << s[i++];
      else { r << '\n'; i += 2; }
    }
    else r << s[i++];
  return r;
}

static tree
embed_images (tree t) {
  if (is_atomic (t)) return t;
  if (is_func (t, IMAGE, 5)) {
    array<tree> a= A(t);
    if (is_func (a[0], TUPLE)) return t;
    string im_name= cork_to_utf8 (as_string (a[0]));
    url image= url_system (im_name);
    if (!exists (image)) image= url (im_name);
    if (!exists (image)) image= url_temp_dir () * (im_name * ".svg");
    string type= "", data;
    tree s (IMAGE);
    load_string (image, data, false);
    if (data == "") {
      std_error << "ai.cpp, cannot embed image: " << im_name << LF;
      return t;
    }
    s << tuple (tree (RAW_DATA, data), as_string (tail (image)));
    s << a[1] << a[2] << a[3] << a[4];
    return s;
  }
  array<tree> a= A(t);
  for (int i= 0; i < N(a); i++)
    a[i]= embed_images (a[i]);
  return tree (L(t), a);
}

/******************************************************************************
* Pictures in the answers: TikZ (as executable folds of the TikZ plug-in)
* and SVG (as images), set aside while the rest is converted
******************************************************************************/

static string ai_block_mark= "TMAIBLOCK";

// the lines of the preamble which a TikZ picture needs: its libraries, and
// the packages which TikZJax has (as the TikZ plug-in asks for them)
static string
ai_tikz_header (string pre) {
  static const char* known[]= {
    "pgfplots", "tikz-cd", "circuitikz", "chemfig", "tkz-tab", "yquant",
    "braids", "kinematikz", "tikz-feynhand", "physics", "pgf-spectra",
    "amsmath", "amssymb", "mathtools", "bm", "cancel", "mhchem", NULL };
  string libs, pkgs;
  int i= 0;
  while ((i= search_forwards ("\\usetikzlibrary{", i, pre)) >= 0) {
    int e= search_forwards ("}", i, pre);
    if (e < 0) break;
    if (libs != "") libs << ", ";
    libs << pre (i + 16, e);
    i= e;
  }
  i= 0;
  while ((i= search_forwards ("\\usepackage", i, pre)) >= 0) {
    int b= search_forwards ("{", i, pre);
    int e= (b < 0)? -1: search_forwards ("}", b, pre);
    if (e < 0) break;
    array<string> l= tokenize (pre (b + 1, e), ",");
    for (int k= 0; k < N(l); k++) {
      string p= trim_spaces (l[k]);
      for (int m= 0; known[m] != NULL; m++)
        if (p == string (known[m])) {
          if (pkgs != "") pkgs << ", ";
          pkgs << p;
        }
    }
    i= e;
  }
  // the other settings of the pictures (pgfplots libraries, styles, colors)
  string other;
  static const char* settings[]= {
    "\\usepgfplotslibrary", "\\pgfplotsset", "\\tikzset", "\\definecolor",
    "\\colorlet", NULL };
  array<string> lines= tokenize (pre, "\n");
  for (int k= 0; k < N(lines); k++) {
    string l= trim_spaces (lines[k]);
    for (int m= 0; settings[m] != NULL; m++)
      if (starts (l, settings[m])) other << l << "\n";
  }
  string r;
  if (other == "") {
    if (pkgs != "") r << "% packages: " << pkgs << "\n";
    if (libs != "") r << "% libraries: " << libs << "\n";
    return r;
  }
  // with settings: a whole document (the TikZ plug-in takes its preamble)
  r << "\\documentclass{article}\n";
  if (pkgs != "") r << "\\usepackage{" << pkgs << "}\n";
  if (libs != "") r << "\\usetikzlibrary{" << libs << "}\n";
  r << other << "\\begin{document}\n";
  return r;
}

// a fold of a plug-in, whose code is text in UTF-8
static tree
ai_script_fold (string lan, string code) {
  array<string> lines= tokenize (code, "\n");
  tree doc (DOCUMENT);
  for (int i= 0; i < N(lines); i++)
    doc << tree (utf8_to_cork (lines[i]));
  return compound ("script-input", lan, "default", doc, "");
}

static tree
ai_svg_image (string svg) {
  static int counter= 0;
  counter++;
  tree data= tuple (tree (RAW_DATA, svg), "answer-" * as_string (counter) * ".svg");
  return tree (IMAGE, data, "", "", "", "");
}

// the next picture given as a data URL in s from i (data:image/png;base64,
// ...): its type, and where its URL begins and ends; -1 if none
static bool
ai_base64_char (char c) {
  return is_alpha (c) || is_digit (c) || c == '+' || c == '/' || c == '=';
}

static int
ai_find_data_image (string s, int i, string& mime, int& end) {
  while ((i= search_forwards ("data:image/", i, s)) >= 0) {
    int k= i + 11;
    while (k < N(s) && (is_alpha (s[k]) || s[k] == '+' || s[k] == '-')) k++;
    if (test (s, k, ";base64,")) {
      mime= s (i + 5, k);
      int e= k + 8;
      while (e < N(s) && ai_base64_char (s[e])) e++;
      if (e > k + 8) { end= e; return i; }
    }
    i++;
  }
  return -1;
}

static tree
ai_raster_image (string mime, string data) {
  static int counter= 0;
  counter++;
  string ext= mime (6, N(mime));
  if (ext == "jpeg") ext= "jpg";
  if (ext == "svg+xml") ext= "svg";
  string name= "answer-picture-" * as_string (counter) * "." * ext;
  tree img= tuple (tree (RAW_DATA, decode_base64 (data)), name);
  return tree (IMAGE, img, ext == "svg"? "": "0.6par", "", "", "");
}

// the pictures of s given as data URLs shortened (in the answer kept as it
// came, and in the conversation sent again)
static string
ai_short_images (string s) {
  string r, mime;
  int i= 0, end;
  while (true) {
    int p= ai_find_data_image (s, i, mime, end);
    if (p < 0) break;
    int b= search_forwards (",", p, s) + 1;
    r << s (i, b) << "... (" << as_string ((end - b) / 4 * 3) << " bytes)";
    i= end;
  }
  r << s (i, N(s));
  return r;
}

// a picture which does not end (the answer was cut, by the limit of its
// length): its code, and why
static tree
ai_cut_picture (string code) {
  tree doc (DOCUMENT);
  doc << compound ("with", "color", "dark red", "font-shape", "italic",
                   "The answer was cut before the end of this picture:");
  array<string> lines= tokenize (code, "\n");
  tree v (DOCUMENT);
  for (int k= 0; k < N(lines); k++) v << tree (utf8_to_cork (lines[k]));
  doc << compound ("verbatim-code", v);
  return doc;
}

// The fold of a TikZ picture: with its picture when it was made (the
// pictures are made as soon as they are complete, while the answer still
// comes, and kept by their code: ai-picture in ai-batch.scm), else asked
// for, and pending (ai-run-pending-folds fills it once it is there)
static tree
ai_picture_fold (string code) {
  tree fold= ai_script_fold ("tikz", code);
  object made= call ("ai-picture", code);
  if (is_tree (made)) {
    fold= tree (L(fold), fold[0], fold[1], fold[2], as_tree (made));
    return compound ("script-output", A(fold));
  }
  (void) call ("ai-picture-request", code);
  tree busy= compound ("script-output", fold[0], fold[1], fold[2],
                       compound ("script-busy"));
  return compound ("with", "ai-tikz", "pending", busy);
}

// the pictures of s replaced by marks; their trees in blocks
static string
ai_set_aside (string s, string pre, array<tree>& blocks) {
  string r;
  int i= 0, n= N(s);
  static const char* envs[]= { "tikzpicture", "tikzcd", "circuitikz", NULL };
  while (i < n) {
    int best= -1, kind= -1;
    for (int k= 0; envs[k] != NULL; k++) {
      int p= search_forwards ("\\begin{" * string (envs[k]) * "}", i, s);
      if (p >= 0 && (best < 0 || p < best)) { best= p; kind= k; }
    }
    int sp= search_forwards ("<svg", i, s);
    if (sp >= 0 && (best < 0 || sp < best)) { best= sp; kind= 100; }
    string dmime;
    int dend;
    int dp= ai_find_data_image (s, i, dmime, dend);
    if (dp >= 0 && (best < 0 || dp < best)) { best= dp; kind= 200; }
    if (best < 0) break;
    int end;
    bool cut= false; // the picture does not end: the answer was cut
    tree block;
    if (kind == 200) {
      // as an image of Markdown, \includegraphics, or <img src="...">
      block= ai_raster_image (dmime, s (search_forwards (",", best, s) + 1,
                                        dend));
      end= dend;
      int x= best;
      if (x >= 2 && s (x - 2, x) == "](" && test (s, end, ")")) {
        int y= search_backwards ("![", x, s);
        int nl= (y >= i)? search_forwards ("\n", y, s): -1;
        if (y >= i && (nl < 0 || nl > x)) { best= y; end++; }
      }
      else if (x >= 1 && s[x-1] == '{' && test (s, end, "}")) {
        int y= search_backwards ("\\includegraphics", x, s);
        if (y >= i && x - y < 200) { best= y; end++; }
      }
      else if (x >= 1 && (s[x-1] == '"' || s[x-1] == '\'')) {
        int y= search_backwards ("<img", x, s);
        int z= search_forwards (">", end, s);
        if (y >= i && x - y < 200 && z >= 0 && z - end < 200) {
          best= y; end= z + 1;
        }
      }
    }
    else if (kind == 100) {
      end= search_forwards ("</svg>", best, s);
      if (end < 0) { cut= true; end= n; }
      else end += 6;
      if (cut) block= ai_cut_picture (s (best, end));
      else
      block= ai_svg_image (s (best, end));
      // the XML declaration, and a fence ```svg ... ``` around it, go with it
      int b= best;
      int x= b;
      while (x > i && (s[x-1] == ' ' || s[x-1] == '\n')) x--;
      if (x >= 2 && s (x - 2, x) == "?>") {
        int y= search_backwards ("<?xml", x, s);
        if (y >= i) { b= y; x= y; }
        while (x > i && (s[x-1] == ' ' || s[x-1] == '\n')) x--;
      }
      best= b;
    }
    else {
      string env= envs[kind];
      string close= "\\end{" * env * "}";
      end= search_forwards (close, best, s);
      if (end < 0) {
        r << s (i, best) << "\n" << ai_block_mark << as_string (N(blocks)) << "Z\n";
        blocks << ai_cut_picture (s (best, n));
        i= n;
        break;
      }
      end += N(close);
      string head= ai_tikz_header (pre);
      // (an ellipsis, U+2026, where TikZ wants ..., as in \foreach)
      string code= head * replace (s (best, end), "\xe2\x80\xa6", "...");
      if (starts (head, "\\documentclass")) code << "\n\\end{document}";
      block= ai_picture_fold (code);
    }
    // a verbatim around a picture (where it is asked for an SVG) goes with
    // it, also when the picture was cut before its end
    {
      int x= best, z= end;
      while (x > i && (s[x-1] == ' ' || s[x-1] == '\n')) x--;
      while (z < n && (s[z] == ' ' || s[z] == '\n')) z++;
      string bv= "\\begin{verbatim}", ev= "\\end{verbatim}";
      if (x - N(bv) >= i && s (x - N(bv), x) == bv) {
        if (test (s, z, ev)) { best= x - N(bv); end= z + N(ev); }
        else if (cut) best= x - N(bv);
      }
    }
    // a picture alone in a formula (\[ ... \], $$ ... $$) is not a formula
    {
      int x= best, z= end;
      while (x > i && (s[x-1] == ' ' || s[x-1] == '\n')) x--;
      while (z < n && (s[z] == ' ' || s[z] == '\n')) z++;
      if (x - 2 >= i && (s (x - 2, x) == "\\[" || s (x - 2, x) == "$$")) {
        string close= (s (x - 2, x) == "$$")? string ("$$"): string ("\\]");
        if (test (s, z, close)) { best= x - 2; end= z + 2; }
      }
    }
    // a fence ```svg ... ``` (```latex, ```tex...) around it goes with it
    {
      int x= best;
      while (x > i && (s[x-1] == ' ' || s[x-1] == '\n')) x--;
      static const char* fences[]= {
        "```svg", "```xml", "```html", "```latex", "```tex", "```tikz",
        "```", NULL };
      for (int f= 0; fences[f] != NULL; f++) {
        string fe= fences[f];
        if (x - N(fe) >= i && s (x - N(fe), x) == fe) {
          int z= end;
          while (z < n && (s[z] == ' ' || s[z] == '\n')) z++;
          if (test (s, z, "```")) { best= x - N(fe); end= z + 3; }
          break;
        }
      }
    }
    r << s (i, best) << "\n" << ai_block_mark << as_string (N(blocks)) << "Z\n";
    blocks << block;
    i= end;
  }
  r << s (i, n);
  return r;
}

// the marks of t replaced by the trees they stand for
static tree
ai_put_back (tree t, array<tree> blocks) {
  if (N(blocks) == 0) return t;
  if (is_atomic (t)) {
    string s= t->label;
    int p= search_forwards (ai_block_mark, s);
    if (p < 0) return t;
    int q= p + N(ai_block_mark), e= q;
    while (e < N(s) && is_digit (s[e])) e++;
    if (e == q || e >= N(s) || s[e] != 'Z') return t;
    int k= as_int (s (q, e));
    if (k < 0 || k >= N(blocks)) return t;
    tree before= s (0, p), after= ai_put_back (s (e + 1, N(s)), blocks);
    if (before == "" && after == "") return blocks[k];
    tree c (CONCAT);
    if (before != "") c << before;
    c << blocks[k];
    if (after != "") {
      if (is_func (after, CONCAT)) c << A(after);
      else c << after;
    }
    return c;
  }
  int i, n= N(t);
  tree r (t, n);
  for (i= 0; i < n; i++) r[i]= ai_put_back (t[i], blocks);
  return r;
}

// the answer as it came, folded, for those who want to see it
static tree
ai_raw_fold (string raw) {
  raw= ai_short_images (raw);
  array<string> lines= tokenize (raw, "\n");
  tree doc (DOCUMENT);
  for (int i= 0; i < N(lines); i++)
    doc << tree (utf8_to_cork (lines[i]));
  tree fold= compound ("folded", compound ("with", "font-shape", "italic",
                                          "The answer as it came"),
                       compound ("verbatim-code", doc));
  return compound ("with", "ai-raw", "true", fold);
}

tree
ai_latex_output (string s, string model, string chat) {
  string r= ai_output (s, model, chat);
  if (DEBUG_IO) {
    string x= "] " * replace (r, "\n", "\n] ");
    debug_io << x << "\n";
  }
  string raw= r;
  tree t;
  array<tree> blocks;
  int start= search_forwards ("\\begin{document}", r);
  int end= (start < 0)? -1: search_forwards ("\\end{document}", start, r);
  // a document which was cut (the limit of the length of the answer)
  if (start >= 0 && end < 0) end= N(r);
  if (start < 0 || end < 0) {
    // an answer which is not a LaTeX document (or an error): text in UTF-8,
    // with its pictures
    string aside= ai_set_aside (r, "", blocks);
    if (N(blocks) == 0) t= utf8_to_cork (r);
    else {
      t= ai_put_back (verbatim_to_tree (aside, false, "utf-8"), blocks);
      // without the empty lines around it (those of a picture alone)
      if (is_func (t, DOCUMENT)) {
        int b= 0, e= N(t);
        while (b < e && t[b] == "") b++;
        while (e > b && t[e-1] == "") e--;
        if (b > 0 || e < N(t)) t= t (b, e);
      }
    }
  }
  else {
    string pre= r (0, start);
    r= r (start + 16, end);
    // (the answers are decoded from JSON: their \n are newlines already, and
    // un_escape_cr would make \nu a newline followed by u)
    string aside= ai_set_aside (r, pre, blocks);
    t= ai_put_back (ai_latex_body_to_tree (aside), blocks);
    t= embed_images (t);
    t= tree (WITH, MODE, "text", t);
  }
  if (get_preference ("ai raw answer", "on") == "on" &&
      !starts (raw, "Error:") && raw != "") {
    tree doc (DOCUMENT);
    if (is_func (t, DOCUMENT)) doc << A(t);
    else doc << t;
    doc << ai_raw_fold (raw);
    t= doc;
  }
  return t;
}

/******************************************************************************
* An answer which is not complete yet (a streamed one, see request_link.cpp)
******************************************************************************/

static bool
ai_verbatim_env (string env) {
  return env == "verbatim" || env == "lstlisting" || env == "minted" ||
         env == "verbatim*" || env == "filecontents" || env == "filecontents*";
}

// the longest beginning of the LaTeX s after which all is closed: the
// environments, the groups, the formulas ($, $$, \(, \[); an environment
// which is not closed yet, and what follows it, waits for its end
static string
ai_latex_closed_prefix (string s) {
  int n= N(s), i= 0, depth= 0, safe= 0;
  bool math= false, dmath= false;
  array<string> envs;
  while (i < n) {
    char c= s[i];
    if (c == '%') {
      while (i < n && s[i] != '\n') i++;
    }
    else if (c == '\\') {
      int j= i + 1;
      if (j >= n) break; // a command which is not written yet
      if (is_alpha (s[j])) {
        while (j < n && is_alpha (s[j])) j++;
        if (j >= n) break; // its name may go on
        string name= s (i + 1, j);
        if (name == "begin" || name == "end") {
          if (j >= n || s[j] != '{') { i= j; goto next; }
          int k= search_forwards ("}", j, s);
          if (k < 0) break;
          string env= s (j + 1, k);
          j= k + 1;
          if (name == "begin") {
            envs << env;
            if (ai_verbatim_env (env)) {
              int e= search_forwards ("\\end{" * env * "}", j, s);
              if (e < 0) break;
              j= e + N(env) + 6;
              envs->resize (N(envs) - 1);
            }
          }
          else if (N(envs) > 0 && envs[N(envs)-1] == env)
            envs->resize (N(envs) - 1);
        }
        i= j;
      }
      else {
        if (s[j] == '(' || s[j] == '[') math= true;
        else if (s[j] == ')' || s[j] == ']') math= false;
        i= j + 1;
      }
    }
    else if (c == '{') { depth++; i++; }
    else if (c == '}') { if (depth > 0) depth--; i++; }
    else if (c == '$') {
      if (i + 1 < n && s[i+1] == '$') { dmath= !dmath; i += 2; }
      else if (i + 1 >= n) break; // $ or $$
      else { math= !math; i++; }
    }
    else i++;
  next:
    // a point is safe when what follows is known and does not go on with a
    // command (its arguments: \section*{...}, \frac{a}{b}, \item[...])
    if (depth == 0 && !math && !dmath && N(envs) == 0 && i < n &&
        s[i] != '{' && s[i] != '[' && s[i] != '*')
      safe= i;
  }
  return s (0, safe);
}

// the constructs of LaTeX which the style of a session does not have, as
// their contents (a minipage of a figure)
static tree
ai_simplify (tree t) {
  if (is_atomic (t)) return t;
  if (is_compound (t, "minipage") && N(t) > 0) return ai_simplify (t[N(t)-1]);
  int i, n= N(t);
  tree r (t, n);
  for (i= 0; i < n; i++) r[i]= ai_simplify (t[i]);
  return r;
}

// the options of the lists (enumitem: \begin{itemize}[nosep]), which the
// import of LaTeX takes for the first item, losing the others
static string
ai_drop_list_options (string s) {
  static const char* lists[]= { "itemize", "enumerate", "description", NULL };
  for (int k= 0; lists[k] != NULL; k++) {
    string b= "\\begin{" * string (lists[k]) * "}";
    int i= 0;
    while ((i= search_forwards (b, i, s)) >= 0) {
      int j= i + N(b);
      while (j < N(s) && s[j] == ' ') j++;
      if (j < N(s) && s[j] == '[') {
        int depth= 0, e= j;
        for (; e < N(s); e++) {
          if (s[e] == '[' || s[e] == '{') depth++;
          else if (s[e] == ']' || s[e] == '}') { depth--; if (depth == 0) break; }
        }
        if (e < N(s)) s= s (0, i + N(b)) * s (e + 1, N(s));
      }
      i += N(b);
    }
  }
  return s;
}

tree
ai_latex_body_to_tree (string r) {
  r= ai_drop_list_options (r);
  r= replace (r, "\\maketitle", "");
  r= replace (r, "\\begin{lstlisting}", "\\begin{verbatim}");
  r= replace (r, "\\end{lstlisting}", "\\end{verbatim}");
  return ai_simplify (generic_to_tree (r, "latex-snippet"));
}

// what is shown of an answer which is still coming: a LaTeX document is set
// as far as all is closed in it (nothing while its preamble comes), any
// other text is shown as it is
tree
ai_latex_partial (string r) {
  int b= search_forwards ("\\begin{document}", r);
  if (b < 0) {
    if (occurs ("\\documentclass", r)) return "";
    return verbatim_to_tree (r, false, "utf-8");
  }
  string pre= r (0, b);
  r= r (b + 16, N(r));
  int e= search_forwards ("\\end{document}", r);
  if (e >= 0) r= r (0, e);
  r= ai_latex_closed_prefix (r);
  int k= 0;
  while (k < N(r) && (r[k] == ' ' || r[k] == '\n' || r[k] == '\r')) k++;
  if (k == N(r)) return "";
  // its complete pictures: SVG images at once, TikZ ones made now
  array<tree> blocks;
  string aside= ai_set_aside (r, pre, blocks);
  return tree (WITH, MODE, "text",
               ai_put_back (ai_latex_body_to_tree (aside), blocks));
}

/******************************************************************************
* Chat with ai
******************************************************************************/

string
ai_chat (string s, string model, string agent, string chat) {
  tree cmd= ai_command (s, model, agent, chat);
  string val= ai_eval_command (cmd);
  //if (DEBUG_IO) {
  //  debug_io << "input, " << cmd << LF;
  //  debug_io << "output, " << val << LF;
  //}
  string r= ai_output (val, model, chat);
  if (DEBUG_IO) {
    debug_io << "ai input, " << s << LF;
    debug_io << "ai output, " << r << LF;
  }
  return r;
}

array<string>
ai_get_body (string r) {
  int start= search_forwards ("<body>", r);
  if (start < 0)
    return array<string> (r, "", "");
  int end= search_forwards ("</body>", start, r);
  if (end < 0)
    return array<string> (r (start, N(r)) * "</body>", r (0, start), "");
  return array<string> (r (start, end+7), r (0, start), r (end+7, N(r)));
}

string
ai_chat (string s, string model, string agent, string chat,
	 string& pre, string& post) {
  string r= ai_chat (s, model, agent, chat);
  array<string> body= ai_get_body (r);
  pre = body[1];
  post= body[2];
  return body[0];
}

/******************************************************************************
* Automatic correction of spelling and grammar
******************************************************************************/

static string
ai_correct_agent_description (string lan, string model) {
  string engine= ai_engine (model);
  string q= string ("If necessary, then please correct the spelling ")
    * string ("and grammar of the following ") * lan
    * string (" text, and show me just the result, ")
    * string ("without further explanations or justifications:");
  if (engine == "albert") {
    q = string ("You are a native " * lan * " speaker. ");
    q << "You correct HTML documents. ";
    q << "Preserve HTML tags. Do not add new lines. ";
    q << as_string (call ("ai-agents-get-corrector", object (engine)));
    q << " Show explanations and justifications in comment tags at the end. ";
  }
  return q;
}

string
ai_correct (string s, string lan, string model, string chat,
	    array<string>& comments) {
  string agent= ai_correct_agent_description (lan, model);
  string pre, post;
  string ret= ai_chat (s, model, agent, chat, pre, post);
  comments= array<string> ();
  int pos= 0, start;
  while ((start= search_forwards ("<!--", pos, post)) >= 0) {
    int end= search_forwards ("-->", start, post);
    if (end <= start) break;
    string comment= trim_spaces (post (start+4, end));
    if (N(comment) > 0)
      comments << comment;
    pos= end;
  }
  return ret;
}

tree
ai_post (tree t, tree u) {
  while (is_document (t) && is_document (u) && N(t) > 0 && N(u) > 1 &&
         u[N(u)-1] == "" && t[N(t)-1] != "")
    u= u (0, N(u) - 1);
  return u;
}

tree
ai_correct (tree t, string lan, string model, string chat) {
  array<string> comments;
  string s= compress_html (t);
  //cout << "s= " << s << "\n";
  string r= ai_correct (s, lan, model, chat, comments);
  //cout << "r= " << r << "\n";
  tree u= decompress_html (r);
  //cout << "u = " << u << "\n";
  tree ret= tree (TUPLE);
  ret << ai_post (r, u);
  for (int i= 0; i < N(comments); i++)
    ret << decompress_html (comments[i]);
  return ret;
}

/******************************************************************************
* Automatic translation
******************************************************************************/

static string
ai_translate_agent_description (string from, string into, string model) {
  string engine= ai_engine (model);
  string q= "Translate HTML documents from ";
  q << from << " into " << into << ", without explanations.";
  if (engine == "albert") {
    q << " " << as_string (call ("ai-agents-get-translator", object (engine)));
  }
  return q;
}

string
ai_translate (string s, string from, string into, string model, string chat) {
  string agent= ai_translate_agent_description (from, into, model);
  string pre, post;
  return ai_chat (s, model, agent, chat, pre, post);
}

tree
ai_translate (tree t, string from, string into, string model, string chat) {
  string s= compress_html (t);
  //cout << "s= " << s << "\n";
  string r= ai_translate (s, from, into, model, chat);
  //cout << "r= " << r << "\n";
  tree u= decompress_html (r);
  //cout << "u = " << u << "\n";
  return ai_post (r, u);
}
