
/******************************************************************************
* MODULE     : sdl_gui.cpp
* DESCRIPTION: Graphical user interface for SDL
* COPYRIGHT  : (C) 2022 Massimiliano Gubinelli
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "sdl_gui.hpp"

#include "tm_timer.hpp"
#include "dictionary.hpp"
#include "image_files.hpp"
#include "message.hpp"
#include "iterator.hpp"
#include "font.hpp"
#include "analyze.hpp"
#include "converter.hpp"
#include "convert.hpp"
#include "locale.hpp"
#include "scheme.hpp"
#include "tm_link.hpp"       // number_of_servers
#include "sys_utils.hpp"     // get_env
#include "file.hpp"          // load_string (scripted events)
#include "socket_notifier.hpp" // notifiers_active (pause of the loop)
#ifdef OS_MACOS
#include "MacOS/mac_utilities.h" // mac_beep
#endif

#include "sdl_window.hpp"
#include "../MuPDF/mupdf_picture.hpp"
#include "../MuPDF/mupdf_renderer.hpp" // mupdf_image_gc

extern hashmap<SDL_Window*,pointer> Window_to_window;
extern int nr_windows;

sdl_gui_rep* the_gui= NULL;

bool char_clip= true;

void initialize_keyboard ();

/******************************************************************************
* General stuff
******************************************************************************/

sdl_gui_rep::sdl_gui_rep (int& argc2, char** argv2):
  mouse_state (0), buttons (0), balloon_win (NULL), interrupted (false),
  interrupt_time (0), update_requested (false),
  wheel_acc (0.0), wheel_precise (false), wheel_stamp (0), key_stamp (0)
{
  (void) argc2; (void) argv2;
  the_gui= this;

  // trackpads: macOS generates the momentum of a gesture itself (SDL drops
  // these events by default)
  SDL_SetHint (SDL_HINT_MAC_SCROLL_MOMENTUM, "1");
  if (!SDL_Init (SDL_INIT_VIDEO)) { // no audio backend is needed
    SDL_Log ("Unable to initialize SDL: %s", SDL_GetError ());
    exit (-1);
  }
  SDL_SetHint (SDL_HINT_MOUSE_FOCUS_CLICKTHROUGH, "1");

  screen_width = 1440;
  screen_height= 900;
  SDL_Rect r;
  if (SDL_GetDisplayBounds (SDL_GetPrimaryDisplay (), &r)) {
    screen_width = r.w;
    screen_height= r.h;
  }
  else SDL_Log ("SDL_GetDisplayBounds failed: %s", SDL_GetError ());

  // The renderers draw at retina_factor device pixels per point. It was
  // hardcoded to 2, which made everything twice too large on a display
  // without HiDPI. TeXmacs keeps one global factor, which follows the
  // primary display: the pixel density of its desktop mode (its content
  // scale is 1 on macOS while it draws at 2 pixels per point).
  float density= 0.0f;
  const SDL_DisplayMode* mode= SDL_GetDesktopDisplayMode (SDL_GetPrimaryDisplay ());
  if (mode != NULL) density= mode->pixel_density;
  string forced= get_env ("TEXMACS_SDL_DENSITY");
  if (N(forced) > 0 && is_double (forced)) density= (float) as_double (forced);
  int factor= (density >= 1.5f) ? 2 : 1; // the renderer wants an integer
  if (density <= 0.0f) factor= 2; // unknown: the previous default
  set_retina_factor (factor);
  if (DEBUG_EVENTS)
    debug_events << "display pixel density " << density
                 << ": drawing at " << factor << "x" << LF;

  initialize_colors ();
  initialize_keyboard ();
}

sdl_gui_rep::~sdl_gui_rep () {
  SDL_Quit();
}

void
sdl_gui_rep::get_extents (SI& width, SI& height) {
  width = screen_width  * PIXEL;
  height= screen_height * PIXEL;
}

void
sdl_gui_rep::get_max_size (SI& width, SI& height) {
  width = 8000 * PIXEL;
  height= 6000 * PIXEL;
}

// The state of the buttons and of the modifiers as TeXmacs expects it
// (as in the Qt port). On macOS, control and option emulate the right and
// middle buttons; the modifiers are passed as well.
void
sdl_gui_rep::update_mouse_state () {
  unsigned int state= 0;
  SDL_Keymod mods= SDL_GetModState ();
  if ((buttons & SDL_BUTTON_LMASK)  != 0) state += 1;
  if ((buttons & SDL_BUTTON_MMASK)  != 0) state += 2;
  if ((buttons & SDL_BUTTON_RMASK)  != 0) state += 4;
  if ((buttons & SDL_BUTTON_X1MASK) != 0) state += 8;
  if ((buttons & SDL_BUTTON_X2MASK) != 0) state += 16;
#ifdef OS_MACOS
  if ((mods & SDL_KMOD_CTRL)  != 0) state= 1024 + 4; // control key
  if ((mods & SDL_KMOD_ALT)   != 0) state= 2048 + 2; // option key
  if ((mods & SDL_KMOD_SHIFT) != 0) state += 256;
  if ((mods & SDL_KMOD_GUI)   != 0) state += 4096;   // command key
#else
  if ((mods & SDL_KMOD_SHIFT) != 0) state += 256;
  if ((mods & SDL_KMOD_CTRL)  != 0) state += 1024;
  if ((mods & SDL_KMOD_ALT)   != 0) state += 2048;
  if ((mods & SDL_KMOD_GUI)   != 0) state += 4096;
#endif
  mouse_state= state;
}

void
sdl_gui_rep::emulate_leave_enter (widget old_widget, widget new_widget) {
  float x, y;
  SDL_GetGlobalMouseState (&x, &y);
  sdl_window old_win= get_sdl_window (old_widget);
  sdl_window new_win= get_sdl_window (new_widget);
  int ox, oy;
  SI px, py;
  if (old_win != NULL) {
    SDL_GetWindowPosition (old_win->sdl_win, &ox, &oy);
    old_win->pointer_position (x - ox, y - oy, px, py);
    send_mouse (old_widget, "leave", px, py, mouse_state, 0);
  }
  if (new_win != NULL) {
    SDL_GetWindowPosition (new_win->sdl_win, &ox, &oy);
    new_win->pointer_position (x - ox, y - oy, px, py);
    send_mouse (new_widget, "enter", px, py, mouse_state, 0);
  }
}

/******************************************************************************
* Grabbing
******************************************************************************/

void
sdl_gui_rep::obtain_mouse_grab (widget wid) {
  widget old_widget;
  if (!is_nil (grab_ptr)) old_widget= grab_ptr->item;
  if (wid == old_widget) return;
  grab_ptr= list<widget> (wid, grab_ptr);
  widget new_widget= grab_ptr->item;
  notify_mouse_grab (new_widget, true);
  // the pointer is tracked outside of the windows while grabbed
  SDL_CaptureMouse (true);
  if (!is_nil (old_widget)) {
    notify_mouse_grab (old_widget, false);
    emulate_leave_enter (old_widget, new_widget);
  }
}

void
sdl_gui_rep::release_mouse_grab () {
  if (is_nil (grab_ptr)) return;
  widget old_widget= grab_ptr->item;
  grab_ptr= grab_ptr->next;
  if (is_nil (grab_ptr)) SDL_CaptureMouse (false);
  else {
    widget new_widget= grab_ptr->item;
    notify_mouse_grab (new_widget, true);
    SDL_CaptureMouse (true);
    notify_mouse_grab (old_widget, false);
    emulate_leave_enter (old_widget, new_widget);
  }
}

bool
sdl_gui_rep::has_mouse_grab (widget w) {
  return (!is_nil (grab_ptr)) && (grab_ptr->item == w);
}

/******************************************************************************
* Hack for getting the remote time
******************************************************************************/

static bool   time_initialized= false;
static time_t time_difference = 0;

static void
synchronize_time (Uint32 t) {
  if (time_initialized && time_difference == 0) return;
  time_t d= texmacs_time () - ((time_t) t);
  if (time_initialized) {
    if (d < time_difference)
      time_difference= d;
  }
  else {
    time_initialized= true;
    time_difference = d;
  }
  if (-1000 <= time_difference && time_difference <= 1000)
    time_difference= 0;
}

static time_t
remote_time (Uint32 t) {
  return ((time_t) t) + time_difference;
}

/******************************************************************************
* Set up keyboard
******************************************************************************/

hashmap<int,string> lower_key;
hashmap<int,string> upper_key;

static void
map (int key, string s) {
  lower_key (key)= s;
  upper_key (key)= "S-" * s;
}

static void
Map (int key, string s) {
  lower_key (key)= s;
  upper_key (key)= s;
}

static void
MMap (int key, string s1, string s2) {
  lower_key (key)= s1;
  upper_key (key)= s2;
}

void
initialize_keyboard () {
  static bool initialized= false;
  if (initialized) return;
  initialized= true;

  // Latin characters
  for (int c= 'a'; c <= 'z'; c++) {
    char lo[2]= { (char) c, 0 }, up[2]= { (char) (c - 'a' + 'A'), 0 };
    MMap (c, string (lo), string (up));
  }
  for (int c= '0'; c <= '9'; c++) {
    char s[2]= { (char) c, 0 };
    Map (c, string (s));
  }

  // Standard ASCII Symbols
  Map (SDLK_EXCLAIM, "!");
  Map (SDLK_DBLAPOSTROPHE, "\x22");
  Map (SDLK_HASH, "#");
  Map (SDLK_DOLLAR, "$");
  Map (SDLK_PERCENT, "%");
  Map (SDLK_AMPERSAND, "&");
  Map (SDLK_APOSTROPHE, "'");
  Map (SDLK_LEFTPAREN, "(");
  Map (SDLK_RIGHTPAREN, ")");
  Map (SDLK_ASTERISK, "*");
  Map (SDLK_PLUS, "+");
  Map (SDLK_COMMA, ",");
  Map (SDLK_MINUS, "-");
  Map (SDLK_PERIOD, ".");
  Map (SDLK_SLASH, "/");
  Map (SDLK_COLON, ":");
  Map (SDLK_SEMICOLON, ";");
  Map (SDLK_LESS, "<");
  Map (SDLK_EQUALS, "=");
  Map (SDLK_GREATER, ">");
  Map (SDLK_QUESTION, "?");
  Map (SDLK_AT, "@");
  Map (SDLK_LEFTBRACKET, "[");
  Map (SDLK_BACKSLASH, "\\");
  Map (SDLK_RIGHTBRACKET, "]");
  Map (SDLK_CARET, "^");
  Map (SDLK_UNDERSCORE, "_");
  Map (SDLK_GRAVE, "`");
  // "{", "|" and "}" are typed with a modifier and come as text events;
  // mapping them here overwrote the "[", "]" entries of the same keys

  // dead keys
  Map (0xFE50, "grave");
  Map (0xFE51, "acute");
  Map (0xFE52, "hat");
  Map (0xFE53, "tilde");
  Map (0xFE54, "macron");
  Map (0xFE55, "breve");
  Map (0xFE56, "abovedot");
  Map (0XFE57, "umlaut");
  Map (0xFE58, "abovering");
  Map (0xFE59, "doubleacute");
  Map (0xFE5A, "check");
  Map (0xFE5B, "cedilla");
  Map (0xFE5C, "ogonek");
  Map (0xFE5D, "iota");
  Map (0xFE5E, "voicedsound");
  Map (0xFE5F, "semivoicedsound");
  Map (0xFE60, "belowdot");

  // Special control keys
  Map (SDLK_UNDO, "undo");
  Map (SDLK_CANCEL, "cancel");

  // Control keys
  map (SDLK_SPACE, "space");
  map (SDLK_RETURN, "return");
  map (SDLK_BACKSPACE, "backspace");
  map (SDLK_DELETE, "delete");
  map (SDLK_INSERT, "insert");
  map (SDLK_TAB, "tab");
  map (SDLK_LEFT_TAB, "tab");
  map (SDLK_ESCAPE, "escape");
  map (SDLK_LEFT, "left");
  map (SDLK_RIGHT, "right");
  map (SDLK_UP, "up");
  map (SDLK_DOWN, "down");
  map (SDLK_PAGEUP, "pageup");
  map (SDLK_PAGEDOWN, "pagedown");
  map (SDLK_HOME, "home");
  map (SDLK_END, "end");
  map (SDLK_F1, "F1");
  map (SDLK_F2, "F2");
  map (SDLK_F3, "F3");
  map (SDLK_F4, "F4");
  map (SDLK_F5, "F5");
  map (SDLK_F6, "F6");
  map (SDLK_F7, "F7");
  map (SDLK_F8, "F8");
  map (SDLK_F9, "F9");
  map (SDLK_F10, "F10");
  map (SDLK_F11, "F11");
  map (SDLK_F12, "F12");
  map (SDLK_F13, "F13");
  map (SDLK_F14, "F14");
  map (SDLK_F15, "F15");
  map (SDLK_F16, "F16");
  map (SDLK_F17, "F17");
  map (SDLK_F18, "F18");
  map (SDLK_F19, "F19");
  map (SDLK_F20, "F20");

  // Keypad keys
  Map (SDLK_KP_SPACE, "K-space");
  Map (SDLK_KP_ENTER, "K-return");
  Map (SDLK_KP_TAB, "K-tab");
  Map (SDLK_KP_EQUALS, "K-=");
  Map (SDLK_KP_MULTIPLY, "K-*");
  Map (SDLK_KP_PLUS, "K-+");
  Map (SDLK_KP_MINUS, "K--");
  Map (SDLK_KP_PERIOD, "K-.");
  Map (SDLK_KP_COMMA, "K-,");
  Map (SDLK_KP_DIVIDE, "K-/");
  Map (SDLK_KP_0, "K-0");
  Map (SDLK_KP_1, "K-1");
  Map (SDLK_KP_2, "K-2");
  Map (SDLK_KP_3, "K-3");
  Map (SDLK_KP_4, "K-4");
  Map (SDLK_KP_5, "K-5");
  Map (SDLK_KP_6, "K-6");
  Map (SDLK_KP_7, "K-7");
  Map (SDLK_KP_8, "K-8");
  Map (SDLK_KP_9, "K-9");

  // Miscellaneous
  Map (0x20ac, "euro");
}

static string
print_modifiers (SDL_Keymod mod) {
  string s;
  s << "[";
  if (mod & SDL_KMOD_NUM)   s << " NUMLOCK";
  if (mod & SDL_KMOD_CAPS)  s << " CAPSLOCK";
  if (mod & SDL_KMOD_CTRL)  s << " CTRL";
  if (mod & SDL_KMOD_SHIFT) s << " SHIFT";
  if (mod & SDL_KMOD_ALT)   s << " ALT";
  if (mod & SDL_KMOD_GUI)   s << " GUI";
  s << " ]";
  return s;
}

static SDL_Keycode
postprocess_key_event (SDL_Scancode scancode, SDL_Keymod *current_mod) {
  // the key with each combination of shift and alt
  static SDL_Keymod combinations[4]= {
    SDL_KMOD_NONE,
    SDL_KMOD_SHIFT,
    SDL_KMOD_ALT,
    SDL_KMOD_SHIFT | SDL_KMOD_ALT
  };
  SDL_Keycode results[4];
  for (int i = 0; i < 4; i++)
    results[i]= SDL_GetKeyFromScancode (scancode, combinations[i], false);

  // a modifier alone is not a key
  SDL_Keycode k= results[0];
  if (k == SDLK_LSHIFT || k == SDLK_RSHIFT || k == SDLK_LCTRL || k == SDLK_RCTRL ||
      k == SDLK_LALT   || k == SDLK_RALT   || k == SDLK_LGUI  || k == SDLK_RGUI  ||
      k == SDLK_LMETA  || k == SDLK_RMETA  || k == SDLK_CAPSLOCK ||
      k == SDLK_NUMLOCKCLEAR || k == SDLK_SCROLLLOCK)
    return SDLK_UNKNOWN;

  // the modifiers which were used to compose the key are removed
  if ((*current_mod & SDL_KMOD_SHIFT) && (*current_mod & SDL_KMOD_ALT) &&
      results[3] != results[0]) {
    *current_mod &= ~(SDL_KMOD_SHIFT | SDL_KMOD_ALT);
    return results[3];
  }
  if ((*current_mod & SDL_KMOD_ALT) && results[2] != results[0]) {
    *current_mod &= ~SDL_KMOD_ALT;
    return results[2];
  }
  if ((*current_mod & SDL_KMOD_SHIFT) && results[1] != results[0]) {
    *current_mod &= ~SDL_KMOD_SHIFT;
    return results[1];
  }
  return results[0];
}

// the name of a key for TeXmacs; produces_text is set when the keystroke
// types text, which then comes as a text event (composed with the dead keys
// and the input method) and is delivered instead of the key
static string
lookup_key (SDL_Scancode scancode, SDL_Keymod mod, bool& produces_text) {
  SDL_Keycode key= postprocess_key_event (scancode, &mod);
  produces_text= false;
  if (key == SDLK_UNKNOWN) return ""; // it is only a modifier, we ignore it
  produces_text= (key >= 0x20 && key != 0x7f && (key & SDLK_SCANCODE_MASK) == 0 &&
                  (mod & (SDL_KMOD_CTRL | SDL_KMOD_ALT | SDL_KMOD_GUI)) == 0);

  const char* str= SDL_GetKeyName (key);
  string r (str, (int) strlen (str));
  r= utf8_to_cork (r);
  if (contains_unicode_char (r)) return r;
  string s= r;
  if ((key >= 'A') && (key <= 'Z')) s= upper_key[key - 'A' + 'a'];
  else if ((key >= 'a') && (key <= 'z')) s= lower_key[key];
  else if (lower_key->contains (key)) s= lower_key [key];
  if ((N(s)>=2) && (s[0]=='K') && (s[1]=='-')) s= s (2, N(s));

  if (mod & SDL_KMOD_SHIFT) s= "S-" * s;
  if (mod & SDL_KMOD_CTRL)  s= "C-" * s;
  if (mod & SDL_KMOD_ALT)   s= "A-" * s;
  if (mod & SDL_KMOD_GUI)   s= "M-" * s;
  if (DEBUG_EVENTS)
    debug_events << "key " << s << " " << print_modifiers (mod)
                 << (produces_text ? " (text follows)" : "") << LF;
  return s;
}

/******************************************************************************
* Scripted events (development aid)
*
* When TEXMACS_SDL_SCRIPT names a file, its lines are executed one by one
* (a line is executed only when no event is pending). Coordinates are in
* points, relative to the target window:
*
*   # comment
*   wait <ms>                       pause
*   window <substring of title>     select the target window (default: the
*   window #<id>                      last one created)
*   move x y                        pointer motion
*   press x y [left|right|middle]   button down
*   release x y [left|right|middle] button up
*   click x y [left|right|middle]   press followed by release
*   wheel x y dx dy                 wheel event at (x, y)
*   key [S-][C-][A-][M-]<name>      key press (SDL name: Return, Escape, Down...)
*   text <string>                   text input, one event per character
*   focus                           pretend the window got the keyboard focus
*   snapshot <name>                 save the backing store of the target
*                                   window as <TEXMACS_SDL_SNAPSHOT>/<name>.png
*   resize w h                      resize the target window (points)
*   close                           ask to close the target window
******************************************************************************/

static bool script_active= false;
static array<string> script_lines;
static int script_pos= 0;
static time_t script_next= 0;
static int script_win_id= 0;       // the target, 0: the last window created
static bool script_no_target= false;

static void
script_init () {
  string file= get_env ("TEXMACS_SDL_SCRIPT");
  if (N(file) == 0) return;
  string s;
  if (load_string (url_system (file), s, false)) {
    cout << "sdl script: cannot read " << file << LF;
    return;
  }
  script_lines= tokenize (s, "\n");
  script_active= true;
  cout << "sdl script: " << N(script_lines) << " lines" << LF;
}

static sdl_window
script_target () {
  if (script_no_target) return NULL;
  if (script_win_id != 0) {
    window w= get_window (script_win_id);
    if (w != NULL) return (sdl_window) w;
  }
  sdl_window last= NULL;
  iterator<SDL_Window*> it= iterate (Window_to_window);
  while (it->busy ()) {
    sdl_window w= (sdl_window) Window_to_window [it->next ()];
    if (!w->popup && (last == NULL || w->id > last->id)) last= w;
  }
  return last;
}

static Uint8
script_button (array<string> a, int i) {
  if (N(a) > i && a[i] == "right") return SDL_BUTTON_RIGHT;
  if (N(a) > i && a[i] == "middle") return SDL_BUTTON_MIDDLE;
  return SDL_BUTTON_LEFT;
}

static SDL_MouseButtonFlags script_buttons= 0;

static void
script_push_button (sdl_window win, float x, float y, Uint8 button, bool down) {
  SDL_Event ev;
  SDL_zero (ev);
  ev.type= down ? SDL_EVENT_MOUSE_BUTTON_DOWN : SDL_EVENT_MOUSE_BUTTON_UP;
  ev.button.timestamp= SDL_GetTicksNS ();
  ev.button.windowID= SDL_GetWindowID (win->sdl_win);
  ev.button.button= button;
  ev.button.down= down;
  ev.button.clicks= 1;
  ev.button.x= x;
  ev.button.y= y;
  if (down) script_buttons |= SDL_BUTTON_MASK (button);
  else script_buttons &= ~SDL_BUTTON_MASK (button);
  SDL_PushEvent (&ev);
}

static void
script_push_motion (sdl_window win, float x, float y) {
  SDL_Event ev;
  SDL_zero (ev);
  ev.type= SDL_EVENT_MOUSE_MOTION;
  ev.motion.timestamp= SDL_GetTicksNS ();
  ev.motion.windowID= SDL_GetWindowID (win->sdl_win);
  ev.motion.state= script_buttons;
  ev.motion.x= x;
  ev.motion.y= y;
  SDL_PushEvent (&ev);
}

static void
save_pixmap_as_png (fz_pixmap* pix, string path) {
  fz_context* ctx= mupdf_context ();
  c_string cpath (path);
  mupdf_protected ("save_pixmap_as_png", [&] () {
    fz_save_pixmap_as_png (ctx, pix, cpath);
  });
}

static void
script_window_event (sdl_window win, Uint32 type) {
  SDL_Event ev;
  SDL_zero (ev);
  ev.type= type;
  ev.window.timestamp= SDL_GetTicksNS ();
  ev.window.windowID= SDL_GetWindowID (win->sdl_win);
  SDL_PushEvent (&ev);
}

static void
script_step () {
  if (!script_active) return;
  if (SDL_PollEvent (NULL)) return; // let pending events be processed first
  time_t now= texmacs_time ();
  if (now < script_next) return;
  while (script_pos < N(script_lines)) {
    string line= trim_spaces (script_lines[script_pos++]);
    if (N(line) == 0 || line[0] == '#') continue;
    array<string> a= tokenize (line, " ");
    string cmd= a[0];
    cout << "sdl script: " << line << LF;
    if (cmd == "wait" && N(a) > 1) {
      script_next= now + as_int (a[1]);
      return;
    }
    if (cmd == "window" && N(a) > 1) {
      string title= line (N(cmd)+1, N(line));
      // no match: the following commands are skipped rather than sent to
      // another window (e.g. closing the main window by mistake)
      script_win_id= 0;
      iterator<SDL_Window*> it= iterate (Window_to_window);
      while (it->busy ()) {
        sdl_window w= (sdl_window) Window_to_window [it->next ()];
        if (title == "#" * as_string (w->id) || occurs (title, w->get_name ()))
          script_win_id= w->id;
      }
      script_no_target= (script_win_id == 0);
      if (script_no_target) {
        cout << "sdl script: no window matches " << title << "; windows:";
        it= iterate (Window_to_window);
        while (it->busy ()) {
          sdl_window w= (sdl_window) Window_to_window [it->next ()];
          cout << " #" << w->id << (w->popup ? " (popup) " : " ") << w->get_name ();
        }
        cout << LF;
      }
      continue;
    }
    sdl_window win= script_target ();
    if (win == NULL) continue;
    if (cmd == "move" && N(a) > 2)
      script_push_motion (win, as_double (a[1]), as_double (a[2]));
    else if (cmd == "press" && N(a) > 2)
      script_push_button (win, as_double (a[1]), as_double (a[2]), script_button (a, 3), true);
    else if (cmd == "release" && N(a) > 2)
      script_push_button (win, as_double (a[1]), as_double (a[2]), script_button (a, 3), false);
    else if (cmd == "click" && N(a) > 2) {
      script_push_motion (win, as_double (a[1]), as_double (a[2]));
      script_push_button (win, as_double (a[1]), as_double (a[2]), script_button (a, 3), true);
      script_push_button (win, as_double (a[1]), as_double (a[2]), script_button (a, 3), false);
    }
    else if (cmd == "wheel" && N(a) > 4) {
      SDL_Event ev;
      SDL_zero (ev);
      ev.type= SDL_EVENT_MOUSE_WHEEL;
      ev.wheel.timestamp= SDL_GetTicksNS ();
      ev.wheel.windowID= SDL_GetWindowID (win->sdl_win);
      ev.wheel.mouse_x= as_double (a[1]);
      ev.wheel.mouse_y= as_double (a[2]);
      ev.wheel.x= as_double (a[3]);
      ev.wheel.y= as_double (a[4]);
      SDL_PushEvent (&ev);
    }
    else if (cmd == "key" && N(a) > 1) {
      SDL_Event ev;
      SDL_zero (ev);
      string kn= a[1];
      SDL_Keymod mod= SDL_KMOD_NONE;
      while (N(kn) > 2 && kn[1] == '-') {
        if (kn[0] == 'S') mod |= SDL_KMOD_LSHIFT;
        else if (kn[0] == 'C') mod |= SDL_KMOD_LCTRL;
        else if (kn[0] == 'A') mod |= SDL_KMOD_LALT;
        else if (kn[0] == 'M') mod |= SDL_KMOD_LGUI;
        else break;
        kn= kn (2, N(kn));
      }
      c_string name (kn);
      ev.type= SDL_EVENT_KEY_DOWN;
      ev.key.timestamp= SDL_GetTicksNS ();
      ev.key.windowID= SDL_GetWindowID (win->sdl_win);
      ev.key.scancode= SDL_GetScancodeFromName (name);
      ev.key.key= SDL_GetKeyFromScancode (ev.key.scancode, SDL_KMOD_NONE, false);
      ev.key.mod= mod;
      ev.key.down= true;
      SDL_PushEvent (&ev);
    }
    else if (cmd == "text" && N(a) > 1) {
      // one text event per (utf8) character, as SDL does
      static char buffers[64][8]; // the events keep pointers to the text
      static int next= 0;
      string txt= line (N(cmd)+1, N(line));
      int i= 0;
      while (i < N(txt)) {
        int start= i;
        unsigned char c= (unsigned char) txt[i];
        int len= (c < 0x80) ? 1 : (c >= 0xF0) ? 4 : (c >= 0xE0) ? 3 : (c >= 0xC0) ? 2 : 1;
        i= min (N(txt), start + len);
        char* buf= buffers[next++ % 64];
        int n= min (i - start, 7);
        for (int j=0; j<n; j++) buf[j]= txt[start+j];
        buf[n]= 0;
        SDL_Event ev;
        SDL_zero (ev);
        ev.type= SDL_EVENT_TEXT_INPUT;
        ev.text.timestamp= SDL_GetTicksNS ();
        ev.text.windowID= SDL_GetWindowID (win->sdl_win);
        ev.text.text= buf;
        SDL_PushEvent (&ev);
      }
    }
    else if (cmd == "focus")
      script_window_event (win, SDL_EVENT_WINDOW_FOCUS_GAINED);
    else if (cmd == "snapshot" && N(a) > 1) {
      string dir= get_env ("TEXMACS_SDL_SNAPSHOT");
      if (N(dir) == 0) dir= ".";
      fz_pixmap* pix= ((mupdf_picture_rep*) win->backing_store->get_handle ())->pix;
      save_pixmap_as_png (pix, dir * "/" * a[1] * ".png");
    }
    else if (cmd == "resize" && N(a) > 2)
      win->set_size (as_int (a[1]) * PIXEL, as_int (a[2]) * PIXEL);
    else if (cmd == "close")
      script_window_event (win, SDL_EVENT_WINDOW_CLOSE_REQUESTED);
    else cout << "sdl script: unknown command " << line << LF;
    return; // one command per loop iteration
  }
  cout << "sdl script: done" << LF;
  script_active= false;
}

/******************************************************************************
* Event loop
******************************************************************************/

#define MIN_DELAY   10
#define MAX_DELAY   1000
#define REPAINT_DT  50   // ms: repaint even while events keep coming

static void (*the_interpose_handler) (void) = NULL;

static int  kbd_count= 0;
static bool request_partial_redraw= false;

// The resize watch (event_watch) repaints a window from inside SDL's event
// pump, which runs from every SDL call that pumps the events -- showing a
// window among them. It may do so only while the loop is waiting for
// events, which is also where a live resize (the window dragged) delivers
// them; anywhere else the event is left in the queue for the loop.
static bool watch_may_run= false;

static bool
loop_poll (SDL_Event* event) {
  watch_may_run= true;
  bool r= SDL_PollEvent (event);
  watch_may_run= false;
  return r;
}

static void
loop_wait (int ms) {
  watch_may_run= true;
  SDL_WaitEventTimeout (NULL, ms);
  watch_may_run= false;
}

// While the window is dragged by its border, the system does not return to
// our loop (macOS runs a loop of its own): the window is laid out and
// repainted here, so that it never shows stale content.
static bool SDLCALL
event_watch (void* data, SDL_Event* event) {
  (void) data;
  static bool busy= false;
  if (busy || !watch_may_run) return true;
  if (event->type != SDL_EVENT_WINDOW_RESIZED &&
      event->type != SDL_EVENT_WINDOW_EXPOSED) return true;
  sdl_window win= get_window_from_ID (event->window.windowID);
  if (win == NULL) return true;
  busy= true;
  if (event->type == SDL_EVENT_WINDOW_RESIZED)
    win->resize_event (event->window.data1, event->window.data2);
  else win->expose ();
  if (the_interpose_handler != NULL) the_interpose_handler ();
  if (nr_windows > 0 && get_window_from_ID (event->window.windowID) == win)
    win->repaint_invalid_regions ();
  busy= false;
  return true;
}

void
sdl_gui_rep::process_messages () {
  if (is_nil (messages)) return;
  list<message> not_ready;
  while (!is_nil (messages)) {
    time_t ct= texmacs_time ();
    message m= messages->item;
    if ((m->t - ct) <= 0) send_delayed_message (m->wid, m->s, m->t);
    else not_ready= list<message> (m, not_ready);
    messages= messages->next;
  }
  messages= not_ready;
}

// the pause of the loop, shortened for the next delayed message
int
sdl_gui_rep::next_message_delay (int delay) {
  time_t now= texmacs_time ();
  for (list<message> l= messages; !is_nil (l); l= l->next)
    delay= min (delay, max (0, (int) (l->item->t - now)));
  if (!is_nil (balloon_wid) && balloon_win == NULL)
    delay= min (delay, max (0, (int) (balloon_time + 666 - now)));
  return delay;
}

void
sdl_gui_rep::repaint_windows () {
  interrupted= false;
  interrupt_time= texmacs_time () + 50;
  // first the window which has the focus, and then the other windows
  iterator<SDL_Window*> it= iterate (Window_to_window);
  while (it->busy ()) {
    sdl_window win= (sdl_window) Window_to_window[it->next ()];
    if (win->has_focus) win->repaint_invalid_regions ();
  }
  it= iterate (Window_to_window);
  while (it->busy ()) {
    sdl_window win= (sdl_window) Window_to_window[it->next ()];
    if (!win->has_focus) win->repaint_invalid_regions ();
  }
}

void
sdl_gui_rep::event_loop () {
  int    delay= MIN_DELAY;
  time_t last_repaint= 0;

  SDL_AddEventWatch (&event_watch, NULL);
  script_init ();

  while (nr_windows > 0 || number_of_servers () != 0) {
    request_partial_redraw= false;
    script_step (); // may push synthetic events

    // 1. the events which are waiting. A burst is handled in one go (a
    // motion followed by another one is superseded by it), except that a
    // keystroke is shown before the next one is handled
    bool   busy= false;
    int    count= 0;
    SDL_Event event;
    while (count < 100 && loop_poll (&event)) {
      if (event.type == SDL_EVENT_MOUSE_MOTION) {
        SDL_Event next;
        if (SDL_PeepEvents (&next, 1, SDL_PEEKEVENT,
                            SDL_EVENT_FIRST, SDL_EVENT_LAST) == 1 &&
            next.type == SDL_EVENT_MOUSE_MOTION &&
            next.motion.windowID == event.motion.windowID &&
            next.motion.state == event.motion.state)
          continue;
      }
      process_event (&event);
      busy= true;
      count++;
      if (request_partial_redraw || nr_windows == 0) break;
    }
    if (nr_windows == 0) continue;

    // 2. nothing to do: sleep until an event arrives, or until the interpose
    // handler or a delayed message needs a turn. The pause grows while
    // nothing happens; sockets and pipes (plugins, the server) have no
    // event of their own and are polled by the interpose handler
    if (busy || update_requested) {
      delay= MIN_DELAY;
      update_requested= false;
    }
    else {
      int pause= notifiers_active () ? min (delay, 40) : delay;
      pause= next_message_delay (pause);
      if (pause > 0) loop_wait (pause);
      delay= min (delay + delay/5 + 1, MAX_DELAY);
    }

    // 3. the editors apply the changes
    if (the_interpose_handler != NULL) the_interpose_handler ();
    if (nr_windows == 0) continue;

    // 4. popup help balloons
    if (!is_nil (balloon_wid))
      if (texmacs_time () - balloon_time >= 666)
        if (balloon_win == NULL)
          map_balloon ();

    // 5. repaint, once the events have been handled. A trackpad delivers
    // its events faster than a frame is drawn and its stream never runs
    // dry: the repaint is therefore done anyway once it is old enough
    time_t now= texmacs_time ();
    if (!SDL_PollEvent (NULL) || request_partial_redraw ||
        now - last_repaint >= REPAINT_DT) {
      repaint_windows ();
      last_repaint= now;
    }

    // 6. delayed messages
    process_messages ();
  }
  SDL_RemoveEventWatch (&event_watch, NULL);
}

static string
mouse_decode (unsigned int mstate) {
  // left last: on macOS, control and option emulate the other buttons
  if (mstate & 2)       return "middle";
  else if (mstate & 4)  return "right";
  else if (mstate & 1)  return "left";
  else if (mstate & 8)  return "up";
  else if (mstate & 16) return "down";
  return "unknown";
}

// The editor scrolls by a fixed step for each "press-up" or "press-down"
// (edit_mouse.cpp): the wheel deltas accumulate into such steps. A notch of
// a mouse wheel is one step. A trackpad gives fractional deltas, a tenth of
// the displacement of the fingers in points, and the editor steps by about
// 100 points: ten units make a step, so that the page follows the fingers.
void
sdl_gui_rep::wheel_event (sdl_window win, SDL_MouseWheelEvent* ev) {
  Uint64 stamp= ev->timestamp;
  if (wheel_stamp == 0 || stamp - wheel_stamp > 200000000ull) {
    // a new stream of events
    wheel_acc= 0.0;
    wheel_precise= false;
  }
  wheel_stamp= stamp;
  double y= ev->y;
  if (y != floor (y)) wheel_precise= true;
  wheel_acc += wheel_precise ? y / 10.0 : y;
  update_mouse_state ();
  time_t t= texmacs_time ();
  while (wheel_acc >= 1.0) {
    win->mouse_event ("press-up", ev->mouse_x, ev->mouse_y, t);
    win->mouse_event ("release-up", ev->mouse_x, ev->mouse_y, t);
    wheel_acc -= 1.0;
  }
  while (wheel_acc <= -1.0) {
    win->mouse_event ("press-down", ev->mouse_x, ev->mouse_y, t);
    win->mouse_event ("release-down", ev->mouse_x, ev->mouse_y, t);
    wheel_acc += 1.0;
  }
}

void
sdl_gui_rep::process_event (SDL_Event *event) {
  sdl_window win;
  switch (event->type) {
    case SDL_EVENT_WINDOW_SHOWN:
    case SDL_EVENT_WINDOW_EXPOSED:
      win= get_window_from_ID (event->window.windowID);
      if (win) win->expose ();
      break;
    case SDL_EVENT_WINDOW_MOVED:
      win= get_window_from_ID (event->window.windowID);
      if (win) win->move_event (event->window.data1, event->window.data2);
      break;
    case SDL_EVENT_WINDOW_RESIZED:
      win= get_window_from_ID (event->window.windowID);
      if (win) win->resize_event (event->window.data1, event->window.data2);
      break;
    case SDL_EVENT_WINDOW_PIXEL_SIZE_CHANGED:
    case SDL_EVENT_WINDOW_DISPLAY_SCALE_CHANGED:
      // e.g. the window moved to a display of another density: the backing
      // store follows (sync_backing_store) and is repainted
      win= get_window_from_ID (event->window.windowID);
      if (win && win->sync_backing_store ()) win->invalidate_all ();
      break;
    case SDL_EVENT_WINDOW_MOUSE_ENTER:
    case SDL_EVENT_WINDOW_MOUSE_LEAVE:
      win= get_window_from_ID (event->window.windowID);
      if (win) {
        float x, y;
        int ox, oy;
        SDL_GetGlobalMouseState (&x, &y);
        SDL_GetWindowPosition (win->sdl_win, &ox, &oy);
        update_mouse_state ();
        bool enter= (event->type == SDL_EVENT_WINDOW_MOUSE_ENTER);
        win->mouse_event (enter ? "enter" : "leave", x - ox, y - oy,
                          texmacs_time ());
      }
      break;
    case SDL_EVENT_WINDOW_FOCUS_GAINED:
      win= get_window_from_ID (event->window.windowID);
      if (win) win->focus_in_event ();
      break;
    case SDL_EVENT_WINDOW_FOCUS_LOST:
      win= get_window_from_ID (event->window.windowID);
      if (win) win->focus_out_event ();
      break;
    case SDL_EVENT_WINDOW_CLOSE_REQUESTED:
      win= get_window_from_ID (event->window.windowID);
      if (win) win->destroy_event();
      break;
    case SDL_EVENT_MOUSE_BUTTON_DOWN:
    case SDL_EVENT_MOUSE_BUTTON_UP:
    {
      unmap_balloon ();
      // the state is that of the event: with the button for a press,
      // without it for a release (the global state of SDL may be ahead of
      // or behind the queue)
      SDL_MouseButtonFlags mask= SDL_BUTTON_MASK (event->button.button);
      bool down= (event->type == SDL_EVENT_MOUSE_BUTTON_DOWN);
      buttons= down ? (buttons | mask) : (buttons & ~mask);
      SDL_MouseButtonFlags now= buttons;
      buttons |= mask;
      update_mouse_state ();
      string action= (down ? "press-" : "release-") * mouse_decode (mouse_state);
      buttons= now;
      update_mouse_state ();
      if (DEBUG_EVENTS) debug_events << action << " state " << mouse_state << LF;
      win= get_window_from_ID (event->button.windowID);
      if (win) win->mouse_event (action, event->button.x, event->button.y,
                                 texmacs_time ());
      break;
    }
    case SDL_EVENT_MOUSE_WHEEL:
    {
      unmap_balloon ();
      win= get_window_from_ID (event->wheel.windowID);
      if (win) wheel_event (win, &event->wheel);
      break;
    }
    case SDL_EVENT_MOUSE_MOTION:
    {
      unmap_balloon ();
      buttons= event->motion.state;
      update_mouse_state ();
      win= get_window_from_ID (event->motion.windowID);
      if (win) win->mouse_event ("move", event->motion.x, event->motion.y,
                                 texmacs_time ());
      break;
    }
    case SDL_EVENT_KEY_DOWN:
    {
      unmap_balloon ();
      win= get_window_from_ID (event->key.windowID);
      if (win == NULL) break;
      bool produces_text= false;
      string key= lookup_key (event->key.scancode, event->key.mod, produces_text);
      if (N(key) == 0) break;
      // SDL3 timestamps are nanoseconds (they were compared with
      // milliseconds, which set request_partial_redraw at random)
      Uint32 stamp= (Uint32) (event->key.timestamp / 1000000ull);
      kbd_count++;
      synchronize_time (stamp);
      if (texmacs_time () - remote_time (stamp) < 100 || (kbd_count & 15) == 0)
        request_partial_redraw= true;
      if (produces_text) break; // the text event of this keystroke follows
      // with a modifier, the system may still send a text event for the
      // keystroke (see SDL_EVENT_TEXT_INPUT); without one there is none, and
      // the text of the next keystroke must not be taken for it
      bool with_mods= (event->key.mod & (SDL_KMOD_CTRL | SDL_KMOD_ALT | SDL_KMOD_GUI)) != 0;
      key_stamp= with_mods ? event->key.timestamp : 0;
      win->key_event (key);
      break;
    }
    case SDL_EVENT_TEXT_INPUT:
    {
      // the text typed by a keystroke (see SDL_EVENT_KEY_DOWN): the key
      // names of TeXmacs for the characters which have one
      win= get_window_from_ID (event->text.windowID);
      if (win == NULL || event->text.text == NULL) break;
      // a text event right after a key delivered as a key (a command
      // modifier, an unconsumed alt) belongs to that keystroke
      if (key_stamp != 0 && event->text.timestamp - key_stamp < 30000000ull) {
        key_stamp= 0;
        break;
      }
      string r= utf8_to_cork (event->text.text);
      if (r == " ") r= "space";
      else if (r == "<") r= "<less>";
      else if (r == ">") r= "<gtr>";
      if (DEBUG_EVENTS) debug_events << "text " << r << LF;
      win->key_event (r);
      break;
    }
    case SDL_EVENT_TEXT_EDITING:
    {
      // the composition of an input method (dead keys, CJK...): shown by
      // the editor as a pre-edit ("pre-edit:<cursor>:<text>", an empty text
      // ends it), as in the Qt port; the committed text comes as text input
      win= get_window_from_ID (event->edit.windowID);
      if (win == NULL) break;
      string t= (event->edit.text != NULL) ?
        utf8_to_cork (string (event->edit.text)) : string ("");
      string k= "pre-edit:";
      if (N(t) > 0) k << as_string (max (0, (int) event->edit.start)) << ":" << t;
      win->key_event (k);
      break;
    }
  }
}

/******************************************************************************
* Windows
******************************************************************************/

void
sdl_gui_rep::created_window (SDL_Window* win) {
  windows_l << win;
}

void
sdl_gui_rep::deleted_window (SDL_Window* win) {
  windows_l= remove (windows_l, win);
}

void
sdl_gui_rep::focussed_window (SDL_Window* win) {
  windows_l= list<SDL_Window*> (win, remove (windows_l, win));
}

/******************************************************************************
* Selections and the clipboard
*
* SDL has the system clipboard only: it holds the "primary" selection; the
* other ones (the internal buffers of TeXmacs) are kept here.
******************************************************************************/

static hashmap<string,tree>   selection_t ("none");
static hashmap<string,string> selection_s ("");

// what is offered to the other applications; owned by SDL until it asks
// for its cleanup
struct sdl_clipboard_data {
  c_string texmacs_data;
  c_string plain_text;
  c_string html_text;
  int      n_texmacs, n_plain, n_html;
  sdl_clipboard_data (string t, string p, string h):
    texmacs_data (t), plain_text (p), html_text (h),
    n_texmacs (N(t)), n_plain (N(p)), n_html (N(h)) {}
};

static const void* SDLCALL
clipboard_data_callback (void *userdata, const char *mime_type, size_t *size) {
  sdl_clipboard_data* data= static_cast<sdl_clipboard_data*> (userdata);
  *size= 0;
  if (data == NULL || mime_type == NULL) return NULL;
  string mime (mime_type);
  if (mime == "text/html" && data->n_html > 0) {
    *size= data->n_html;
    return (const void*) (char*) data->html_text;
  }
  if (starts (mime, "text/plain") && data->n_plain > 0) {
    *size= data->n_plain;
    return (const void*) (char*) data->plain_text;
  }
  *size= data->n_texmacs;
  return (const void*) (char*) data->texmacs_data;
}

static void SDLCALL
clipboard_cleanup_callback (void *userdata) {
  delete static_cast<sdl_clipboard_data*> (userdata);
}

bool
set_selection (string key, tree t, string s, string sv, string sh, string format) {
  selection_t (key)= copy (t);
  selection_s (key)= copy (s);
  if (key != "primary") return true;

  string plain= s;
  if ((format == "verbatim" || format == "default") && N(sv) > 0) plain= sv;
  string html= (format == "html") ? s : sh;
  sdl_clipboard_data* data= new sdl_clipboard_data (s, plain, html);
  const char* mime_types[4];
  size_t n= 0;
  mime_types[n++]= "application/x-texmacs-clipboard";
  if (N(html) > 0) mime_types[n++]= "text/html";
  mime_types[n++]= "text/plain;charset=utf-8";
  mime_types[n++]= "text/plain";
  if (!SDL_SetClipboardData (clipboard_data_callback, clipboard_cleanup_callback,
                             data, mime_types, n)) {
    SDL_Log ("Failed to set clipboard data: %s", SDL_GetError ());
    delete data;
    return false;
  }
  return true;
}

static bool
clipboard_fetch (const char* mime, string& s) {
  if (!SDL_HasClipboardData (mime)) return false;
  size_t size= 0;
  void* p= SDL_GetClipboardData (mime, &size);
  if (p == NULL) return false;
  s= string ((char*) p, (int) size);
  SDL_free (p);
  return true;
}

bool
get_selection (string key, tree& t, string& s, string format) {
  bool direct= (key == "extern");
  if (direct) key= "primary";
  s= "";
  t= "none";
  if (key != "primary") {
    if (!selection_t->contains (key)) return false;
    t= copy (selection_t [key]);
    s= copy (selection_s [key]);
    return true;
  }

  string input_format;
  if (format == "default") {
    if (clipboard_fetch ("application/x-texmacs-clipboard", s))
      input_format= "texmacs-snippet";
    else if (clipboard_fetch ("text/html", s))
      input_format= "html-snippet";
    else if (clipboard_fetch ("text/plain;charset=utf-8", s) ||
             clipboard_fetch ("text/plain", s))
      input_format= "verbatim-snippet";
  }
  if (N(s) == 0 && input_format == "") {
    char* text= SDL_GetClipboardText ();
    if (text != NULL) {
      s= string (text);
      SDL_free (text);
      if (format == "default") input_format= "verbatim-snippet";
    }
  }
  if (N(s) == 0) return false;

  if (input_format == "html-snippet" && seems_buggy_html_paste (s))
    s= correct_buggy_html_paste (s);
  if (seems_buggy_paste (s))
    s= correct_buggy_paste (s);
  if (input_format != "" && !direct)
    s= as_string (call ("convert", s, input_format, "texmacs-snippet"));
  if (input_format == "html-snippet") {
    tree tt= as_tree (call ("convert", s, "texmacs-snippet", "texmacs-tree"));
    tt= default_with_simplify (tt);
    s= as_string (call ("convert", tt, "texmacs-tree", "texmacs-snippet"));
  }
  t= tuple ("extern", s);
  return true;
}

void
clear_selection (string key) {
  selection_t->reset (key);
  selection_s->reset (key);
  if (key == "primary") SDL_ClearClipboardData ();
}

/******************************************************************************
* Mouse pointers
******************************************************************************/

static SDL_Cursor*
system_cursor (SDL_SystemCursor id) {
  static hashmap<int,pointer> cursors (NULL);
  if (!cursors->contains ((int) id))
    cursors ((int) id)= (pointer) SDL_CreateSystemCursor (id);
  return (SDL_Cursor*) cursors [(int) id];
}

// the X11 cursor names (XC_...) which TeXmacs asks for
void
sdl_gui_rep::set_mouse_pointer (widget w, string name) {
  (void) w;
  if (starts (name, "XC_")) name= name (3, N(name));
  SDL_SystemCursor id= SDL_SYSTEM_CURSOR_DEFAULT;
  if (name == "xterm") id= SDL_SYSTEM_CURSOR_TEXT;
  else if (name == "watch" || name == "clock") id= SDL_SYSTEM_CURSOR_WAIT;
  else if (name == "crosshair" || name == "cross" || name == "tcross")
    id= SDL_SYSTEM_CURSOR_CROSSHAIR;
  else if (name == "hand1" || name == "hand2") id= SDL_SYSTEM_CURSOR_POINTER;
  else if (name == "fleur") id= SDL_SYSTEM_CURSOR_MOVE;
  else if (name == "sb_h_double_arrow") id= SDL_SYSTEM_CURSOR_EW_RESIZE;
  else if (name == "sb_v_double_arrow") id= SDL_SYSTEM_CURSOR_NS_RESIZE;
  else if (name == "X_cursor" || name == "pirate") id= SDL_SYSTEM_CURSOR_NOT_ALLOWED;
  SDL_Cursor* c= system_cursor (id);
  if (c != NULL) SDL_SetCursor (c);
  SDL_ShowCursor ();
}

// a cursor drawn by TeXmacs: only the invisible one (graphics mode draws
// its own pointer) is supported
void
sdl_gui_rep::set_mouse_pointer (widget w, string name, string mask_name) {
  (void) mask_name;
  if (occurs ("none", name)) SDL_HideCursor ();
  else set_mouse_pointer (w, "XC_top_left_arrow");
}

/******************************************************************************
* Miscellaneous
******************************************************************************/

void
sdl_gui_rep::show_help_balloon (widget wid, SI x, SI y) {
  unmap_balloon ();
  balloon_wid = wid;
  balloon_win = NULL;
  balloon_x   = x;
  balloon_y   = y;
  balloon_time= texmacs_time ();
}

void
sdl_gui_rep::map_balloon () {
  widget win_wid= popup_window_widget (balloon_wid, "Balloon");
  set_position (win_wid, balloon_x, balloon_y);
  balloon_win= (window) get_sdl_window (win_wid);
  balloon_win->set_visibility (true);
}

void
sdl_gui_rep::unmap_balloon () {
  if (!is_nil (balloon_wid)) {
    if (balloon_win != NULL) {
      balloon_win->set_visibility (false);
      tm_delete (balloon_win);
      balloon_win= NULL;
    }
    balloon_wid= widget ();
  }
}

void
sdl_gui_rep::show_wait_indicator (widget w, string message, string arg) {
  // NOTE: the wait indicator is directly displayed inside the window
  // corresponding to w. We explicitly shortcut the main event loop
  // by invalidating the wait widget and requesting a redraw.
  // Using a popup window does not work, because it would be necessary
  // to return to the main loop to map and redraw it.
  sdl_window ww= get_sdl_window (w);
  if (ww == NULL || message == "") return;
  if (arg != "") message= message * " " * arg * "...";
  SI width= 400*PIXEL, height= 160*PIXEL;
  widget wait_wid= wait_widget (width, height, message);
  SI win_w, win_h;
  ww->get_size (win_w, win_h);
  SI mid_x= win_w/2, mid_y= -win_h/2 + height;
  SI x= mid_x- width/2, y= mid_y- height/2;
  widget old_wid= ww->w;
  ww->w= wait_wid;
  set_position (wait_wid, x, y);
  set_identifier (wait_wid, ww->id);
  send_invalidate_all (wait_wid);
  ww->repaint_invalid_regions ();
  ww->w= old_wid;
  send_invalidate_all (old_wid);
}

void
sdl_gui_rep::external_event (string type, time_t t) {
  (void) t;
  if (!is_nil (windows_l)) {
    SDL_Window* win= windows_l->item;
    sdl_window sdl_win= (sdl_window) Window_to_window[win];
    sdl_win->key_event (type);
  }
}

bool
sdl_gui_rep::check_event (int type) {
  bool status;
  switch (type) {
  case INTERRUPT_EVENT:
    if (interrupted) return true;
    else {
      time_t now= texmacs_time ();
      if (now - interrupt_time < 0) return false;
      else interrupt_time= now + 50;
      interrupted= SDL_HasEvent (SDL_EVENT_KEY_DOWN) ||
                   SDL_HasEvent (SDL_EVENT_TEXT_INPUT) ||
                   SDL_HasEvent (SDL_EVENT_MOUSE_BUTTON_DOWN);
      return interrupted;
    }
  case INTERRUPTED_EVENT:
    return interrupted;
  case ANY_EVENT:
    // SDL leaves a poll sentinel in the queue after each pump: it is not an
    // event of ours, and counting it made the editor never idle (no delayed
    // :idle commands, no pre-edit of the input methods)
    return SDL_HasEvents (SDL_EVENT_FIRST, SDL_EVENT_POLL_SENTINEL - 1) ||
           SDL_HasEvents (SDL_EVENT_POLL_SENTINEL + 1, SDL_EVENT_USER - 1);
  case MOTION_EVENT:
    return SDL_HasEvent (SDL_EVENT_MOUSE_MOTION);
  case DRAG_EVENT:
    {
      status= false;
      SDL_Event event;
      if (SDL_PeepEvents (&event, 1, SDL_PEEKEVENT,
                          SDL_EVENT_MOUSE_MOTION, SDL_EVENT_MOUSE_MOTION) == 1)
        status= (event.motion.state != 0);
    }
    return status;
  case MENU_EVENT:
    status= SDL_HasEvent (SDL_EVENT_MOUSE_BUTTON_UP);
    if (!status) {
      SDL_Event event;
      if (SDL_PeepEvents (&event, 1, SDL_PEEKEVENT,
                          SDL_EVENT_MOUSE_MOTION, SDL_EVENT_MOUSE_MOTION) == 1)
        status= (event.motion.state != 0);
    }
    return status;
  }
  return interrupted;
}

/******************************************************************************
* Fonts
******************************************************************************/

static string the_default_font ("");
font the_default_wait_font;

void
sdl_gui_rep::set_default_font (string name) {
  the_default_font= name;
}

font
sdl_gui_rep::default_font_sub (bool tt, bool mini, bool bold) {
  string s= the_default_font;
  string series= (bold? string ("bold"): string ("medium"));
  if (s == "") s= "ecrm11@300";
  int i, j, n= N(s);
  for (j=0; j<n; j++) if (is_digit (s[j])) break;
  string fam= s (0, j);
  if (mini && fam == "ecrm") fam= "ecss";
  if (bold && fam == "ecrm") fam= "ecbx";
  if (bold && fam == "ecss") fam= "ecsx";
  for (i=j; j<n; j++) if (s[j] == '@') break;
  int sz= (j<n? as_int (s (i, j)): 10);
  if (j<n) j++;
  int dpi= (j<n? as_int (s (j, n)): 300);
  if (mini) { sz= (int) (0.6 * sz); dpi= (int) (1.3333333 * dpi); }
  if (use_macos_fonts ()) {
    // the family is named directly: the "apple-lucida" rule maps to the
    // regular face whatever the series (see get_default_font in the Vue port)
    return find_font ("Lucida Grande", "ss", series, "right",
                      sz, (int) (0.95 * dpi));
  }
  if (N(fam) >= 2) {
    string ff= fam (0, 2);
    string out_lan= get_output_language ();
    if (((out_lan == "bulgarian") || (out_lan == "russian") ||
         (out_lan == "ukrainian")) &&
        ((ff == "cm") || (ff == "ec"))) {
      fam= "la" * fam (2, N(fam)); ff= "la"; if (sz<100) sz *= 100; }
    if (out_lan == "japanese" || out_lan == "korean") {
      tree modern_fn= tuple ("modern", "ss", series, "right");
      modern_fn << as_string (sz) << as_string (dpi);
      return find_font (modern_fn);
    }
    if (out_lan == "chinese" || out_lan == "taiwanese")
      return unicode_font ("fireflysung", sz, dpi);
    if (out_lan == "greek")
      return unicode_font ("Stix", sz, dpi);
    if (ff == "ec")
      return tex_ec_font (tt? ff * "tt": fam, sz, dpi);
    if (ff == "la")
      return tex_la_font (tt? ff * "tt": fam, sz, dpi, 1000);
    if (ff == "pu") tt= false;
    if ((ff == "cm") || (ff == "pn") || (ff == "pu"))
      return tex_cm_font (tt? ff * "tt": fam, sz, dpi);
  }
  return tex_font (fam, sz, dpi);
}

font
sdl_gui_rep::default_font (bool tt, bool mini, bool bold) {
  font fn= default_font_sub (tt, mini, bold);
  if (!tt && !mini) the_default_wait_font= fn;
  return fn;
}

/******************************************************************************
* Interface
******************************************************************************/

void
gui_open (int& argc2, char** argv2) {
  ASSERT (the_gui == NULL, "gui already open");
  the_gui= tm_new<sdl_gui_rep> (argc2, argv2);
}

void
gui_start_loop () {
  the_gui->event_loop ();
}

void
gui_close () {
  ASSERT (the_gui != NULL, "gui not yet open");
  tm_delete (the_gui);
  the_gui= NULL;
}

void
gui_root_extents (SI& width, SI& height) {
  the_gui->get_extents (width, height);
}

void
gui_maximal_extents (SI& width, SI& height) {
  the_gui->get_max_size (width, height);
}

void
gui_refresh () {
  iterator<SDL_Window*> it= iterate (Window_to_window);
  while (it->busy()) {
    sdl_window win= (sdl_window) Window_to_window [it->next()];
    if (get_sdl_window (win->w) != NULL)
      send_update (win->w);
  }
}

string
gui_version () {
  return "sdl";
}

void
beep () {
#ifdef OS_MACOS
  mac_beep ();
#else
  cerr << "\a" << flush;
#endif
}

void
image_gc (string name) {
  // the renderer caches the decoded images, the patterns and their images
  mupdf_image_gc (name);
}

void
show_help_balloon (widget wid, SI x, SI y) {
  the_gui->show_help_balloon (wid, x, y);
}

void
show_wait_indicator (widget w, string message, string arg) {
  the_gui->show_wait_indicator (w, message, arg);
}

void
external_event (string type, time_t t) {
  the_gui->external_event (type, t);
}

void
needs_update () {
  // the editor asks for a repaint: do not sleep in the loop
  if (the_gui != NULL) the_gui->update_requested= true;
}

bool
check_event (int type) {
  return the_gui->check_event (type);
}

void
gui_interpose (void (*r) (void)) {
  the_interpose_handler= r;
}

void
set_default_font (string name) {
  the_gui->set_default_font (name);
}

font
get_default_font (bool tt, bool mini, bool bold) {
  return the_gui->default_font (tt, mini, bold);
}
