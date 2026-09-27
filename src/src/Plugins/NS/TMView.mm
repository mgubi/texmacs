
/******************************************************************************
* MODULE     : TMView.mm
* DESCRIPTION: Main TeXmacs view
* COPYRIGHT  : (C) 2007  Massimiliano Gubinelli
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#import "TMView.h"

#include "converter.hpp"
#include "message.hpp"

#include "ns_utilities.h"
#include "ns_renderer.h"
#include "ns_gui.h"

//extern bool ns_update_flag;
//extern int time_credit;
//extern int timeout_time;

hashmap<int,string> nskeymap("");

@interface TMView (Private)
- (void) focusIn;
- (void) focusOut;
@end

@implementation TMView

inline void
map (int code, string name)
{
  nskeymap(code) = name;
}

void
initkeymap () {
  map(0x0d,"return");
  map(0x09,"tab");
  map(0xf728,"backspace");
  map(0xf003,"enter");
  map(0x1b,"escape");
  map(0x0003,"K-enter");
  map(0x7f,"backspace");
  
  map( NSUpArrowFunctionKey       ,"up" );
  map( NSDownArrowFunctionKey     ,"down" );
  map( NSLeftArrowFunctionKey     ,"left" );
  map( NSRightArrowFunctionKey    ,"right" );
  map( NSF1FunctionKey    ,"F1" );
  map( NSF2FunctionKey    ,"F2" );
  map( NSF3FunctionKey    ,"F3" );
  map( NSF4FunctionKey    ,"F4" );
  map( NSF5FunctionKey    ,"F5" );
  map( NSF6FunctionKey    ,"F6" );
  map( NSF7FunctionKey    ,"F7" );
  map( NSF8FunctionKey    ,"F8" );
  map( NSF9FunctionKey    ,"F9" );
  map( NSF10FunctionKey   ,"F10" );
  map( NSF11FunctionKey   ,"F11" );
  map( NSF12FunctionKey   ,"F12" );
  map( NSF13FunctionKey   ,"F13" );
  map( NSF14FunctionKey   ,"F14" );
  map( NSF15FunctionKey   ,"F15" );
  map( NSF16FunctionKey   ,"F16" );
  map( NSF17FunctionKey   ,"F17" );
  map( NSF18FunctionKey   ,"F18" );
  map( NSF19FunctionKey   ,"F19" );
  map( NSF20FunctionKey   ,"F20" );
  map( NSF21FunctionKey   ,"F21" );
  map( NSF22FunctionKey   ,"F22" );
  map( NSF23FunctionKey   ,"F23" );
  map( NSF24FunctionKey   ,"F24" );
  map( NSF25FunctionKey   ,"F25" );
  map( NSF26FunctionKey   ,"F26" );
  map( NSF27FunctionKey   ,"F27" );
  map( NSF28FunctionKey   ,"F28" );
  map( NSF29FunctionKey   ,"F29" );
  map( NSF30FunctionKey   ,"F30" );
  map( NSF31FunctionKey   ,"F31" );
  map( NSF32FunctionKey   ,"F32" );
  map( NSF33FunctionKey   ,"F33" );
  map( NSF34FunctionKey   ,"F34" );
  map( NSF35FunctionKey   ,"F35" );
  map( NSInsertFunctionKey        ,"insert" );
  map( NSDeleteFunctionKey        ,"delete" );
  map( NSHomeFunctionKey  ,"home" );
  map( NSBeginFunctionKey         ,"begin" );
  map( NSEndFunctionKey   ,"end" );
  map( NSPageUpFunctionKey        ,"pageup" );
  map( NSPageDownFunctionKey      ,"pagedown" );
  map( NSPrintScreenFunctionKey   ,"printscreen" );
  map( NSScrollLockFunctionKey    ,"scrolllock" );
  map( NSPauseFunctionKey         ,"pause" );
  map( NSSysReqFunctionKey        ,"sysreq" );
  map( NSBreakFunctionKey         ,"break" );
  map( NSResetFunctionKey         ,"reset" );
  map( NSStopFunctionKey  ,"stop" );
  map( NSMenuFunctionKey  ,"menu" );
  map( NSUserFunctionKey  ,"user" );
  map( NSSystemFunctionKey        ,"system" );
  map( NSPrintFunctionKey         ,"print" );
  map( NSClearLineFunctionKey     ,"clear" );
  map( NSClearDisplayFunctionKey  ,"cleardisplay" );
  map( NSInsertLineFunctionKey    ,"insertline" );
  map( NSDeleteLineFunctionKey    ,"deleteline" );
  map( NSInsertCharFunctionKey    ,"insert" );
  map( NSDeleteCharFunctionKey    ,"delete" );
  map( NSPrevFunctionKey  ,"prev" );
  map( NSNextFunctionKey  ,"next" );
  map( NSSelectFunctionKey        ,"select" );
  map( NSExecuteFunctionKey       ,"execute" );
  map( NSUndoFunctionKey  ,"undo" );
  map( NSRedoFunctionKey  ,"redo" );
  map( NSFindFunctionKey  ,"find" );
  map( NSHelpFunctionKey  ,"help" );
  map( NSModeSwitchFunctionKey    ,"modeswitch" );  
}


- (id) initWithFrame: (NSRect)frame {
  self = [super initWithFrame:frame];
  if (self) {
    // Initialization code here.
    wid = NULL;
    processingCompose = NO;
    workingText = nil;
  }
  return self;
}

-(void) dealloc
{
  [self deleteWorkingText];
  [[NSNotificationCenter defaultCenter] removeObserver: self
                                                  name: @"NSWindowDidBecomeKeyNotification"
                                                object: nil];
  [[NSNotificationCenter defaultCenter] removeObserver: self
                                                  name: @"NSWindowDidBecomeKeyNotification"
                                                object: nil];
  
  [super dealloc];
}

- (void) setWidget: (widget_rep*) w
{
	wid = (simple_widget_rep*) w;
}

- (widget_rep*) widget
{
	return  (widget_rep*)wid;
}

- (void) viewWillMoveToWindow: (NSWindow *)newWindow
{
  // query widget preferred size
  SI w = 0, h = 0;
  wid->handle_get_size_hint (w, h);
  [self setFrameSize: to_nssize (w, h)];
  
  // register to receive focus in/out notifications  
  [[NSNotificationCenter defaultCenter] removeObserver: self
                                                  name: @"NSWindowDidBecomeKeyNotification"
                                                object: nil];
  [[NSNotificationCenter defaultCenter] removeObserver: self
                                                  name: @"NSWindowDidBecomeKeyNotification"
                                                object: nil];
  
  [[NSNotificationCenter defaultCenter] addObserver: self
                                           selector: @selector(focusIn)
                                               name: @"NSWindowDidBecomeKeyNotification"
                                             object: newWindow];
  
  [[NSNotificationCenter defaultCenter] addObserver: self
                                           selector: @selector(focusOut)
                                               name: @"NSWindowDidResignKeyNotification"
                                             object: newWindow];
  
}

- (void) focusIn
{
  if (DEBUG_EVENTS) cout << "FOCUSIN" << LF;
  if (wid) {
      if (DEBUG_QT) debug_qt << "FOCUSIN: " << wid->type_as_string () << LF;
      the_gui->process_keyboard_focus (wid, true, texmacs_time ());
  }
}

- (void) focusOut
{
  if (DEBUG_EVENTS)   cout << "FOCUSOUT" << LF;
  if (wid) {
    if (DEBUG_QT) debug_qt << "FOCUSOUT: " << wid->type_as_string () << LF;
    the_gui -> process_keyboard_focus (wid, false, texmacs_time ());
  }
}

- (void) drawRect: (NSRect)rect
{
  // Copy the corresponding part of the backing store, which has
  // retina_factor pixels per point; the view is flipped, and so is the
  // backing store, so that the coordinates correspond directly
  if (!wid || !wid->backingPixmap) return;
  NSRect src= NSMakeRect (rect.origin.x * retina_factor,
                          rect.origin.y * retina_factor,
                          rect.size.width * retina_factor,
                          rect.size.height * retina_factor);
  [wid->backingPixmap drawInRect: rect fromRect: src
                       operation: NSCompositingOperationCopy
                        fraction: 1.0 respectFlipped: NO hints: nil];
}

#if 0
- (void)keyDown:(NSEvent *)theEvent
{
  if (!wid) return;
  
  {
    char str[256];
    string r;
    NSString *nss = [theEvent charactersIgnoringModifiers];
    unsigned int mods = [theEvent modifierFlags];
    
    
    
    if (([nss length]==1)&& (!processingCompose))
      
    {
      int key = [nss characterAtIndex:0];
      if (nskeymap->contains(key)) {
        r = nskeymap[key];
        r = ((mods & NSShiftKeyMask)? "S-" * r: r);
      }
      else
      {
        [nss getCString:str maxLength:256 encoding:NSUTF8StringEncoding];
        string rr (str, strlen(str));
        r= utf8_to_cork (rr);          
      } 
      
      
      string s (r);
      if (! contains_unicode_char (s))     
      {
        //      string s= ((mods & NSShiftKeyMask)? "S-" * r: r);
        /* other keyboard modifiers */
        if (N(s)!=0) {
          if (mods & NSControlKeyMask ) s= "C-" * s;
          if (mods & NSAlternateKeyMask) s= "A-" * s;
          if (mods & NSCommandKeyMask) s= "M-" * s;
          // if (mods & NSNumericPadKeyMask) s= "K-" * s;
	  // if (mods & NSHelpKeyMask) s= "H-" * s;
          // if (mods & NSFunctionKeyMask) s= "F-" * s;
        }
        cout << "key press: " << s << LF;
        wid -> handle_keypress (s, texmacs_time());    
      }
    }
    else {
      processingCompose = YES;
      static NSMutableArray *nsEvArray = nil;
      if (nsEvArray == nil)
        nsEvArray = [[NSMutableArray alloc] initWithCapacity: 1];
      
      [nsEvArray addObject: theEvent];
      [self interpretKeyEvents: nsEvArray];
      [nsEvArray removeObject: theEvent];
    }
  }	
  
  
}
#else
- (void)keyDown:(NSEvent *)theEvent
{
  if (!wid) return;

  static bool fInit = false;
  if (!fInit) {
    if (DEBUG_EVENTS)
      cout << "Initializing keymap\n";
    initkeymap();
    fInit= true;
  }
  
  {
    // char str[256];
    string r;
    NSString *nss = [theEvent charactersIgnoringModifiers];
    unsigned int mods = [theEvent modifierFlags];
    
    string modstr;
    
    if (mods & NSControlKeyMask ) modstr= "C-" * modstr;
    if (mods & NSAlternateKeyMask) modstr= "A-" * modstr;
    if (mods & NSCommandKeyMask) modstr= "M-" * modstr;
    // if (mods & NSNumericPadKeyMask) modstr= "K-" * modstr;
    // if (mods & NSHelpKeyMask) modstr= "H-" * modstr;
    // if (mods & NSFunctionKeyMask) modstr= "F-" * modstr;
    
    //    if (!processingCompose)
    {
      if ([nss length]>0) {
        int key = [nss characterAtIndex:0];
        if (nskeymap->contains(key)) {
          r = nskeymap[key];
          r = ((mods & NSShiftKeyMask)? "S-" * modstr: modstr) * r;          
          if (DEBUG_QT && DEBUG_KEYBOARD) debug_qt << "key press: " << r << LF;
          [self deleteWorkingText];
          the_gui->process_keypress (wid, r, texmacs_time());
          return;
        } else if (mods & (NSControlKeyMask  | NSCommandKeyMask | NSHelpKeyMask))
        {
          static char str[256];
          [nss getCString:str maxLength:256 encoding:NSUTF8StringEncoding];
          string rr (str, strlen(str));
          r= utf8_to_cork (rr);          
          
          string s ( modstr * r);
          [self deleteWorkingText];
          
          if (DEBUG_QT && DEBUG_KEYBOARD) debug_qt << "key press: " << s << LF;
          the_gui->process_keypress (wid, s, texmacs_time());

          return;
        }
      }
    }
    
    processingCompose = YES;
    static NSMutableArray *nsEvArray = nil;
    if (nsEvArray == nil)
      nsEvArray = [[NSMutableArray alloc] initWithCapacity: 1];
    
    [nsEvArray addObject: theEvent];
    [self interpretKeyEvents: nsEvArray];
    [nsEvArray removeObject: theEvent];
  }
}

#endif

static unsigned int
mouse_state (NSEvent* event, bool flag) {
  // As in the Qt interface on the Mac: control and option emulate the right
  // and middle buttons, but the modifiers are passed anyway
  (void) flag;
  unsigned int i= 0;
  NSInteger b= [event buttonNumber];
  switch ([event type]) {
    case NSEventTypeLeftMouseDown: case NSEventTypeLeftMouseUp:
    case NSEventTypeLeftMouseDragged:
      b= 0; break;
    case NSEventTypeRightMouseDown: case NSEventTypeRightMouseUp:
    case NSEventTypeRightMouseDragged:
      b= 1; break;
    default: break;
  }
  if (b == 0) i += 1;
  else if (b == 1) i += 4;
  else if (b == 2) i += 2;
  else if (b == 3) i += 8;
  else if (b == 4) i += 16;
  NSEventModifierFlags mods = [event modifierFlags];
  if (mods & NSEventModifierFlagControl) i = 1024 + 4;
  if (mods & NSEventModifierFlagOption)  i = 2048 + 2;
  if (mods & NSEventModifierFlagShift)   i += 256;
  if (mods & NSEventModifierFlagCommand) i += 4096;
  return i;
}

static string
mouse_decode (unsigned int mstate) {
  if      (mstate & 1 ) return "left";
  else if (mstate & 2 ) return "middle";
  else if (mstate & 4 ) return "right";
  else if (mstate & 8 ) return "up";
  else if (mstate & 16) return "down";
  return "unknown";
}

- (void) mouseDown: (NSEvent *)event
{
  if (wid) {
    NSPoint point = [[self superview] convertPoint: [event locationInWindow] fromView: nil];
    coord2 pt = from_nspoint (point);
    unsigned int mstate = mouse_state (event, false);
    string s = "press-" * mouse_decode (mstate);
    the_gui -> process_mouse (wid, s, pt.x1, pt.x2, mstate, texmacs_time ());
  }
}

- (void) mouseUp: (NSEvent *)event
{
  if (wid) {
    NSPoint point = [[self superview] convertPoint: [event locationInWindow] fromView: nil];
    coord2 pt = from_nspoint (point);
    unsigned int mstate = mouse_state (event, false);
    string s = "release-" * mouse_decode (mstate);
    the_gui -> process_mouse (wid, s, pt.x1, pt.x2, mstate, texmacs_time ());
  }
}

- (void) mouseDragged: (NSEvent *)event
{
  if (wid) {
    NSPoint point = [[self superview] convertPoint: [event locationInWindow] fromView: nil];
    coord2 pt = from_nspoint (point);
    unsigned int mstate = mouse_state (event, false);
    string s = "move";
    the_gui -> process_mouse (wid, s, pt.x1, pt.x2, mstate, texmacs_time ());
  }
}

- (void) rightMouseDown: (NSEvent *)event { [self mouseDown: event]; }
- (void) rightMouseUp: (NSEvent *)event { [self mouseUp: event]; }
- (void) rightMouseDragged: (NSEvent *)event { [self mouseDragged: event]; }
- (void) otherMouseDown: (NSEvent *)event { [self mouseDown: event]; }
- (void) otherMouseUp: (NSEvent *)event { [self mouseUp: event]; }
- (void) otherMouseDragged: (NSEvent *)event { [self mouseDragged: event]; }

- (void) mouseMoved: (NSEvent *)event
{
  if (wid) {
    NSPoint point = [[self superview] convertPoint: [event locationInWindow] fromView: nil];
    coord2 pt = from_nspoint (point);
    unsigned int mstate = mouse_state (event, false);
    string s = "move";
    the_gui -> process_mouse (wid, s, pt.x1, pt.x2, mstate, texmacs_time ());
  }
}

- (BOOL) isFlipped
{
  return YES;
}

- (BOOL) isOpaque
{
  return YES;
}

- (void) resizeWithOldSuperviewSize: (NSSize)oldBoundsSize
{
  [super resizeWithOldSuperviewSize: oldBoundsSize];
  if (wid)  {
    NSSize size = [self bounds].size;
    coord2 s = from_nssize (size);
    the_gui -> process_resize (wid, s.x1, s.x2);
  }
}

- (BOOL) acceptsFirstMouse: (NSEvent*) event
{
  // NOTE: a click in an inactive window also positions the cursor, as in
  // the Qt interface
  (void) event;
  return YES;
}

- (BOOL) acceptsFirstResponder
{
	return YES;
}

- (void) deleteWorkingText
{ 
  if (workingText == nil)
    return;
  [workingText release];
  workingText = nil;
  processingCompose = NO;
}

#pragma mark NSTextInputClient protocol implementation

// NOTE: as in QTMWidget::inputMethodEvent, the text being composed by an
// input method is sent to TeXmacs as a key "pre-edit:<pos>:<text>", and the
// composed text as ordinary keys

static NSString*
plain_string (id s) {
  return [s isKindOfClass: [NSAttributedString class]]? [s string]: s;
}

- (void) sendPreEdit: (NSString*) str position: (NSUInteger) pos
{
  if (!wid) return;
  string r= "pre-edit:";
  if ([str length] > 0)
    r= r * as_string ((int) pos) * ":" * from_nsstring (str);
  the_gui->process_keypress (wid, r, texmacs_time ());
}

- (void) insertText: (id) aString replacementRange: (NSRange) replacementRange
{
  (void) replacementRange;
  NSString *str= plain_string (aString);
  if (workingText) {
    [self deleteWorkingText];
    [self sendPreEdit: @"" position: 0];
  }
  processingCompose = NO;
  if (!wid) return;
  for (NSUInteger i=0; i<[str length]; i++) {
    NSString* c= [str substringWithRange: [str rangeOfComposedCharacterSequenceAtIndex: i]];
    i += [c length] - 1;
    string s= from_nsstring (c);
    if (DEBUG_QT && DEBUG_KEYBOARD) debug_qt << "key press: " << s << LF;
    the_gui->process_keypress (wid, s, texmacs_time ());
  }
}

- (void) doCommandBySelector: (SEL) aSelector
{
  // NOTE: the keys with a command are handled in keyDown:
  (void) aSelector;
}

- (void) setMarkedText: (id) aString selectedRange: (NSRange) selRange
      replacementRange: (NSRange) replacementRange
{
  (void) replacementRange;
  NSString *str= plain_string (aString);
  [self deleteWorkingText];
  if ([str length] > 0) {
    workingText = [str copy];
    processingCompose = YES;
  }
  [self sendPreEdit: str position: selRange.location];
}

- (void) unmarkText
{
  [self deleteWorkingText];
  [self sendPreEdit: @"" position: 0];
}

- (BOOL) hasMarkedText
{
  return workingText != nil;
}

- (NSRange) markedRange
{
  return workingText != nil
    ? NSMakeRange (0, [workingText length]) : NSMakeRange (NSNotFound, 0);
}

- (NSRange) selectedRange
{
  return NSMakeRange (NSNotFound, 0);
}

- (NSAttributedString *) attributedSubstringForProposedRange: (NSRange) range
                                                 actualRange: (NSRangePointer) actualRange
{
  (void) range; (void) actualRange;
  return nil;
}

- (NSArray*) validAttributesForMarkedText
{
  return [NSArray array];
}

- (NSRect) firstRectForCharacterRange: (NSRange) range
                          actualRange: (NSRangePointer) actualRange
{
  // The cursor on the screen, for the windows of the input methods
  (void) range; (void) actualRange;
  NSPoint p= wid? wid->cursor_pos: NSZeroPoint;
  NSRect r= [[self superview] convertRect: NSMakeRect (p.x, p.y, 1, 16) toView: nil];
  return [self window]? [[self window] convertRectToScreen: r]: r;
}

- (NSUInteger) characterIndexForPoint: (NSPoint) point
{
  (void) point;
  return NSNotFound;
}

@end
