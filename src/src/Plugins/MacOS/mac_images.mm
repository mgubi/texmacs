
/******************************************************************************
* MODULE     : mac_images.mm
* DESCRIPTION: interface with the MacOSX image conversion facilities
* COPYRIGHT  : (C) 2009  Massimiliano Gubinelli
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "config.h"
#include "MacOS/mac_images.h"

#if !defined(QTTEXMACS) \
    || !defined(AC_QT_MAJOR_VERSION) || AC_QT_MAJOR_VERSION < 6

#include "converter.hpp"
#include "wencoding.hpp"
#include "MacOS/mac_cocoa.h"
#include "ApplicationServices/ApplicationServices.h"

static NSString *
to_nsstring_utf8 (string s) {
  // NOTE: the names of files come in both encodings (cork from the drops,
  // UTF-8 from the file chooser), hence the heuristic of to_qstring
  if (!(looks_utf8 (s) && !(looks_ascii (s) || looks_universal (s))))
    s= cork_to_utf8 (s);
  c_string p = c_string (s);
  NSString *nss = [NSString stringWithCString:p encoding:NSUTF8StringEncoding];
  return nss;
}

void mac_image_to_png (url img_file, url png_file, int w, int h) {
  // we need to be sure that the Cocoa application infrastructure is initialized 
  // (apparently Qt does not do this properly)
  NSApplication *NSApp=[NSApplication sharedApplication]; (void) NSApp;
  NSAutoreleasePool *pool = [[NSAutoreleasePool alloc] init];

  NSImage *image = [[NSImage alloc] initWithContentsOfFile: to_nsstring_utf8 ( concretize (img_file) )];
  if (!image || w < 1 || h < 1) {
    [image release];
    [pool release];
    return;
  }
  // NOTE: the image is drawn in a bitmap of exactly w x h pixels (with
  // lockFocus, it would be captured at the scale of the screen)
  NSBitmapImageRep *bmp =
    [[NSBitmapImageRep alloc] initWithBitmapDataPlanes: NULL
                                            pixelsWide: w
                                            pixelsHigh: h
                                         bitsPerSample: 8
                                       samplesPerPixel: 4
                                              hasAlpha: YES
                                              isPlanar: NO
                                        colorSpaceName: NSDeviceRGBColorSpace
                                           bytesPerRow: 4 * w
                                          bitsPerPixel: 32];
  memset ([bmp bitmapData], 0, 4 * w * h);
  [NSGraphicsContext saveGraphicsState];
  [NSGraphicsContext setCurrentContext:
    [NSGraphicsContext graphicsContextWithBitmapImageRep: bmp]];
  [image drawInRect: NSMakeRect (0, 0, w, h) fromRect: NSZeroRect
          operation: NSCompositingOperationCopy fraction: 1.0];
  [NSGraphicsContext restoreGraphicsState];
  [image release];
  NSData *png_data = [bmp representationUsingType: NSBitmapImageFileTypePNG properties: [NSDictionary dictionary]];
  [png_data writeToURL:[NSURL fileURLWithPath: to_nsstring_utf8 ( concretize (png_file))] atomically: YES];
  [bmp release];
  [pool release];
} 

bool mac_image_size (url img_file, int& w, int& h) 
{
  string suf= suffix (img_file);
  if (suf == "ps" || suf == "eps" || suf == "pdf") return false;

  bool res = false; 
  // we need to be sure that the Cocoa application infrastructure is initialized 
  // (apparently Qt does not do this properly)
  NSApplication *NSApp=[NSApplication sharedApplication]; (void) NSApp;
  NSAutoreleasePool *pool = [[NSAutoreleasePool alloc] init];
  
  NSImage *image = [[NSImage alloc] initWithContentsOfFile: to_nsstring_utf8 ( concretize (img_file) )];
  if (image) {
    NSSize size = [image size];
    [image release];
    //NSLog(@"Probing  image size %f %f.\n", size.width, size.height);
    w = size.width;
    h = size.height;
    res = true;
  }
  [pool release];
  return res;
}

bool mac_supports (url img_file) {
  int w, h;
  return mac_image_size (img_file, w, h);
}

void mac_ps_to_pdf (url ps_file, url pdf_file) 
{
  NSAutoreleasePool *pool = [[NSAutoreleasePool alloc] init];
  NSString *inpath = to_nsstring_utf8 ( concretize (ps_file) );
  NSString *outpath = to_nsstring_utf8 ( concretize (pdf_file) );
  NSURL *inurl = [NSURL fileURLWithPath:inpath];
  NSURL *outurl = [NSURL fileURLWithPath: outpath];
  
  CGPSConverterCallbacks callbacks = {
    0, // unsigned int version;
    nil, // CGPSConverterBeginDocumentCallback beginDocument;
    nil, // CGPSConverterEndDocumentCallback endDocument;
    nil, // CGPSConverterBeginPageCallback beginPage;
    nil, // CGPSConverterEndPageCallback endPage;
    nil, // CGPSConverterProgressCallback noteProgress;
    nil, // CGPSConverterMessageCallback noteMessage;
    nil  // CGPSConverterReleaseInfoCallback releaseInfo;
  };
  
  CGPSConverterRef converter = CGPSConverterCreate (NULL,&callbacks,NULL);  
  CGDataProviderRef provider = CGDataProviderCreateWithURL ((CFURLRef)inurl);
  CGDataConsumerRef consumer = CGDataConsumerCreateWithURL ((CFURLRef)outurl);
  
  BOOL converted = CGPSConverterConvert (converter,provider,consumer,NULL);
  
  if (converted) {
    NSLog(@"Postscript file converted.\n");
    //CGDataConsumerRetain(consumer);
  } else {
    NSLog(@"Converting postscript failed.\n");
  }
  
  CGDataProviderRelease (provider);
  CGDataConsumerRelease (consumer);
  CFRelease (converter);
  
  [pool release];
}

#endif // not QTTEXMACS
