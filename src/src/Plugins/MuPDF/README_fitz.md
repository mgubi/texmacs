# Fitz Renderer

A direct implementation of a TeXmacs renderer using MuPDF's core Fitz API.

## Overview

The Fitz renderer provides a clean, direct interface to MuPDF's graphics capabilities through the Fitz device API. Unlike the existing MuPDF renderer which uses PDF processors, this renderer works directly with Fitz devices and pixmaps for more efficient rendering.

## Architecture

### Core Components

- **fitz_renderer_rep**: Main renderer class implementing the TeXmacs renderer interface
- **fitz_factory**: Factory functions for creating Fitz renderers and pictures
- **Fitz Device API**: Direct use of `fz_device` for graphics operations
- **Pixmap Management**: Uses `fz_pixmap` for bitmap rendering target

### Key Features

1. **Direct Device Rendering**: Uses `fz_new_draw_device()` for direct pixmap rendering
2. **Path-based Graphics**: All graphics primitives built using `fz_path` operations
3. **Text Rendering**: Character glyphs rendered as cached bitmap images
4. **Color Management**: Full RGBA color support with proper alpha blending
5. **Transformation Support**: Matrix transformations for scaling, rotation, etc.

### Compared to Existing Renderers

| Feature | MuPDF Renderer | Fitz Renderer | NanoVG Renderer |
|---------|----------------|---------------|------------------|
| API Used | PDF Processor | Fitz Device | NanoVG Context |
| Target | PDF Generation | Direct Rendering | Vector Graphics |
| Text | Native Fonts | Bitmap Glyphs | Font Atlas |
| Performance | Medium | High | Very High |
| Memory | High | Medium | Low |

## Implementation Details

### Graphics Primitives

```cpp
// Line drawing
void line(SI x1, SI y1, SI x2, SI y2) {
    begin_path();
    fz_moveto(ctx, current_path, to_fitz_x(x1), to_fitz_y(y1));
    fz_lineto(ctx, current_path, to_fitz_x(x2), to_fitz_y(y2));
    stroke_current_path();
    end_path();
}

// Rectangle filling
void fill(SI x1, SI y1, SI x2, SI y2) {
    begin_path();
    // Build rectangle path
    fill_current_path();
    end_path();
}
```

### Text Rendering

The renderer uses a hybrid approach with native font rendering and bitmap fallback:

1. **Native Font Loading**: Uses `tt_font_find()` to locate font files and `fz_new_font_from_file()` to load them
2. **Font Size Extraction**: Parses font size and DPI from font names (e.g., "cmr10.600.300")
3. **Glyph Index Decoding**: Uses the same logic as MuPDF renderer for proper glyph mapping
4. **Native Rendering**: Uses `fz_fill_text()` with proper `fz_text` objects for optimal quality
5. **Bitmap Fallback**: Falls back to cached bitmap glyphs for fonts that can't be loaded natively

**Font Loading Process:**
```cpp
fz_font* load_fitz_font(string fontname) {
    string fname = extract_font_family(fontname);
    url font_path = tt_font_find(fname);
    fz_font* font = fz_new_font_from_file(ctx, font_path);
    setup_freetype_encoding(font);
    return font;
}
```

**Native Text Rendering:**
```cpp
void draw(int char_code, font_glyphs fn, SI x, SI y) {
    setup_font(fn);
    if (current_font) {
        unsigned int glyph_id = decode_glyph_index(current_font, char_code);
        fz_text* text = create_text_object(glyph_id, x, y);
        fz_fill_text(ctx, device, text, ...);
    } else {
        // Fallback to bitmap rendering
    }
}
```

### Image and Picture Rendering

The renderer provides comprehensive image support following the MuPDF renderer blueprint:

1. **Multi-format Support**: JPG, PNG, GIF, BMP, TIFF, XPM (with PNG fallbacks)
2. **Image Caching**: Loaded images are cached using URL-based keys
3. **Picture Conversion**: Automatic conversion between picture formats
4. **Effects Processing**: Support for image effects and transformations
5. **Scalable Images**: Efficient handling of scalable images with proper scaling

**Image Loading Process:**
```cpp
fz_image* fitz_load_image(url u) {
    // Try direct loading for supported formats
    if (supported_format(u)) {
        return fz_new_image_from_file(ctx, path);
    }
    // Handle XPM with PNG fallbacks (_x4.png, _x2.png, .png)
    // Convert unsupported formats using TeXmacs converters
}
```

**Picture Rendering:**
```cpp
void draw_picture(picture pict, SI x, SI y, int alpha) {
    picture p = as_fitz_picture(pict);  // Convert to Fitz format
    fitz_picture_rep* rep = get_picture_handle(p);
    fz_image* im = fz_new_image_from_pixmap(ctx, rep->pix);
    fz_fill_image(ctx, device, im, transform, alpha);
}
```

**Scalable Image Rendering:**
```cpp
void draw_scalable(scalable im, SI x, SI y, int alpha) {
    url u = im->get_name();
    fz_image* cached_im = load_and_cache_image(u);
    fz_matrix transform = compute_scaling_transform(im, x, y);
    fz_fill_image(ctx, device, cached_im, transform, alpha);
}
```

### Pattern and Brush Support

The renderer provides comprehensive pattern and brush support following the MuPDF renderer blueprint:

1. **Pattern Loading**: Uses `get_pattern_data()` to extract pattern information from brushes
2. **Shade Creation**: Converts pattern images to Fitz shades for efficient rendering
3. **Pattern Caching**: Caches created shades using pattern tree keys
4. **Brush Integration**: Seamless integration with TeXmacs brush system
5. **Fill/Stroke Support**: Patterns can be used for both fill and stroke operations

**Pattern Registration Process:**
```cpp
void register_pattern(brush br, SI pixel) {
    tree pattern_key = br->get_pattern();
    url u; SI w, h; tree eff;
    get_pattern_data(u, w, h, eff, br, pixel);

    fz_shade* shade = create_pattern_shade(u, w, h, eff, pixel);
    pattern_cache(pattern_key) = shade;
}
```

**Pattern Rendering:**
```cpp
void fill_current_path() {
    if (current_fill_pattern) {
        fz_fill_shade(ctx, device, current_fill_pattern, transform, alpha);
    } else {
        // Standard color fill
        fz_fill_path(ctx, device, current_path, ...);
    }
}
```

**Brush Handling:**
```cpp
void set_brush(brush br) {
    if (br->get_type() == brush_pattern) {
        select_fill_pattern(br);  // Load and cache pattern
    } else {
        current_fill_pattern = NULL;  // Use solid color
    }
}
```

### Picture Renderer

The Fitz renderer includes a specialized picture renderer for direct rendering to pictures:

1. **Picture-to-Picture Rendering**: Renders directly onto picture objects
2. **Zoom Support**: Built-in zoom factor handling for high-resolution rendering
3. **Native Integration**: Seamless integration with TeXmacs picture system
4. **Format Support**: Creates and manipulates Fitz-native picture format

**Picture Renderer Creation:**
```cpp
// Create a picture renderer with zoom
renderer ren = picture_renderer(picture_obj, 2.0);  // 2x zoom

// Use like any other renderer
ren->set_pencil(pencil(black, 2));
ren->line(0, 0, 100, 100);
ren->fill(50, 50, 150, 150);
```

**Native Picture Functions:**
```cpp
// Create blank picture
picture pic = native_picture(800, 600, 0, 0);

// Load from file with effects
picture loaded = load_picture(url, width, height, effects, pixel);

// Save to PNG
save_picture(url("output.png"), pic);

// Convert to native format
picture native = as_native_picture(any_picture);
```

**Picture Renderer Implementation:**
```cpp
class fitz_picture_renderer_rep : public fitz_renderer_rep {
    picture pict;
public:
    fitz_picture_renderer_rep(picture p, double zoom) {
        // Set up zoom factors like MuPDF renderer
        zoomf = zoom;
        shrinkf = (int)tm_round(std_shrinkf / zoomf);
        pixel = (SI)tm_round((std_shrinkf * PIXEL) / zoomf);

        // Convert to Fitz format and begin rendering
        picture fitz_pict = as_fitz_picture(pict);
        fitz_picture_rep* handle = get_picture_handle(fitz_pict);
        begin(handle->pix);  // Start rendering to picture's pixmap
    }
};
```

### Color Management

```cpp
void fitz_color_from_color(color c, float *fz_color, int *alpha) {
    int r, g, b, a;
    get_rgb_color(c, r, g, b, a);
    fz_color[0] = ((float)r) / 255.0f;
    fz_color[1] = ((float)g) / 255.0f;
    fz_color[2] = ((float)b) / 255.0f;
    *alpha = a;
}
```

## Usage

### Basic Usage

```cpp
#include "fitz_factory.hpp"

// Create a renderer
renderer ren = fitz_renderer(800, 600);

// Set graphics state
ren->set_pencil(pencil(black, 2));
ren->set_brush(brush(red));

// Draw primitives
ren->line(0, 0, 100, 100);
ren->fill(50, 50, 150, 150);

// Clean up
delete_renderer(ren);
```

### Picture Rendering

```cpp
// Render to a picture
picture pic = ...; // source picture
renderer ren = fitz_renderer(pic, 2.0); // 2x zoom

// Render content
// ...

// Extract result as picture
picture result = ren->get_picture();
```

## Current Status

### Implemented Features

- ✅ Basic graphics primitives (lines, rectangles, arcs)
- ✅ Color management with RGBA support
- ✅ Path-based rendering using Fitz paths
- ✅ Native text rendering with FreeType integration
- ✅ Font loading and caching system
- ✅ Hybrid text rendering (native + bitmap fallback)
- ✅ Image and picture rendering with format support
- ✅ Scalable image rendering with caching
- ✅ Picture conversion and effects processing
- ✅ Pattern fills and brush support
- ✅ Pattern caching and management
- ✅ Picture renderer for direct picture editing
- ✅ Native picture creation and manipulation
- ✅ Picture loading and saving (PNG format)
- ✅ Coordinate transformation system
- ✅ Factory pattern for renderer creation

### Partially Implemented

- 🚧 Clipping operations (basic rectangle clipping)
- 🚧 Shadow operations (stubs in place)
- 🚧 Advanced image scaling and filtering

### Not Yet Implemented

- ❌ Complex transformations (rotation, skew)
- ❌ Advanced image scaling and filtering
- ❌ Shadow and transparency effects
- ❌ Performance optimizations
- ❌ Advanced pattern types (radial gradients, mesh patterns)

## Advantages

1. **Direct API Access**: No intermediate PDF generation overhead
2. **Native Text Rendering**: High-quality text using actual font files with proper glyph mapping
3. **Comprehensive Image Support**: Full image format support with intelligent caching
4. **Pattern and Brush Support**: Complete pattern fill/stroke system using Fitz shades
5. **Memory Efficient**: Direct pixmap rendering without document structures
6. **Font Compatibility**: Uses same font discovery system as existing TeXmacs renderers
7. **Image Compatibility**: Follows same image loading patterns as MuPDF renderer
8. **Pattern Compatibility**: Uses same pattern data extraction as MuPDF renderer
9. **Clean Architecture**: Simple device-based rendering model
10. **Hybrid Fallback**: Graceful degradation for fonts, images, and patterns
11. **Effects Support**: Compatible with TeXmacs image effects system
12. **Extensible**: Easy to add new graphics primitives
13. **Well-Documented**: MuPDF Fitz API is well-documented

## Future Improvements

1. **Advanced Graphics**: Add support for gradients, patterns, and effects
2. **Performance**: Optimize path building and caching, font loading
3. **Text Optimization**: Batch text rendering, better font caching strategies
4. **Image Optimization**: Advanced scaling algorithms, better caching strategies
5. **Vector Output**: Add support for vector output formats (SVG, PDF)
6. **GPU Acceleration**: Potential integration with OpenGL backend
7. **Font Features**: Support for font styles, weights, and OpenType features
8. **Image Effects**: Enhance image effects processing and add new effects

## Dependencies

- MuPDF library (Fitz API)
- TeXmacs renderer interface
- FreeType (for font handling)

## Building

The Fitz renderer is built as part of the TeXmacs plugin system. Ensure MuPDF development headers are available:

```bash
# Example for Ubuntu/Debian
sudo apt-get install libmupdf-dev

# Example for macOS with Homebrew
brew install mupdf
```

The renderer will be automatically built when the Fitz plugin is enabled in the TeXmacs build configuration.