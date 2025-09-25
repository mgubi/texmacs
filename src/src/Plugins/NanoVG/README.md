# NanoVG Renderer for GNU TeXmacs

This directory contains a hardware-accelerated renderer implementation for GNU TeXmacs based on the NanoVG graphics library.

## Overview

The NanoVG renderer provides:
- **Hardware acceleration** through OpenGL
- **High-quality antialiasing** for smooth vector graphics
- **Scalable vector rendering** without pixelation
- **High-DPI display support** with proper pixel ratio handling
- **Modern graphics pipeline** leveraging GPU capabilities

## Features

### Core Rendering
- All standard TeXmacs drawing primitives (lines, arcs, polygons, rectangles)
- Advanced path operations with Bézier curves
- Hardware-accelerated antialiasing
- Proper coordinate system mapping with Y-axis flipping

### Text Rendering
- Two-tier text system: native NanoVG fonts with bitmap fallback
- Glyph caching for performance
- Support for complex mathematical fonts through bitmap rendering

### Graphics State
- Full pencil and brush support
- Line width, cap, and join styles
- Color conversion with alpha channel support
- Efficient state synchronization

### Advanced Features
- Shadow rendering for offscreen operations
- Clipping support with scissor testing
- Transformation matrix support
- Frame buffer management for compositing

## Dependencies

### Required
- **NanoVG**: Vector graphics library (headers in include path)
- **OpenGL**: 2.0+ or OpenGL ES 2.0+
- **Standard TeXmacs**: Core renderer interfaces and data structures

### Optional
- **FreeType**: For enhanced font support (already used by TeXmacs)
- **stb_image**: For image loading (included with NanoVG)

## Build Integration

### CMake
The renderer includes a `CMakeLists.txt` that automatically detects NanoVG availability:

```cmake
# From TeXmacs root CMakeLists.txt
add_subdirectory(src/Plugins/NanoVG)
```

### Autotools/Make
Add to the appropriate Makefile:

```makefile
# NanoVG renderer support
if USE_NANOVG
NANOVG_SOURCES = src/Plugins/NanoVG/nanovg_renderer.cpp
NANOVG_CFLAGS = -DUSE_NANOVG=1 -I/path/to/nanovg
NANOVG_LIBS = -lGL
endif
```

## Usage

### Basic Setup
```cpp
#include "nanovg_renderer.hpp"

// Create with default settings
renderer ren = nanovg_renderer(NANOVG_DEFAULT_FLAGS, 800, 600);

// Or use existing NanoVG context
NVGcontext* vg = nvgCreateGL3(NVG_ANTIALIAS | NVG_STENCIL_STROKES);
renderer ren = nanovg_renderer(vg, 800, 600);
```

### Rendering Loop
```cpp
nanovg_renderer_rep* nvg_ren = (nanovg_renderer_rep*)ren;

nvg_ren->begin_frame();

// Standard TeXmacs rendering calls
ren->set_pencil(pencil(red, 2*PIXEL));
ren->line(0, 0, 100*PIXEL, 100*PIXEL);
ren->fill(50*PIXEL, 50*PIXEL, 150*PIXEL, 100*PIXEL);

nvg_ren->end_frame();
```

### High-DPI Support
```cpp
// Set pixel ratio for retina displays
nvg_ren->set_frame_size(width, height, 2.0f);
```

### Integration with TeXmacs
```cpp
// Replace or supplement existing renderer
nanovg_renderer_rep* nvg_renderer = the_nanovg_renderer();
if (nvg_renderer && nvg_renderer->is_started()) {
    // Use NanoVG for vector operations
    current_renderer = nvg_renderer;
}
```

## Performance Considerations

### Optimizations
- **State batching**: Graphics state changes are minimized
- **Glyph caching**: Character bitmaps are cached as textures
- **Path batching**: Multiple path operations combined when possible
- **GPU memory**: Textures and vertex data kept on GPU

### Best Practices
- Call `begin_frame()` and `end_frame()` properly
- Minimize graphics state changes within a frame
- Use shadow renderers for complex compositing
- Prefer vector operations over bitmap when possible

## Architecture

### Class Hierarchy
```
basic_renderer_rep
└── nanovg_renderer_rep
    └── nanovg_shadow_renderer_rep
```

### Key Components
- **nanovg_renderer_rep**: Main renderer implementation
- **nanovg_shadow_renderer_rep**: Offscreen rendering support
- **nanovg_image**: Texture/image wrapper
- **Factory functions**: Global renderer management

### Coordinate System
- TeXmacs coordinates (SI units) → NanoVG coordinates (float pixels)
- Y-axis flipping handled automatically
- Proper zoom and transformation support

## Debugging

### Common Issues
1. **Context creation fails**: Check OpenGL version and extensions
2. **Clipping problems**: Ensure proper scissor state management
3. **Font rendering**: Verify font paths and fallback mechanisms
4. **Performance**: Monitor GPU memory usage and draw calls

### Debug Build
```bash
cmake -DCMAKE_BUILD_TYPE=Debug -DUSE_NANOVG=ON ..
```

### Logging
Enable debug output by setting appropriate compile flags in development builds.

## Future Enhancements

### Planned Features
- [ ] Native font rendering with NanoVG text API
- [ ] Image and picture rendering optimization
- [ ] Advanced shader support for mathematical rendering
- [ ] Better integration with TeXmacs color management
- [ ] Performance profiling and optimization tools

### Extensions
- Multi-threading support for large documents
- Custom shaders for specialized mathematical notation
- Advanced compositing modes
- Vector export capabilities (SVG, PDF)

## License

This renderer follows the same licensing as GNU TeXmacs (GPLv3+). See the main TeXmacs LICENSE file for details.