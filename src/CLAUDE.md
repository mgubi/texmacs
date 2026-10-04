# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

GNU TeXmacs is a free, cross-platform scientific document editor providing a "WYSIWYW" (What You See Is What You Want) editing environment. The codebase combines C++17 core functionality with Scheme/Guile scripting and Qt for the GUI.

## Build Commands

### Primary Build System (Autotools)
```bash
make TEXMACS          # Build dynamic version (default)
make STATIC_TEXMACS   # Build static version
make PLUGINS          # Build plugins
make DEPS             # Generate dependencies
make install          # Install the application
```

### Alternative Build System (CMake)
```bash
mkdir build && cd build
cmake ..
make -j8
```

### Configuration Options
```bash
./configure --prefix=[directory]     # Custom install location
./configure --enable-debug           # Debug build
./configure --disable-optimize       # Disable optimizations
```

## Testing

### C++ Tests
```bash
ctest                    # Run all tests
ctest -R [pattern]       # Run tests matching pattern
```

### Scheme Tests
```bash
TeXmacs -x "(run-all-tests)" -q     # Command line
# Or within Scheme session: (run-all-tests)
```

## Architecture

### Core Source Structure (`src/`)
- **`Kernel/`** - Core data structures, containers, types, and abstractions
- **`System/`** - System-level functionality (files, networking, boot, language support)
- **`Graphics/`** - Rendering engine, fonts, colors, mathematics, GUI abstractions
- **`Typeset/`** - Document typesetting and box-based layout system
- **`Edit/`** - Editor functionality, interface, modification, and process handling
- **`Style/`** - Document styling and evaluation system
- **`Texmacs/`** - Main application logic, server, and window management
- **`Scheme/`** - Scheme/Guile integration and language bindings
- **`Data/`** - Data conversion, parsing, and observer patterns
- **`Plugins/`** - Platform-specific implementations (Qt, Unix)

### Application Structure (`TeXmacs/`)
- **`progs/`** - Scheme programs and extensions
- **`styles/`** - Document style definitions and packages
- **`plugins/`** - External application integrations (50+ tools supported)
- **`fonts/`** - Font files and definitions

### Key Architectural Patterns
1. **Modular Design**: Clear layered separation (kernel → system → graphics → typeset → editor)
2. **Plugin Architecture**: Extensible system supporting Computer Algebra Systems, Programming Languages, Graphics tools
3. **Dual Language Core**: C++ for performance-critical components, Scheme for high-level logic and extensions
4. **Document Object Model**: Tree-based document representation with observer patterns
5. **Cross-platform Abstraction**: Platform-specific code isolated in plugins directory

## Development Notes

- **Environment**: Set `TEXMACS_PATH` environment variable for runtime
- **Dependencies**: Requires Guile Scheme, FreeType 2, libiconv; optional aspell, ImageMagick
- **Version Control**: Primary SVN repository on Savannah, GitHub mirror for visibility
- **Plugin Integration**: New plugins follow established patterns in `plugins/` directory
- **Testing**: C++ tests in `tests/` directory use CTest, Scheme tests integrated into application