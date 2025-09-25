# NanoVG Renderer Plugin for TeXmacs

# Check if NanoVG is available
find_path(NANOVG_INCLUDE_DIR nanovg.h
    PATHS
        /usr/local/include
        /opt/local/include
        ${CMAKE_INSTALL_PREFIX}/include
        ~/nanovg/src
)

if(NANOVG_INCLUDE_DIR)
    message(STATUS "Found NanoVG headers at: ${NANOVG_INCLUDE_DIR}")

    # Add NanoVG renderer sources
    set(NANOVG_RENDERER_SOURCES
        nanovg_renderer.cpp
    )

    set(NANOVG_RENDERER_HEADERS
        nanovg_renderer.hpp
    )

    # Create the library
    add_library(nanovg_renderer STATIC ${NANOVG_RENDERER_SOURCES})

    # Include directories
    target_include_directories(nanovg_renderer PRIVATE
        ${NANOVG_INCLUDE_DIR}
        ${CMAKE_CURRENT_SOURCE_DIR}
        ../../Graphics/Renderer
        ../../Graphics/Fonts
        ../../Graphics/Pictures
        ../../System/Classes
        ../../System/Misc
        ../../Kernel/Types
        ../../Data/String
        ../../Data/Tree
    )

    # Find and link OpenGL
    find_package(OpenGL REQUIRED)
    target_link_libraries(nanovg_renderer ${OPENGL_LIBRARIES})

    # Platform-specific OpenGL linking
    if(APPLE)
        target_link_libraries(nanovg_renderer "-framework OpenGL")
    elseif(UNIX)
        target_link_libraries(nanovg_renderer GL)
    elseif(WIN32)
        target_link_libraries(nanovg_renderer opengl32)
    endif()

    # Compiler definitions
    target_compile_definitions(nanovg_renderer PRIVATE
        USE_NANOVG=1
    )

    # Set C++ standard
    set_target_properties(nanovg_renderer PROPERTIES
        CXX_STANDARD 17
        CXX_STANDARD_REQUIRED ON
    )

    # Installation
    install(TARGETS nanovg_renderer
        ARCHIVE DESTINATION lib
        LIBRARY DESTINATION lib
        RUNTIME DESTINATION bin
    )

    install(FILES ${NANOVG_RENDERER_HEADERS}
        DESTINATION include/Plugins/NanoVG
    )

else()
    message(STATUS "NanoVG not found - skipping NanoVG renderer")
endif()