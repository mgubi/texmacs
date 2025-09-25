/******************************************************************************
* MODULE     : nanovg_demo.cpp
* DESCRIPTION: Demo/example usage of NanoVG renderer
* COPYRIGHT  : (C) 2024
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "nanovg_renderer.hpp"
#include "pencil.hpp"
#include "brush.hpp"
#include <iostream>

/******************************************************************************
* Demo function showing basic usage
******************************************************************************/

void demo_nanovg_renderer() {
    // Create a NanoVG renderer with default flags
    renderer ren = nanovg_renderer(NANOVG_DEFAULT_FLAGS, 800, 600);

    if (!ren || !ren->is_started()) {
        std::cerr << "Failed to create NanoVG renderer" << std::endl;
        return;
    }

    nanovg_renderer_rep* nvg_ren = (nanovg_renderer_rep*)ren;

    // Begin frame
    nvg_ren->begin_frame();

    // Set up graphics state
    ren->set_pencil(pencil(rgb_color(255, 0, 0), 2 * PIXEL));  // Red pen, 2px width
    ren->set_background(brush(rgb_color(255, 255, 255)));       // White background

    // Clear the surface
    ren->clear_device(0, 0, 800 * PIXEL, 600 * PIXEL);

    // Draw some basic shapes

    // Rectangle
    ren->fill(100 * PIXEL, 100 * PIXEL, 200 * PIXEL, 150 * PIXEL);

    // Line
    ren->line(50 * PIXEL, 50 * PIXEL, 250 * PIXEL, 200 * PIXEL);

    // Triangle
    ren->draw_triangle(300 * PIXEL, 100 * PIXEL,
                      350 * PIXEL, 200 * PIXEL,
                      250 * PIXEL, 200 * PIXEL);

    // Polygon (pentagon)
    array<SI> x_coords, y_coords;
    SI center_x = 500 * PIXEL, center_y = 150 * PIXEL, radius = 50 * PIXEL;
    for (int i = 0; i < 5; i++) {
        double angle = i * 2 * M_PI / 5;
        x_coords << center_x + (SI)(radius * cos(angle));
        y_coords << center_y + (SI)(radius * sin(angle));
    }
    ren->polygon(x_coords, y_coords);

    // Arc
    ren->arc(400 * PIXEL, 250 * PIXEL, 500 * PIXEL, 350 * PIXEL, 0, 90 * 64);

    // Connected lines
    array<SI> line_x, line_y;
    line_x << 100 * PIXEL << 150 * PIXEL << 200 * PIXEL << 180 * PIXEL << 120 * PIXEL;
    line_y << 300 * PIXEL << 280 * PIXEL << 320 * PIXEL << 350 * PIXEL << 340 * PIXEL;
    ren->lines(line_x, line_y);

    // End frame
    nvg_ren->end_frame();

    // Clean up
    tm_delete(ren);

    std::cout << "NanoVG renderer demo completed successfully!" << std::endl;
}

/******************************************************************************
* Integration example with existing TeXmacs renderer system
******************************************************************************/

void demo_renderer_integration() {
    // Example of how to integrate with TeXmacs renderer selection
    nanovg_renderer_rep* nvg_ren = the_nanovg_renderer();

    if (nvg_ren && nvg_ren->is_started()) {
        std::cout << "Global NanoVG renderer is available" << std::endl;

        // Set frame size for high-DPI display
        nvg_ren->set_frame_size(1920, 1080, 2.0f);  // 2x pixel ratio for retina

        // Enable zoom
        nvg_ren->set_zoom_factor(1.5);

        // Demonstrate clipping
        nvg_ren->begin_frame();
        nvg_ren->set_clipping(100 * PIXEL, 100 * PIXEL,
                             400 * PIXEL, 300 * PIXEL);

        // Drawing will be clipped to this region
        nvg_ren->set_pencil(pencil(rgb_color(0, 255, 0), PIXEL));
        nvg_ren->fill(0, 0, 800 * PIXEL, 600 * PIXEL);  // Only visible part will show

        nvg_ren->end_frame();
    }
}

/******************************************************************************
* Shadow rendering example
******************************************************************************/

void demo_shadow_rendering() {
    renderer main_ren = nanovg_renderer(NANOVG_DEFAULT_FLAGS, 800, 600);
    if (!main_ren || !main_ren->is_started()) {
        std::cerr << "Failed to create main renderer" << std::endl;
        return;
    }

    nanovg_renderer_rep* nvg_ren = (nanovg_renderer_rep*)main_ren;
    nvg_ren->begin_frame();

    // Set clipping region for shadow
    nvg_ren->set_clipping(100 * PIXEL, 100 * PIXEL,
                         300 * PIXEL, 200 * PIXEL);

    // Create shadow renderer
    renderer shadow_ren;
    nvg_ren->new_shadow(shadow_ren);

    if (shadow_ren) {
        // Render something to the shadow
        shadow_ren->set_pencil(pencil(rgb_color(128, 128, 255), PIXEL));
        shadow_ren->fill(0, 0, 200 * PIXEL, 100 * PIXEL);

        // Copy shadow back to main renderer
        nvg_ren->put_shadow(shadow_ren, 150 * PIXEL, 150 * PIXEL,
                           350 * PIXEL, 250 * PIXEL);

        // Clean up shadow
        nvg_ren->delete_shadow(shadow_ren);
    }

    nvg_ren->end_frame();
    tm_delete(main_ren);

    std::cout << "Shadow rendering demo completed" << std::endl;
}

#ifdef COMPILE_DEMO
/******************************************************************************
* Main function for standalone demo
******************************************************************************/

int main() {
    std::cout << "NanoVG Renderer Demo" << std::endl;
    std::cout << "===================" << std::endl;

    demo_nanovg_renderer();
    demo_renderer_integration();
    demo_shadow_rendering();

    return 0;
}
#endif // COMPILE_DEMO