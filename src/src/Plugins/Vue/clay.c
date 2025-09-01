#include <SDL3/SDL.h>
//#include <SDL3_ttf/SDL_ttf.h>

#define CLAY_IMPLEMENTATION
#include "clay.h"
#include "clay_grid.h"

//#include <stdio.h>

#include "clay_renderer_SDL3.c"
#include <assert.h>
#include <limits.h>

// grid layout from @keldonalleyne in Clay's discord server
// 2/7/25

/* Switch based coroutines (you cannot use CO_YIELD inside a nested switch) */
#define CO_INIT()       switch (coState.entryPoint)
#define CO_ENTRYPOINT() case 0:
#define CO_ASSERT_UNREACHABLE() \
    default:                    \
        assert(false);          \
        return coState;
/* You must only have one CO_YIELD() per line (so you can't use multiple CO_YIELD() in a macro) */
#define CO_YIELD() CO_YIELD_IDX(__LINE__)
#define CO_YIELD_IDX(idx)     \
    coState.entryPoint = idx; \
    return coState;           \
    case idx:
#define CO_END() return coState;


GridState
GridComponent (GridState coState) {
    CO_INIT() {
        CO_ENTRYPOINT();
        // Element count is not known at initialization. It will be set after child components are rendered/processed.
        // The Grid #define sets state.elementCount to state.index.
        coState.elementCount = INT_MAX;

        // Initial call yields here.
        CO_YIELD();

        for (coState.row = 0; coState.row < (coState.elementCount / coState.columnCount); coState.row++) {
            CLAY({
                .id = CLAY_IDI_LOCAL("Row", coState.row),
                /* Some other row config stuff */
            }) {
                for (coState.col = 0; coState.col < coState.columnCount && coState.index < coState.elementCount;
                    coState.col++, coState.index++) {
                    CLAY({
                        .id = CLAY_IDI_LOCAL("Col", coState.col),
                        /* Some other column config stuff */
                    }) {
                        CO_YIELD(); // Resume to body.
                        CO_YIELD(); // Body has completed.
                    }
                }
            }
        }
        CO_END();
        CO_ASSERT_UNREACHABLE();
    }
}
