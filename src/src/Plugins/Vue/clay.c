#include <SDL3/SDL.h>
//#include <SDL3_ttf/SDL_ttf.h>

#define CLAY_IMPLEMENTATION
#include "clay.h"
#include "clay_grid.h"

#include <stdio.h>  // snprintf, for vue_clay_capacity_report

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
            CLAY(CLAY_IDI_LOCAL("Row", coState.row), {
                /* Some other row config stuff */
            }) {
                for (coState.col = 0; coState.col < coState.columnCount && coState.index < coState.elementCount;
                    coState.col++, coState.index++) {
                    CLAY(CLAY_IDI_LOCAL("Col", coState.col), {
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

// How full are the internal arrays of the current Clay context? Clay
// reports an array which has run out of room and a genuine out of bounds
// read with the same error (CLAY_ERROR_TYPE_INTERNAL_ERROR, "out of bounds
// array access"), so these counts are what tells the two apart: an array at
// its capacity is the first, none at capacity is the second. Writes at most
// n bytes and returns the number of arrays which are full.
int
vue_clay_capacity_report (char* buf, int n) {
  Clay_Context* ctx= Clay_GetCurrentContext ();
  int full= 0, off= 0;
  if (buf == NULL || n <= 0) return 0;
  buf[0]= 0;
  if (ctx == NULL) { snprintf (buf, n, "no current Clay context"); return 0; }
#define VUE_CLAY_ARRAY(field, name)                                     \
  do {                                                                  \
    int len= (int) ctx->field.length, cap= (int) ctx->field.capacity;    \
    if (len >= cap) {                                                   \
      full++;                                                           \
      if (off < n)                                                      \
        off += snprintf (buf + off, n - off, "%sFULL %s %d/%d",          \
                         off > 0 ? ", " : "", name, len, cap);           \
    }                                                                   \
  } while (0)
  VUE_CLAY_ARRAY (layoutElements, "elements");
  VUE_CLAY_ARRAY (renderCommands, "render commands");
  VUE_CLAY_ARRAY (layoutElementChildren, "element children");
  VUE_CLAY_ARRAY (layoutElementChildrenBuffer, "children buffer");
  VUE_CLAY_ARRAY (layoutElementsHashMapInternal, "element hash map");
  VUE_CLAY_ARRAY (layoutElementIdStrings, "id strings");
  VUE_CLAY_ARRAY (measureTextHashMapInternal, "text cache");
  VUE_CLAY_ARRAY (measuredWords, "measured words");
  VUE_CLAY_ARRAY (wrappedTextLines, "wrapped text lines");
  VUE_CLAY_ARRAY (scrollContainerDatas, "scroll containers");
  VUE_CLAY_ARRAY (transitionDatas, "transitions");
  VUE_CLAY_ARRAY (openLayoutElementStack, "open element stack");
  VUE_CLAY_ARRAY (openClipElementStack, "open clip stack");
  VUE_CLAY_ARRAY (layoutElementTreeRoots, "tree roots");
  VUE_CLAY_ARRAY (layoutElementTreeNodeArray1, "tree nodes");
  VUE_CLAY_ARRAY (pointerOverIds, "pointer over ids");
  VUE_CLAY_ARRAY (dynamicStringData, "dynamic strings");
#undef VUE_CLAY_ARRAY
  if (off < n)
    snprintf (buf + off, n - off,
              "%selements %d/%d, render commands %d/%d, frame %u",
              off > 0 ? "; " : "",
              (int) ctx->layoutElements.length, (int) ctx->layoutElements.capacity,
              (int) ctx->renderCommands.length, (int) ctx->renderCommands.capacity,
              (unsigned) ctx->generation);
  return full;
}

// does the current Clay context have a transition in progress? (the
// context structure is only visible here, where Clay is implemented)
bool vue_clay_transitions_active (void) {
  Clay_Context* ctx= Clay_GetCurrentContext ();
  if (ctx == NULL) return false;
  for (int32_t i= 0; i < ctx->transitionDatas.length; i++)
    if (Clay__TransitionDataInternalArray_Get (&ctx->transitionDatas, i)->state != CLAY_TRANSITION_STATE_IDLE)
      return true;
  return false;
}
