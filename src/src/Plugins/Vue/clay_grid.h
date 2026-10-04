//
//  clay_grid.h
//
//  Created by Massimiliano Gubinelli on 01/09/2025.
//  Copyright © 2025 TeXmacs.org. All rights reserved.
//

// grid layout from @keldonalleyne in Clay's discord server
// 2/7/25

#ifdef __cplusplus
extern "C" {
#endif

/* Grids can be nested. */
#define GRID(GRID__ColumnCount)                                                                                        \
GridState gridState;                                                                                               \
for (int GRID__LATCH             = (gridState = GridComponent((GridState) {.columnCount = (GRID__ColumnCount)}), 0); \
GRID__LATCH < 1; GRID__LATCH = 1, (gridState.elementCount = gridState.index, GridComponent(gridState)))

#define GRID_ELEMENT()                                                                 \
for (int GRID__LATCH = (gridState = GridComponent(gridState), 0); GRID__LATCH < 1; \
GRID__LATCH = 1, gridState = GridComponent(gridState))

/* All function state is captured here. */
typedef struct GridState {
  int entryPoint;
  int columnCount;
  int index;
  int elementCount;
  int row;
  int col;
} GridState;

GridState GridComponent(GridState coState);


#ifdef __cplusplus
}
#endif

#if 0 // example
void SomeComponent() {
    GRID(2 /* Two columns */ ) {
        GRID_ELEMENT() {
            GRID(1 /* One column */) {
                GRID_ELEMENT() {
                    // Do your clay stuff here.
                }
                GRID_ELEMENT() {
                }
            }
        }
        GRID_ELEMENT() {
        }
        GRID_ELEMENT() {
        }
    }
}
#endif
