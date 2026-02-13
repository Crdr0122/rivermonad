#include "river-wm.h"
#include <stdio.h>
#include <wayland-client.h>

/* Haskell callback */
extern void hs_output_dimensions(void *data, struct river_output_v1 *output,
                                 int32_t width, int32_t height);
extern void hs_output_position(void *data, struct river_output_v1 *output,
                               int32_t x, int32_t y);
extern void hs_output_removed(void *data, struct river_output_v1 *output);
extern void hs_output_wl_output(void *data, struct river_output_v1 *output,
                                uint32_t name);

static const struct river_output_v1_listener output_listener = {
    .removed = hs_output_removed,
    .position = hs_output_position,
    .dimensions = hs_output_dimensions,
    .wl_output = hs_output_wl_output,
};

const struct river_output_v1_listener *get_river_output_listener(void) {
  return &output_listener;
}
