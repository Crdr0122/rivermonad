#include "river-wm.h"
#include <stdio.h>
#include <wayland-client.h>

/* Haskell callback */
extern void hs_output_dimensions(void *data, struct river_output_v1 *output,
                                 int32_t width, int32_t height);
extern void hs_output_position(void *data, struct river_output_v1 *output,
                               int32_t x, int32_t y);

static void handle_removed(void *data, struct river_output_v1 *output) {
  printf("Output removed\n");
  river_output_v1_destroy(output);
}

// static void handle_dimensions(void *data, struct river_output_v1 *output,
//                               int32_t width, int32_t height) {
//   printf("Output dimensions %ux%u\n", width, height);
//   hs_output_dimensions(data, output, width, height);
// }

// static void handle_position(void *data, struct river_output_v1 *output,
//                             int32_t x, int32_t y) {
//   printf("Output position %d %d\n", x, y);
// }

static void handle_wl_output(void *data, struct river_output_v1 *output,
                             uint32_t name) {
  printf("Output name %u\n", name);
}

static const struct river_output_v1_listener output_listener = {
    .removed = handle_removed,
    .position = hs_output_position,
    .dimensions = hs_output_dimensions,
    .wl_output = handle_wl_output,
};

const struct river_output_v1_listener *get_river_output_listener(void) {
  return &output_listener;
}
