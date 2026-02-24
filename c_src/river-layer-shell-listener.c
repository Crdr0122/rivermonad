#include "../generated/river-layer-shell.h"
#include <stdio.h>
#include <wayland-client.h>

/* Haskell callback */
extern void hs_layer_shell_output_non_exclusive_area(
    void *data, struct river_layer_shell_output_v1 *output, int32_t x,
    int32_t y, int32_t width, int32_t height);

static const struct river_layer_shell_output_v1_listener
    layer_shell_output_listener = {
        .non_exclusive_area = hs_layer_shell_output_non_exclusive_area};

const struct river_layer_shell_output_v1_listener *
get_river_layer_shell_output_listener(void) {
  return &layer_shell_output_listener;
}
