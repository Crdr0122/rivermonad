#include "river-wm.h"
#include "river-xkb-binding.h"
#include <stdio.h>
#include <wayland-client.h>

/* Haskell callback */

static void handle_removed(void *data, struct river_seat_v1 *seat) {
  river_seat_v1_destroy(seat);
}
static const struct river_xkb_binding_v1_listener bindings_listener = {};

const struct river_xkb_binding_v1_listener *get_xkb_binding_listener(void) {
  return &bindings_listener;
}
