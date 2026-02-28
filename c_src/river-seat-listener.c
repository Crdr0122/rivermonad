#include "../generated/river-wm.h"
#include <stdio.h>
#include <wayland-client.h>

/* Haskell callback */

extern void hs_pointer_enter(void *data, struct river_seat_v1 *seat,
                             struct river_window_v1 *window);
extern void hs_window_interaction(void *data, struct river_seat_v1 *seat,
                                  struct river_window_v1 *window);
extern void hs_op_delta(void *data, struct river_seat_v1 *seat, int32_t dx,
                        int32_t dy);
extern void hs_op_release(void *data, struct river_seat_v1 *seat);

extern void hs_pointer_position(void *data, struct river_seat_v1 *seat,
                                int32_t x, int32_t y);

static void handle_removed(void *data, struct river_seat_v1 *seat) {
  river_seat_v1_destroy(seat);
}
static void handle_wl_seat(void *data, struct river_seat_v1 *seat,
                           uint32_t name) {}

static void handle_pointer_leave(void *data, struct river_seat_v1 *seat) {}

static void
handle_shell_surface_interaction(void *data, struct river_seat_v1 *seat,
                                 struct river_shell_surface_v1 *shell_surface) {
}

static const struct river_seat_v1_listener seat_listener = {
    .removed = handle_removed,
    .wl_seat = handle_wl_seat,
    .pointer_enter = hs_pointer_enter,
    .pointer_leave = handle_pointer_leave,
    .window_interaction = hs_window_interaction,
    .shell_surface_interaction = handle_shell_surface_interaction,
    .op_delta = hs_op_delta,
    .op_release = hs_op_release,
    .pointer_position = hs_pointer_position,
};

const struct river_seat_v1_listener *get_river_seat_listener(void) {
  return &seat_listener;
}
