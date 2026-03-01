#include "../generated/river-wm.h"
#include <stdio.h>

extern void hs_window_closed(void *data, struct river_window_v1 *window);
extern void hs_window_dimensions(void *data, struct river_window_v1 *window,
                                 int32_t width, int32_t height);
extern void hs_window_parent(void *data, struct river_window_v1 *window,
                             struct river_window_v1 *parent);
extern void hs_window_dimensions_hint(void *data,
                                      struct river_window_v1 *window,
                                      int32_t min_width, int32_t min_height,
                                      int32_t max_width, int32_t max_height);

static void handle_title(void *data, struct river_window_v1 *window,
                         const char *title) {}

static void handle_app_id(void *data, struct river_window_v1 *window,
                          const char *app_id) {}

static void handle_decoration_hint(void *data, struct river_window_v1 *window,
                                   uint32_t hint) {}

static void handle_pointer_move_requested(void *data,
                                          struct river_window_v1 *window,
                                          struct river_seat_v1 *seat) {}

static void handle_pointer_resize_requested(void *data,
                                            struct river_window_v1 *window,
                                            struct river_seat_v1 *seat,
                                            uint32_t edges) {}

static void handle_show_window_menu_requested(void *data,
                                              struct river_window_v1 *window,
                                              int32_t x, int32_t y) {}

static void handle_maximize_requested(void *data,
                                      struct river_window_v1 *window) {}

static void handle_unmaximize_requested(void *data,
                                        struct river_window_v1 *window) {}

static void handle_fullscreen_requested(void *data,
                                        struct river_window_v1 *window,
                                        struct river_output_v1 *output) {}

static void handle_exit_fullscreen_requested(void *data,
                                             struct river_window_v1 *window) {}

static void handle_minimize_requested(void *data,
                                      struct river_window_v1 *window) {}

static void handle_unreliable_pid(void *data, struct river_window_v1 *window,
                                  int32_t pid) {}

static const struct river_window_v1_listener window_listener = {
    .closed = hs_window_closed,
    .dimensions_hint = hs_window_dimensions_hint,
    .dimensions = hs_window_dimensions,
    .title = handle_title,
    .app_id = handle_app_id,
    .parent = hs_window_parent,
    .decoration_hint = handle_decoration_hint,
    .pointer_move_requested = handle_pointer_move_requested,
    .pointer_resize_requested = handle_pointer_resize_requested,
    .show_window_menu_requested = handle_show_window_menu_requested,
    .maximize_requested = handle_maximize_requested,
    .unmaximize_requested = handle_unmaximize_requested,
    .fullscreen_requested = handle_fullscreen_requested,
    .exit_fullscreen_requested = handle_exit_fullscreen_requested,
    .minimize_requested = handle_minimize_requested,
    .unreliable_pid = handle_unreliable_pid,
};

const struct river_window_v1_listener *get_river_window_listener(void) {
  return &window_listener;
}
