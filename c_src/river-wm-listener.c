#include "river-wm.h"
#include <HsFFI.h>
#include <stdio.h>
#include <wayland-client-core.h>

extern void hs_on_new_window(void *window);
extern void hs_on_new_output(void *output);
extern void hs_manage_start(struct river_window_manager_v1 *wm);
extern const struct river_window_listener *get_river_window_listener(void);
extern const struct river_output_listener *get_river_output_listener(void);

static void handle_unavailable(void *data, struct river_window_manager_v1 *wm) {
  printf("River WM unavailable (another WM running)\n");
}

static void handle_output(void *data, struct river_window_manager_v1 *wm,
                          struct river_output_v1 *output) {
  hs_on_new_output(output);
}

static void handle_seat(void *data, struct river_window_manager_v1 *wm,
                        struct river_seat_v1 *seat) {
  printf("River: new seat\n");
}

static void handle_manage_start(void *data,
                                struct river_window_manager_v1 *wm) {
  printf("River: manage start\n");
  hs_manage_start(wm);
}

static void handle_render_start(void *data,
                                struct river_window_manager_v1 *wm) {
  printf("River: render start\n");
}

static void handle_session_locked(void *data,
                                  struct river_window_manager_v1 *wm) {}

static void handle_session_unlocked(void *data,
                                    struct river_window_manager_v1 *wm) {}

static void handle_window(void *data, struct river_window_manager_v1 *wm,
                          struct river_window_v1 *window) {
  hs_on_new_window(window);
}

static const struct river_window_manager_v1_listener wm_listener = {
    .unavailable = handle_unavailable,
    .output = handle_output,
    .seat = handle_seat,
    .window = handle_window,
    .manage_start = handle_manage_start,
    .render_start = handle_manage_start,
    .session_locked = handle_session_locked,
    .session_unlocked = handle_session_unlocked,
};

const struct river_window_manager_v1_listener *get_river_wm_listener(void) {
  return &wm_listener;
}
