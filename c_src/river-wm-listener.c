#include "../generated/river-window-management.h"
#include <HsFFI.h>
#include <stdio.h>
#include <wayland-client-core.h>

extern void hs_on_new_window(void *data, struct river_window_manager_v1 *wm,
                             struct river_window_v1 *window);
extern void hs_on_new_output(void *data, struct river_window_manager_v1 *wm,
                             struct river_output_v1 *output);
extern void hs_on_new_seat(void *data, struct river_window_manager_v1 *wm,
                           struct river_seat_v1 *seat);
extern void hs_wm_manage_start(void *data, struct river_window_manager_v1 *wm);
extern void hs_wm_render_start(void *data, struct river_window_manager_v1 *wm);
extern const struct river_window_listener *get_river_window_listener(void);
extern const struct river_output_listener *get_river_output_listener(void);

static void handle_unavailable(void *data, struct river_window_manager_v1 *wm) {
  printf("River WM unavailable (another WM running)\n");
}

static void handle_finished(void *data, struct river_window_manager_v1 *wm) {
  river_window_manager_v1_destroy(wm);
}

static void handle_session_locked(void *data,
                                  struct river_window_manager_v1 *wm) {}

static void handle_session_unlocked(void *data,
                                    struct river_window_manager_v1 *wm) {}

static const struct river_window_manager_v1_listener wm_listener = {
    .unavailable = handle_unavailable,
    .finished = handle_finished,
    .output = hs_on_new_output,
    .seat = hs_on_new_seat,
    .window = hs_on_new_window,
    .manage_start = hs_wm_manage_start,
    .render_start = hs_wm_render_start,
    .session_locked = handle_session_locked,
    .session_unlocked = handle_session_unlocked,
};

const struct river_window_manager_v1_listener *get_river_wm_listener(void) {
  return &wm_listener;
}
