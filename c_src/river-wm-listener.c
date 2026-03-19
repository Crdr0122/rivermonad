#include "../generated/river-window-management.h"
#include <HsFFI.h>
#include <stdio.h>
#include <wayland-client-core.h>

extern void hs_wm_window(void *data, struct river_window_manager_v1 *wm,
                         struct river_window_v1 *window);
extern void hs_wm_output(void *data, struct river_window_manager_v1 *wm,
                         struct river_output_v1 *output);
extern void hs_wm_seat(void *data, struct river_window_manager_v1 *wm,
                       struct river_seat_v1 *seat);
extern void hs_wm_manage_start(void *data, struct river_window_manager_v1 *wm);
extern void hs_wm_render_start(void *data, struct river_window_manager_v1 *wm);

static void handle_unavailable(void *data, struct river_window_manager_v1 *wm) {
  printf("River WM unavailable (another WM running)\n");
}

static void handle_finished(void *data, struct river_window_manager_v1 *wm) {
  river_window_manager_v1_destroy(wm);
}

extern void hs_wm_session_locked(void *data,
                                 struct river_window_manager_v1 *wm);

extern void hs_wm_session_unlocked(void *data,
                                   struct river_window_manager_v1 *wm);

static const struct river_window_manager_v1_listener wm_listener = {
    .unavailable = handle_unavailable,
    .finished = handle_finished,
    .output = hs_wm_output,
    .seat = hs_wm_seat,
    .window = hs_wm_window,
    .manage_start = hs_wm_manage_start,
    .render_start = hs_wm_render_start,
    .session_locked = hs_wm_session_locked,
    .session_unlocked = hs_wm_session_unlocked,
};

const struct river_window_manager_v1_listener *get_river_wm_listener(void) {
  return &wm_listener;
}
