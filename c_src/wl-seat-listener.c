#include <wayland-client.h>

extern void hs_wl_seat_capabilities(void *data, struct wl_seat *seat,
                                    uint32_t capabilities);

extern void hs_wl_seat_name(void *data, struct wl_seat *seat, const char *name);

static const struct wl_seat_listener seat_listener = {
    .capabilities = hs_wl_seat_capabilities,
    .name = hs_wl_seat_name,
};
const struct wl_seat_listener *get_wl_seat_listener(void) {
  return &seat_listener;
}
extern void hs_wl_pointer_enter(void *data, struct wl_pointer *pointer,
                                uint32_t serial, struct wl_surface *,
                                wl_fixed_t, wl_fixed_t);
static void handle_leave(void *data, struct wl_pointer *pointer,
                         uint32_t serial, struct wl_surface *surface) {}

static void handle_motion(void *data, struct wl_pointer *pointer,
                          uint32_t serial, wl_fixed_t surface_x,
                          wl_fixed_t surface_y) {}
static void handle_button(void *data, struct wl_pointer *pointer,
                          uint32_t serial, uint32_t time, uint32_t button,
                          uint32_t state) {}
static void handle_axis(void *data, struct wl_pointer *pointer, uint32_t time,
                        uint32_t axis, wl_fixed_t value) {}
static void handle_frame(void *data, struct wl_pointer *pointer) {}
static void handle_axis_source(void *data, struct wl_pointer *pointer,
                               uint32_t source) {}
static void handle_axis_discrete(void *data, struct wl_pointer *pointer,
                                 uint32_t axis, int32_t steps) {}
static void handle_axis_relative_direction(void *data,
                                           struct wl_pointer *pointer,
                                           uint32_t axis, uint32_t steps) {}
static void handle_axis_stop(void *data, struct wl_pointer *pointer,
                             uint32_t time, uint32_t axis) {}
static void handle_axis_value120(void *data, struct wl_pointer *pointer,
                                 uint32_t axis, int32_t steps) {}

static const struct wl_pointer_listener pointer_listener = {
    .enter = hs_wl_pointer_enter,
    .leave = handle_leave,
    .motion = handle_motion,
    .button = handle_button,
    .axis = handle_axis,
    .frame = handle_frame,
    .axis_discrete = handle_axis_discrete,
    .axis_relative_direction = handle_axis_relative_direction,
    .axis_source = handle_axis_source,
    .axis_stop = handle_axis_stop,
    .axis_value120 = handle_axis_value120,
};
const struct wl_pointer_listener *get_wl_pointer_listener(void) {
  return &pointer_listener;
}
