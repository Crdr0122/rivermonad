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

static const struct wl_pointer_listener pointer_listener = {
    .enter = hs_wl_pointer_enter,
};
const struct wl_pointer_listener *get_wl_pointer_listener(void) {
  return &pointer_listener;
}
