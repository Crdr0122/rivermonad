#include "../generated/river-xkb-config.h"
#include <HsFFI.h>
#include <stdio.h>
#include <wayland-client-core.h>

extern void hs_xkb_config_finished(void *data, struct river_xkb_config_v1 *xkb);
extern void hs_xkb_config_xkb_keyboard(void *data,
                                       struct river_xkb_config_v1 *xkb,
                                       struct river_xkb_keyboard_v1 *keyboard);

static const struct river_xkb_config_v1_listener xkb_config_listener = {
    .finished = hs_xkb_config_finished,
    .xkb_keyboard = hs_xkb_config_xkb_keyboard,
};

static void capslock_disabled(void *data, struct river_xkb_keyboard_v1 *xkb) {}
static void capslock_enabled(void *data, struct river_xkb_keyboard_v1 *xkb) {}
static void numlock_disabled(void *data, struct river_xkb_keyboard_v1 *xkb) {}
static void numlock_enabled(void *data, struct river_xkb_keyboard_v1 *xkb) {}
static void removed(void *data, struct river_xkb_keyboard_v1 *xkb) {
  river_xkb_keyboard_v1_destroy(xkb);
}
static void input_device(void *data,
                         struct river_xkb_keyboard_v1 *river_xkb_keyboard_v1,
                         struct river_input_device_v1 *device) {}

static void layout(void *data,
                   struct river_xkb_keyboard_v1 *river_xkb_keyboard_v1,
                   uint32_t index, const char *name) {}

static const struct river_xkb_keyboard_v1_listener xkb_keyboard_listener = {
    .capslock_disabled = capslock_disabled,
    .capslock_enabled = capslock_enabled,
    .numlock_disabled = numlock_disabled,
    .numlock_enabled = numlock_enabled,
    .removed = removed,
    .input_device = input_device,
    .layout = layout,
};

const struct river_xkb_config_v1_listener *get_river_xkb_config_listener(void) {
  return &xkb_config_listener;
}
const struct river_xkb_keyboard_v1_listener *
get_river_xkb_keyboard_listener(void) {
  return &xkb_keyboard_listener;
}
