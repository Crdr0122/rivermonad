#include "../generated/river-libinput-config.h"
#include <HsFFI.h>
#include <wayland-client-core.h>

static void handle_finished(void *data,
                            struct river_libinput_config_v1 *config) {
  river_libinput_config_v1_destroy(config);
}

static void handle_device(void *data, struct river_libinput_config_v1 *cfg,
                          struct river_libinput_device_v1 *device) {}

static const struct river_libinput_config_v1_listener libinput_config_listener =
    {
        .finished = handle_finished,
        .libinput_device = handle_device,
};

const struct river_libinput_config_v1_listener *
get_libinput_config_listener(void) {
  return &libinput_config_listener;
}
