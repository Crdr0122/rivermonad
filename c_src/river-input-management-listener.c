#include "../generated/river-input-management.h"
#include <HsFFI.h>
#include <wayland-client-core.h>

static void handle_input_device(void *data,
                                struct river_input_manager_v1 *manager,
                                struct river_input_device_v1 *device) {}
static void handle_manager_finished(void *data,
                                    struct river_input_manager_v1 *manager) {
  river_input_manager_v1_destroy(manager);
}
static const struct river_input_manager_v1_listener
    river_input_manager_listener = {
        .input_device = handle_input_device,
        .finished = handle_manager_finished,
};
static void handle_name(void *data, struct river_input_device_v1 *device,
                        const char *name) {}
static void handle_removed(void *data, struct river_input_device_v1 *device) {

  river_input_device_v1_destroy(device);
}
static void handle_type(void *data, struct river_input_device_v1 *device,
                        uint32_t type) {}
static const struct river_input_device_v1_listener river_input_device_listener =
    {
        .name = handle_name,
        .removed = handle_removed,
        .type = handle_type,
};

const struct river_input_manager_v1_listener *
get_river_input_manager_listener(void) {
  return &river_input_manager_listener;
}
const struct river_input_device_v1_listener *
get_river_input_device_listener(void) {
  return &river_input_device_listener;
}
