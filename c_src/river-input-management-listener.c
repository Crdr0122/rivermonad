#include "../generated/river-input-management.h"
#include <wayland-client-core.h>

extern void
hs_input_manager_input_device(void *data,
                              struct river_input_manager_v1 *manager,
                              struct river_input_device_v1 *device);
extern void hs_input_manager_finished(void *data,
                                      struct river_input_manager_v1 *manager);
static const struct river_input_manager_v1_listener
    river_input_manager_listener = {
        .input_device = hs_input_manager_input_device,
        .finished = hs_input_manager_finished,
};
extern void hs_input_device_name(void *data,
                                 struct river_input_device_v1 *device,
                                 const char *name);
extern void hs_input_device_removed(void *data,
                                    struct river_input_device_v1 *device);
extern void hs_input_device_type(void *data,
                                 struct river_input_device_v1 *device,
                                 uint32_t type);
extern void hs_input_device_done(void *data,
                                 struct river_input_device_v1 *device);

static const struct river_input_device_v1_listener river_input_device_listener =
    {
        .name = hs_input_device_name,
        .removed = hs_input_device_removed,
        .type = hs_input_device_type,
        .done = hs_input_device_done,
};

const struct river_input_manager_v1_listener *
get_river_input_manager_listener(void) {
  return &river_input_manager_listener;
}
const struct river_input_device_v1_listener *
get_river_input_device_listener(void) {
  return &river_input_device_listener;
}
