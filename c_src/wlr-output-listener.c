#include "../generated/wlr-output-management-unstable.h"
#include <wayland-client-core.h>

extern void hs_wlr_output_manager_head(void *data,
                                       struct zwlr_output_manager_v1 *manager,
                                       struct zwlr_output_head_v1 *head);
extern void hs_wlr_output_manager_done(void *data,
                                       struct zwlr_output_manager_v1 *manager,
                                       uint32_t serial);
extern void
hs_wlr_output_manager_finished(void *data,
                               struct zwlr_output_manager_v1 *manager);
static const struct zwlr_output_manager_v1_listener
    zwlr_output_manager_listener = {
        .head = hs_wlr_output_manager_head,
        .done = hs_wlr_output_manager_done,
        .finished = hs_wlr_output_manager_finished,

};

static void hs_wlr_output_head_name(void *data,
                                    struct zwlr_output_head_v1 *head,
                                    const char *name) {}
static void hs_wlr_output_head_description(void *data,
                                           struct zwlr_output_head_v1 *head,
                                           const char *description) {}
static void hs_wlr_output_head_physical_size(void *data,
                                             struct zwlr_output_head_v1 *head,
                                             int32_t width, int32_t height) {}
static void hs_wlr_output_head_mode(void *data,
                                    struct zwlr_output_head_v1 *head,
                                    struct zwlr_output_mode_v1 *mode) {}
static void hs_wlr_output_head_enabled(void *data,
                                       struct zwlr_output_head_v1 *head,
                                       int32_t enabled) {}
static void
hs_wlr_output_head_current_mode(void *data, struct zwlr_output_head_v1 *head,
                                struct zwlr_output_mode_v1 *current_mode) {}
static void hs_wlr_output_head_position(void *data,
                                        struct zwlr_output_head_v1 *head,
                                        int32_t x, int32_t y) {}
static void hs_wlr_output_head_transform(void *data,
                                         struct zwlr_output_head_v1 *head,
                                         int32_t transform) {}
static void hs_wlr_output_head_scale(void *data,
                                     struct zwlr_output_head_v1 *head,
                                     wl_fixed_t scale) {}
static void hs_wlr_output_head_finished(void *data,
                                        struct zwlr_output_head_v1 *head) {}
static void hs_wlr_output_head_make(void *data,
                                    struct zwlr_output_head_v1 *head,
                                    const char *make) {}
static void hs_wlr_output_head_model(void *data,
                                     struct zwlr_output_head_v1 *head,
                                     const char *model) {}
static void hs_wlr_output_head_serial_number(void *data,
                                             struct zwlr_output_head_v1 *head,
                                             const char *serial_number) {}
static void hs_wlr_output_head_adaptive_sync(void *data,
                                             struct zwlr_output_head_v1 *head,
                                             uint32_t adaptive_sync) {}

// extern void hs_wlr_output_head_name(void *data,
//                                     struct zwlr_output_head_v1 *head,
//                                     const char *name);
// extern void hs_wlr_output_head_description(void *data,
//                                            struct zwlr_output_head_v1 *head,
//                                            const char *description);
// extern void hs_wlr_output_head_physical_size(void *data,
//                                              struct zwlr_output_head_v1
//                                              *head, int32_t width, int32_t
//                                              height);
// extern void hs_wlr_output_head_mode(void *data,
//                                     struct zwlr_output_head_v1 *head,
//                                     struct zwlr_output_mode_v1 *mode);
// extern void hs_wlr_output_head_enabled(void *data,
//                                        struct zwlr_output_head_v1 *head,
//                                        int32_t enabled);
// extern void
// hs_wlr_output_head_current_mode(void *data, struct zwlr_output_head_v1 *head,
//                                 struct zwlr_output_mode_v1 *current_mode);
// extern void hs_wlr_output_head_position(void *data,
//                                         struct zwlr_output_head_v1 *head,
//                                         int32_t x, int32_t y);
// extern void hs_wlr_output_head_transform(void *data,
//                                          struct zwlr_output_head_v1 *head,
//                                          int32_t transform);
// extern void hs_wlr_output_head_scale(void *data,
//                                      struct zwlr_output_head_v1 *head,
//                                      wl_fixed_t scale);
// extern void hs_wlr_output_head_finished(void *data,
//                                         struct zwlr_output_head_v1 *head);
// extern void hs_wlr_output_head_make(void *data,
//                                     struct zwlr_output_head_v1 *head,
//                                     const char *make);
// extern void hs_wlr_output_head_model(void *data,
//                                      struct zwlr_output_head_v1 *head,
//                                      const char *model);
// extern void hs_wlr_output_head_serial_number(void *data,
//                                              struct zwlr_output_head_v1
//                                              *head, const char
//                                              *serial_number);
// extern void hs_wlr_output_head_adaptive_sync(void *data,
//                                              struct zwlr_output_head_v1
//                                              *head, uint32_t adaptive_sync);
//
static const struct zwlr_output_head_v1_listener zwlr_output_head_listener = {
    .name = hs_wlr_output_head_name,
    .description = hs_wlr_output_head_description,
    .physical_size = hs_wlr_output_head_physical_size,
    .mode = hs_wlr_output_head_mode,
    .enabled = hs_wlr_output_head_enabled,
    .current_mode = hs_wlr_output_head_current_mode,
    .position = hs_wlr_output_head_position,
    .transform = hs_wlr_output_head_transform,
    .scale = hs_wlr_output_head_scale,
    .finished = hs_wlr_output_head_finished,
    .make = hs_wlr_output_head_make,
    .model = hs_wlr_output_head_model,
    .serial_number = hs_wlr_output_head_serial_number,
    .adaptive_sync = hs_wlr_output_head_adaptive_sync,
};

static void hs_wlr_output_mode_size(void *data,
                                    struct zwlr_output_mode_v1 *mode,
                                    int32_t width, int32_t height) {}
static void hs_wlr_output_mode_refresh(void *data,
                                       struct zwlr_output_mode_v1 *mode,
                                       int32_t refersh) {}
static void hs_wlr_output_mode_preferred(void *data,
                                         struct zwlr_output_mode_v1 *mode) {}
static void hs_wlr_output_mode_finished(void *data,
                                        struct zwlr_output_mode_v1 *mode) {}

static const struct zwlr_output_mode_v1_listener zwlr_output_mode_listener = {
    .size = hs_wlr_output_mode_size,
    .refresh = hs_wlr_output_mode_refresh,
    .preferred = hs_wlr_output_mode_preferred,
    .finished = hs_wlr_output_mode_finished,
};

static void hs_wlr_output_configuration_succeeded(
    void *data, struct zwlr_output_configuration_v1 *config) {}
static void hs_wlr_output_configuration_failed(
    void *data, struct zwlr_output_configuration_v1 *config) {}
static void hs_wlr_output_configuration_cancelled(
    void *data, struct zwlr_output_configuration_v1 *config) {}

static const struct zwlr_output_configuration_v1_listener
    zwlr_output_configuration_listener = {
        .succeeded = hs_wlr_output_configuration_succeeded,
        .failed = hs_wlr_output_configuration_failed,
        .cancelled = hs_wlr_output_configuration_cancelled,
};

const struct zwlr_output_manager_v1_listener *
get_wlr_output_manager_listener(void) {
  return &zwlr_output_manager_listener;
}
const struct zwlr_output_head_v1_listener *get_wlr_output_head_listener(void) {
  return &zwlr_output_head_listener;
}
const struct zwlr_output_mode_v1_listener *get_wlr_output_mode_listener(void) {
  return &zwlr_output_mode_listener;
}
const struct zwlr_output_configuration_v1_listener *
get_wlr_output_configuration_listener(void) {
  return &zwlr_output_configuration_listener;
}
