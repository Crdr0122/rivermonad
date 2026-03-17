#include "../generated/river-libinput-config.h"
#include <HsFFI.h>
#include <wayland-client-core.h>

static void handle_finished(void *data,
                            struct river_libinput_config_v1 *config) {
  river_libinput_config_v1_destroy(config);
}

static void handle_libinput_device(void *data,
                                   struct river_libinput_config_v1 *cfg,
                                   struct river_libinput_device_v1 *device) {}

static const struct river_libinput_config_v1_listener libinput_config_listener =
    {
        .finished = handle_finished,
        .libinput_device = handle_libinput_device,
};

static void handle_input_device(void *data,
                                struct river_libinput_device_v1 *device,
                                struct river_input_device_v1 *dev) {}

static void handle_device_removed(void *data,
                                  struct river_libinput_device_v1 *device) {
  river_libinput_device_v1_destroy(device);
}
static void handle_send_events_support(
    void *data, struct river_libinput_device_v1 *river_libinput_device_v1,
    uint32_t modes) {}
static void handle_send_events_default(
    void *data, struct river_libinput_device_v1 *river_libinput_device_v1,
    uint32_t mode) {}
static void handle_send_events_current(
    void *data, struct river_libinput_device_v1 *river_libinput_device_v1,
    uint32_t mode) {}
static void
handle_tap_support(void *data,
                   struct river_libinput_device_v1 *river_libinput_device_v1,
                   int32_t finger_count) {}
static void
handle_tap_default(void *data,
                   struct river_libinput_device_v1 *river_libinput_device_v1,
                   uint32_t state) {}
static void
handle_tap_current(void *data,
                   struct river_libinput_device_v1 *river_libinput_device_v1,
                   uint32_t state) {}
static void handle_tap_button_map_default(
    void *data, struct river_libinput_device_v1 *river_libinput_device_v1,
    uint32_t button_map) {}
static void handle_tap_button_map_current(
    void *data, struct river_libinput_device_v1 *river_libinput_device_v1,
    uint32_t button_map) {}
static void
handle_drag_default(void *data,
                    struct river_libinput_device_v1 *river_libinput_device_v1,
                    uint32_t state) {}
static void
handle_drag_current(void *data,
                    struct river_libinput_device_v1 *river_libinput_device_v1,
                    uint32_t state) {}
static void handle_drag_lock_default(
    void *data, struct river_libinput_device_v1 *river_libinput_device_v1,
    uint32_t state) {}
static void handle_drag_lock_current(
    void *data, struct river_libinput_device_v1 *river_libinput_device_v1,
    uint32_t state) {}
static void handle_three_finger_drag_support(
    void *data, struct river_libinput_device_v1 *river_libinput_device_v1,
    int32_t finger_count) {}
static void handle_three_finger_drag_default(
    void *data, struct river_libinput_device_v1 *river_libinput_device_v1,
    uint32_t state) {}
static void handle_three_finger_drag_current(
    void *data, struct river_libinput_device_v1 *river_libinput_device_v1,
    uint32_t state) {}
static void handle_calibration_matrix_support(
    void *data, struct river_libinput_device_v1 *river_libinput_device_v1,
    int32_t supported) {}
static void handle_calibration_matrix_default(
    void *data, struct river_libinput_device_v1 *river_libinput_device_v1,
    struct wl_array *matrix) {}
static void handle_calibration_matrix_current(
    void *data, struct river_libinput_device_v1 *river_libinput_device_v1,
    struct wl_array *matrix) {}
static void handle_accel_profiles_support(
    void *data, struct river_libinput_device_v1 *river_libinput_device_v1,
    uint32_t profiles) {}
static void handle_accel_profile_default(
    void *data, struct river_libinput_device_v1 *river_libinput_device_v1,
    uint32_t profile) {}
static void handle_accel_profile_current(
    void *data, struct river_libinput_device_v1 *river_libinput_device_v1,
    uint32_t profile) {}
static void handle_accel_speed_default(
    void *data, struct river_libinput_device_v1 *river_libinput_device_v1,
    struct wl_array *speed) {}
static void handle_accel_speed_current(
    void *data, struct river_libinput_device_v1 *river_libinput_device_v1,
    struct wl_array *speed) {}
static void handle_natural_scroll_support(
    void *data, struct river_libinput_device_v1 *river_libinput_device_v1,
    int32_t supported) {}
static void handle_natural_scroll_default(
    void *data, struct river_libinput_device_v1 *river_libinput_device_v1,
    uint32_t state) {}
static void handle_natural_scroll_current(
    void *data, struct river_libinput_device_v1 *river_libinput_device_v1,
    uint32_t state) {}
static void handle_left_handed_support(
    void *data, struct river_libinput_device_v1 *river_libinput_device_v1,
    int32_t supported) {}
static void handle_left_handed_default(
    void *data, struct river_libinput_device_v1 *river_libinput_device_v1,
    uint32_t state) {}
static void handle_left_handed_current(
    void *data, struct river_libinput_device_v1 *river_libinput_device_v1,
    uint32_t state) {}
static void handle_click_method_support(
    void *data, struct river_libinput_device_v1 *river_libinput_device_v1,
    uint32_t methods) {}
static void handle_click_method_default(
    void *data, struct river_libinput_device_v1 *river_libinput_device_v1,
    uint32_t method) {}
static void handle_click_method_current(
    void *data, struct river_libinput_device_v1 *river_libinput_device_v1,
    uint32_t method) {}
static void handle_clickfinger_button_map_default(
    void *data, struct river_libinput_device_v1 *river_libinput_device_v1,
    uint32_t button_map) {}
static void handle_clickfinger_button_map_current(
    void *data, struct river_libinput_device_v1 *river_libinput_device_v1,
    uint32_t button_map) {}
static void handle_middle_emulation_support(
    void *data, struct river_libinput_device_v1 *river_libinput_device_v1,
    int32_t supported) {}
static void handle_middle_emulation_default(
    void *data, struct river_libinput_device_v1 *river_libinput_device_v1,
    uint32_t state) {}
static void handle_middle_emulation_current(
    void *data, struct river_libinput_device_v1 *river_libinput_device_v1,
    uint32_t state) {}
static void handle_scroll_method_support(
    void *data, struct river_libinput_device_v1 *river_libinput_device_v1,
    uint32_t methods) {}
static void handle_scroll_method_default(
    void *data, struct river_libinput_device_v1 *river_libinput_device_v1,
    uint32_t method) {}
static void handle_scroll_method_current(
    void *data, struct river_libinput_device_v1 *river_libinput_device_v1,
    uint32_t method) {}
static void handle_scroll_button_default(
    void *data, struct river_libinput_device_v1 *river_libinput_device_v1,
    uint32_t button) {}
static void handle_scroll_button_current(
    void *data, struct river_libinput_device_v1 *river_libinput_device_v1,
    uint32_t button) {}
static void handle_scroll_button_lock_default(
    void *data, struct river_libinput_device_v1 *river_libinput_device_v1,
    uint32_t state) {}
static void handle_scroll_button_lock_current(
    void *data, struct river_libinput_device_v1 *river_libinput_device_v1,
    uint32_t state) {}
static void
handle_dwt_support(void *data,
                   struct river_libinput_device_v1 *river_libinput_device_v1,
                   int32_t supported) {}
static void
handle_dwt_default(void *data,
                   struct river_libinput_device_v1 *river_libinput_device_v1,
                   uint32_t state) {}
static void
handle_dwt_current(void *data,
                   struct river_libinput_device_v1 *river_libinput_device_v1,
                   uint32_t state) {}
static void
handle_dwtp_support(void *data,
                    struct river_libinput_device_v1 *river_libinput_device_v1,
                    int32_t supported) {}
static void
handle_dwtp_default(void *data,
                    struct river_libinput_device_v1 *river_libinput_device_v1,
                    uint32_t state) {}
static void
handle_dwtp_current(void *data,
                    struct river_libinput_device_v1 *river_libinput_device_v1,
                    uint32_t state) {}
static void handle_rotation_support(
    void *data, struct river_libinput_device_v1 *river_libinput_device_v1,
    int32_t supported) {}
static void handle_rotation_default(
    void *data, struct river_libinput_device_v1 *river_libinput_device_v1,
    uint32_t angle) {}
static void handle_rotation_current(
    void *data, struct river_libinput_device_v1 *river_libinput_device_v1,
    uint32_t angle) {}

static const struct river_libinput_device_v1_listener libinput_device_listener =
    {
        .input_device = handle_input_device,
        .removed = handle_device_removed,
        .send_events_support = handle_send_events_support,
        .send_events_current = handle_send_events_current,
        .send_events_default = handle_send_events_default,
        .tap_support = handle_tap_support,
        .tap_default = handle_tap_default,
        .tap_current = handle_tap_current,
        .tap_button_map_current = handle_tap_button_map_current,
        .tap_button_map_default = handle_tap_button_map_default,
        .drag_current = handle_drag_current,
        .drag_default = handle_drag_default,
        .drag_lock_current = handle_drag_lock_current,
        .drag_lock_default = handle_drag_lock_default,
        .three_finger_drag_support = handle_three_finger_drag_support,
        .three_finger_drag_default = handle_three_finger_drag_default,
        .three_finger_drag_current = handle_three_finger_drag_current,
        .calibration_matrix_current = handle_calibration_matrix_current,
        .calibration_matrix_default = handle_calibration_matrix_default,
        .calibration_matrix_support = handle_calibration_matrix_support,
        .accel_profile_default = handle_accel_profile_default,
        .accel_profile_current = handle_accel_profile_current,
        .accel_profiles_support = handle_accel_profiles_support,
        .accel_speed_current = handle_accel_speed_current,
        .accel_speed_default = handle_accel_speed_default,
        .natural_scroll_current = handle_natural_scroll_current,
        .natural_scroll_support = handle_natural_scroll_support,
        .natural_scroll_default = handle_natural_scroll_default,
        .left_handed_default = handle_left_handed_default,
        .left_handed_current = handle_left_handed_current,
        .left_handed_support = handle_left_handed_support,
        .click_method_current = handle_click_method_current,
        .click_method_default = handle_click_method_default,
        .click_method_support = handle_click_method_support,
        .clickfinger_button_map_current = handle_clickfinger_button_map_current,
        .clickfinger_button_map_default = handle_clickfinger_button_map_default,
        .middle_emulation_current = handle_middle_emulation_current,
        .middle_emulation_support = handle_middle_emulation_support,
        .middle_emulation_default = handle_middle_emulation_default,
        .scroll_method_default = handle_scroll_method_default,
        .scroll_method_current = handle_scroll_method_current,
        .scroll_method_support = handle_scroll_method_support,
        .scroll_button_default = handle_scroll_button_default,
        .scroll_button_current = handle_scroll_button_current,
        .scroll_button_lock_current = handle_scroll_button_lock_current,
        .scroll_button_lock_default = handle_scroll_button_lock_default,
        .dwt_current = handle_dwt_current,
        .dwt_default = handle_dwt_default,
        .dwt_support = handle_dwt_support,
        .dwtp_default = handle_dwtp_default,
        .dwtp_support = handle_dwtp_support,
        .dwtp_current = handle_dwtp_current,
        .rotation_current = handle_rotation_current,
        .rotation_default = handle_rotation_default,
        .rotation_support = handle_rotation_support,
};

const struct river_libinput_config_v1_listener *
get_river_libinput_config_listener(void) {
  return &libinput_config_listener;
}
const struct river_libinput_device_v1_listener *
get_river_libinput_device_listener(void) {
  return &libinput_device_listener;
}
