#include "../generated/river-input-management.h"
#include "../generated/river-layer-shell.h"
#include "../generated/river-libinput-config.h"
#include "../generated/river-window-management.h"
#include "../generated/river-xkb-bindings.h"
#include "../generated/river-xkb-config.h"
#include <stdio.h>
#include <string.h>
#include <wayland-client-core.h>
#include <wayland-client-protocol.h>

static struct wl_compositor *compositor = NULL;
static struct wl_seat *seat = NULL;
static struct river_window_manager_v1 *river = NULL;
static struct river_xkb_bindings_v1 *xkb_binding = NULL;
static struct river_layer_shell_v1 *layer_shell = NULL;
static struct river_xkb_config_v1 *xkb_config = NULL;
static struct river_input_manager_v1 *input_manager = NULL;
static struct river_libinput_config_v1 *libinput_config = NULL;

static void registry_global(void *data, struct wl_registry *registry,
                            uint32_t name, const char *interface,
                            uint32_t version) {
  // printf("Interface: %s\n", interface);
  if (strcmp(interface, wl_compositor_interface.name) == 0) {
    compositor = wl_registry_bind(registry, name, &wl_compositor_interface, 6);
    printf("Bound wl_compositor!\n");
  }

  if (strcmp(interface, wl_seat_interface.name) == 0) {
    seat = wl_registry_bind(registry, name, &wl_seat_interface, 10);
    printf("Bound wl_seat!\n");
  }

  if (strcmp(interface, river_window_manager_v1_interface.name) == 0) {
    river =
        wl_registry_bind(registry, name, &river_window_manager_v1_interface, 4);
    printf("Bound river_wm!\n");
  }

  if (strcmp(interface, river_xkb_bindings_v1_interface.name) == 0) {
    xkb_binding =
        wl_registry_bind(registry, name, &river_xkb_bindings_v1_interface, 2);
    printf("Bound river_xkb_binding!\n");
  }

  if (strcmp(interface, river_layer_shell_v1_interface.name) == 0) {
    layer_shell =
        wl_registry_bind(registry, name, &river_layer_shell_v1_interface, 1);
    printf("Bound river_layer_shell!\n");
  }

  if (strcmp(interface, river_input_manager_v1_interface.name) == 0) {
    input_manager =
        wl_registry_bind(registry, name, &river_input_manager_v1_interface, 1);
    printf("Bound river_input_management!\n");
  }

  if (strcmp(interface, river_libinput_config_v1_interface.name) == 0) {
    libinput_config = wl_registry_bind(registry, name,
                                       &river_libinput_config_v1_interface, 1);
    printf("Bound river_libinput_config!\n");
  }

  if (strcmp(interface, river_xkb_config_v1_interface.name) == 0) {
    xkb_config =
        wl_registry_bind(registry, name, &river_xkb_config_v1_interface, 1);
    printf("Bound river_xkb_config!\n");
  }
}
static void registry_global_remove(void *data, struct wl_registry *registry,
                                   uint32_t version) {}

static const struct wl_registry_listener registry_listener = {
    .global = registry_global, .global_remove = registry_global_remove};

const struct wl_registry_listener *get_registry_listener(void) {
  return &registry_listener;
}

struct wl_compositor *get_compositor(void) { return compositor; }
struct wl_seat *get_seat(void) { return seat; }
struct river_window_manager_v1 *get_river(void) { return river; }
struct river_xkb_bindings_v1 *get_xkb_bindings(void) { return xkb_binding; }
struct river_layer_shell_v1 *get_layer_shell(void) { return layer_shell; }
struct river_xkb_config_v1 *get_xkb_config(void) { return xkb_config; }
struct river_input_manager_v1 *get_input_manager(void) { return input_manager; }
struct river_libinput_config_v1 *get_libinput_config(void) {
  return libinput_config;
}
