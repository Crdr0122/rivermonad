#include "../generated/river-layer-shell.h"
#include "../generated/river-wm.h"
#include "../generated/river-xkb-binding.h"
#include <stdio.h>
#include <string.h>
#include <wayland-client-core.h>
#include <wayland-client-protocol.h>

static struct wl_compositor *compositor = NULL;
static struct river_wm *river = NULL;
static struct river_output *output = NULL;
static struct river_xkb_bindings *xkb_binding = NULL;
static struct river_layer_shell *layer_shell = NULL;

static void registry_global(void *data, struct wl_registry *registry,
                            uint32_t name, const char *interface,
                            uint32_t version) {
  // printf("Interface: %s\n", interface);
  if (strcmp(interface, wl_compositor_interface.name) == 0) {
    compositor = wl_registry_bind(registry, name, &wl_compositor_interface, 4);
    printf("Bound wl_compositor!\n");
  }

  if (strcmp(interface, river_window_manager_v1_interface.name) == 0) {
    river =
        wl_registry_bind(registry, name, &river_window_manager_v1_interface, 3);
    printf("Bound river_wm!\n");
  }

  if (strcmp(interface, river_output_v1_interface.name) == 0) {
    output = wl_registry_bind(registry, name, &river_output_v1_interface, 1);
    printf("Bound river_output!\n");
  }

  if (strcmp(interface, river_xkb_bindings_v1_interface.name) == 0) {
    xkb_binding =
        wl_registry_bind(registry, name, &river_xkb_bindings_v1_interface, 1);
    printf("Bound river_xkb_binding!\n");
  }

  if (strcmp(interface, river_layer_shell_v1_interface.name) == 0) {
    layer_shell =
        wl_registry_bind(registry, name, &river_layer_shell_v1_interface, 1);
    printf("Bound river_layer_shell!\n");
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
struct river_wm *get_river(void) { return river; }
struct river_output *get_output(void) { return output; }
struct river_xkb_bindings *get_xkb_bindings(void) { return xkb_binding; }
struct river_layer_shell *get_layer_shell(void) { return layer_shell; }
