#include "river-wm.h"
#include <stdio.h>
#include <string.h>
#include <wayland-client-core.h>
#include <wayland-client-protocol.h>

static struct wl_compositor *compositor = NULL;
static struct river_wm *river = NULL;
static struct river_output *output = NULL;

static void registry_global(void *data, struct wl_registry *registry,
                            uint32_t name, const char *interface,
                            uint32_t version) {
  printf("global: %s\n", interface);
  if (strcmp(interface, wl_compositor_interface.name) == 0) {
    compositor = wl_registry_bind(registry, name, &wl_compositor_interface, 4);
    printf("Bound wl_compositor!\n");
  }

  if (strcmp(interface, river_window_manager_v1_interface.name) == 0) {
    river =
        wl_registry_bind(registry, name, &river_window_manager_v1_interface, 1);
    printf("Bound river_wm!\n");
  }

  if (strcmp(interface, river_output_v1_interface.name) == 0) {
    output = wl_registry_bind(registry, name, &river_output_v1_interface, 1);
    printf("Bound river_output!\n");
  }
}

static const struct wl_registry_listener registry_listener = {
    .global = registry_global, .global_remove = NULL};

const struct wl_registry_listener *get_registry_listener(void) {
  return &registry_listener;
}

struct wl_compositor *get_compositor(void) { return compositor; }
struct river_wm *get_river(void) { return river; }
struct river_output *get_output(void) { return output; }
