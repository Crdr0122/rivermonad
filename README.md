Haskell window manager for the river compositor

Uses FFI to libwayland for wayland communication

Everything will be hardcoded for personal use

# TODO
- [X] Layouts 
  - [X] Workspace individual layouts
  - [ ] Magnifier layout
  - [ ] Combine layouts
- [X] Floating, resize, dragging
- [ ] Ipc
  - [ ] Receive ipc keydispatches
  - [ ] Send out workspace information for bars
- [ ] Multimonitor don't crash
- [ ] Window pinning (Only floating)
- [ ] Directional window focus and swapping (Only tiling)
- [ ] Layout alter geometry rather than only changing ratio
- [ ] Hotkey repetition
- [ ] Libinput
- [ ] Recompile and restart wm with state saving
  - [ ] Recompiling and restarting
  - [ ] Saving state
