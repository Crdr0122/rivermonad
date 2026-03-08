Haskell window manager for the river compositor

Uses FFI to libwayland for wayland communication

Everything will be hardcoded for personal use

# TODO
- [-] Layouts 
  - [X] Stack
  - [X] Twopane
  - [X] Monocle
  - [X] Circle
  - [X] Roledex
  - [X] Ifmax of two layouts
  - [ ] Magnifier
  - [X] Workspace individual layouts
  - [ ] Combine layouts
  - [ ] Adding deleting master windows (I dont need)
- [X] Floating, resize, dragging
  - [X] Floating resizing
  - [X] Tiling resizing
- [X] Recompile and restart wm with state saving
  - [X] Recompiling and restarting
  - [X] Saving state
      - [X] Window workspaces
      - [X] Workspace layout ratios
- [-] Window rules
  - [X] Open floating  
  - [X] Open fullscreen (?)
  - [ ] Open on workspace
  - [ ] Open with set size
- [ ] Ipc
  - [ ] Receive ipc keydispatches
  - [ ] Send out workspace information for bars
- [ ] Multimonitor focus other outputs
- [ ] Window pinning (Only floating)
- [ ] Directional window focus and swapping (Only tiling)
- [ ] Layout alter geometry rather than only changing ratio
- [ ] Hotkey repetition
- [ ] Libinput
- [ ] Multiseats (Not needed?)
