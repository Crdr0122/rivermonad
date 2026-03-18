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
  - [X] Magnifier
  - [X] Workspace individual layouts
  - [X] Adding deleting master windows
  - [ ] Combine layouts
- [X] Floating, resize, dragging
  - [X] Floating resizing
  - [X] Tiling resizing
- [X] Recompile and restart wm with state saving
  - [X] Recompiling and restarting
  - [X] Saving state
      - [X] Window workspaces
- [-] Window rules
  - [X] Open floating  
  - [X] Open fullscreen (?)
  - [X] Open on workspace
  - [ ] Open with set size
- [ ] Ipc
  - [ ] Receive ipc keydispatches
  - [ ] Send out workspace information for bars
  - [ ] Copy swayipc
- [X] Window pinning (Only floating)
- [X] Directional window focus and swapping
- [X] Xkb Keymap (Needs manually writing keymap string but works)
- [ ] Ext-workspace (wait for river)
- [ ] Cursor shape (wait for river)
- [X] Multimonitor focus other outputs
- [ ] Layout alter geometry rather than only changing ratio
- [ ] Hotkey repetition
- [ ] Libinput
- [ ] Multiseats (Not needed?)
- [ ] Overview (Grid view?)
