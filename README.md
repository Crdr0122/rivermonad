Haskell window manager for the river compositor

Uses FFI to libwayland for wayland communication

Everything will be hardcoded for personal use

Current protocols: River 0.4.7

# TODO
- wl_fixed_t into haskell type, currently placeholder, needed for scale, cursor stuff
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
      - [X] Focused workspace on each output
- [X] Window rules
  - [X] Open floating  
  - [X] Open fullscreen (?)
  - [X] Open on workspace
  - [X] Open with set size
- [-] Ipc
  - [ ] Receive ipc keydispatches
  - [X] Send out workspace information for bars
  - [ ] Copy swayipc
- [X] Window pinning (Only floating)
- [X] Directional window focus and swapping
- [X] Xkb Keymap (Needs manually writing keymap string but works)
- [ ] Ext-workspace (wait for river)
- [X] Cursor shape
- [X] Multimonitor focus other outputs
  - [ ] Use pointer_enter_output event rather than calculating windows when river publishes the event 
- [ ] Layout alter geometry rather than only changing ratio
- [ ] Hotkey repetition
- [ ] Libinput
- [-] Multiseats (Not needed?)
- [X] Overview (Grid view?)
- [X] New floating windows offset to not exactly block previous window
- [ ] Xkb bindings seat -> needed for key eating and modifier watch
