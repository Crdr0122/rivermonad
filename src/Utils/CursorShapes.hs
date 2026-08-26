module Utils.CursorShapes where

import Foreign.C

cursorToCUInt :: CursorShape -> CUInt
cursorToCUInt = fromIntegral . (+ 1) . fromEnum -- The protocol enums start from 1

data CursorShape
  = CursorDefault
  | CursorContextMenu
  | CursorHelp
  | CursorPointer
  | CursorProgress
  | CursorWait
  | CursorCell
  | CursorCrosshair
  | CursorText
  | CursorVerticalText
  | CursorAlias
  | CursorCopy
  | CursorMove
  | CursorNoDrop
  | CursorNotAllowed
  | CursorGrab
  | CursorGrabbing
  | CursorEResize
  | CursorNResize
  | CursorNeResize
  | CursorNwResize
  | CursorSResize
  | CursorSeResize
  | CursorSwResize
  | CursorWResize
  | CursorEwResize
  | CursorNsResize
  | CursorNeswResize
  | CursorNwseResize
  | CursorColResize
  | CursorRowResize
  | CursorAllScroll
  | CursorZoomIn
  | CursorZoomOut
  | CursorDndAsk
  | CursorAllResize
  deriving (Eq, Ord, Show, Read, Enum)
