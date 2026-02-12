module Utils.Keysyms where

import Foreign.C

-- Keysyms
keyQ :: CUInt
keyQ = 0x0071

keyEnter :: CUInt
keyEnter = 0xFF0D

-- Modifiers
modNone :: CUInt
modNone = 0

modShift :: CUInt
modShift = 0x01

modControl :: CUInt
modControl = 0x04

modAlt :: CUInt
modAlt = 0x08

modSuper :: CUInt
modSuper = 0x40
