module Utils.Keysyms where

import Foreign.C

-- Keysyms
keyA :: CUInt
keyB :: CUInt
keyC :: CUInt
keyD :: CUInt
keyE :: CUInt
keyF :: CUInt
keyG :: CUInt
keyH :: CUInt
keyI :: CUInt
keyJ :: CUInt
keyK :: CUInt
keyL :: CUInt
keyM :: CUInt
keyN :: CUInt
keyO :: CUInt
keyP :: CUInt
keyQ :: CUInt
keyR :: CUInt
keyS :: CUInt
keyT :: CUInt
keyU :: CUInt
keyV :: CUInt
keyW :: CUInt
keyX :: CUInt
keyY :: CUInt
keyZ :: CUInt
keyA = 0x0061
keyB = 0x0062
keyC = 0x0063
keyD = 0x0064
keyE = 0x0065
keyF = 0x0066
keyG = 0x0067
keyH = 0x0068
keyI = 0x0069
keyJ = 0x006a
keyK = 0x006b
keyL = 0x006c
keyM = 0x006d
keyN = 0x006e
keyO = 0x006f
keyP = 0x0070
keyQ = 0x0071
keyR = 0x0072
keyS = 0x0073
keyT = 0x0074
keyU = 0x0075
keyV = 0x0076
keyW = 0x0077
keyX = 0x0078
keyY = 0x0079
keyZ = 0x007a

key0 :: CUInt
key1 :: CUInt
key2 :: CUInt
key3 :: CUInt
key4 :: CUInt
key5 :: CUInt
key6 :: CUInt
key7 :: CUInt
key8 :: CUInt
key9 :: CUInt
key0 = 0x0030
key1 = 0x0031
key2 = 0x0032
key3 = 0x0033
key4 = 0x0034
key5 = 0x0035
key6 = 0x0036
key7 = 0x0037
key8 = 0x0038
key9 = 0x0039

keyEnter :: CUInt
keyEnter = 0xFF0D

keySpace :: CUInt
keySpace = 0x0020

keyTab :: CUInt
keyTab = 0xFF09

keyMinus :: CUInt
keyMinus = 0x002d

keyEqual :: CUInt
keyEqual = 0x003d

-- Pointer
btnLeft, btnRight, btnMiddle, btnSide, btnExtra :: CUInt
btnLeft = 0x110
btnRight = 0x111
btnMiddle = 0x112
btnSide = 0x113
btnExtra = 0x114

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
-- modSuper = 0x40
modSuper = 0x08
