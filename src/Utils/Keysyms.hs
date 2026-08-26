{-# LANGUAGE PatternSynonyms #-}
{-# OPTIONS_GHC -Wno-missing-pattern-synonym-signatures #-}

module Utils.Keysyms where

import Data.Bits (Bits, (.|.))
import Foreign.C

newtype Keysym = Keysym {unKeySym :: CUInt}
  deriving stock (Eq, Ord, Show, Read)
  deriving newtype (Num, Integral, Real, Enum, Bits)

newtype KeyMod = KeyMod {unKeyMod :: CUInt}
  deriving stock (Eq, Ord, Show, Read)
  deriving newtype (Num, Integral, Real, Enum, Bits)

newtype PointerBtn = PointerBtn {unPointerBtn :: CUInt}
  deriving stock (Eq, Ord, Show, Read)
  deriving newtype (Num, Integral, Real, Enum, Bits)

-- Keysyms
pattern KeyA = Keysym 0x0061
pattern KeyB = Keysym 0x0062
pattern KeyC = Keysym 0x0063
pattern KeyD = Keysym 0x0064
pattern KeyE = Keysym 0x0065
pattern KeyF = Keysym 0x0066
pattern KeyG = Keysym 0x0067
pattern KeyH = Keysym 0x0068
pattern KeyI = Keysym 0x0069
pattern KeyJ = Keysym 0x006a
pattern KeyK = Keysym 0x006b
pattern KeyL = Keysym 0x006c
pattern KeyM = Keysym 0x006d
pattern KeyN = Keysym 0x006e
pattern KeyO = Keysym 0x006f
pattern KeyP = Keysym 0x0070
pattern KeyQ = Keysym 0x0071
pattern KeyR = Keysym 0x0072
pattern KeyS = Keysym 0x0073
pattern KeyT = Keysym 0x0074
pattern KeyU = Keysym 0x0075
pattern KeyV = Keysym 0x0076
pattern KeyW = Keysym 0x0077
pattern KeyX = Keysym 0x0078
pattern KeyY = Keysym 0x0079
pattern KeyZ = Keysym 0x007a

pattern Key0 = Keysym 0x0030
pattern Key1 = Keysym 0x0031
pattern Key2 = Keysym 0x0032
pattern Key3 = Keysym 0x0033
pattern Key4 = Keysym 0x0034
pattern Key5 = Keysym 0x0035
pattern Key6 = Keysym 0x0036
pattern Key7 = Keysym 0x0037
pattern Key8 = Keysym 0x0038
pattern Key9 = Keysym 0x0039

pattern KeyKP0 = Keysym 0xffb0
pattern KeyKP1 = Keysym 0xffb1
pattern KeyKP2 = Keysym 0xffb2
pattern KeyKP3 = Keysym 0xffb3
pattern KeyKP4 = Keysym 0xffb4
pattern KeyKP5 = Keysym 0xffb5
pattern KeyKP6 = Keysym 0xffb6
pattern KeyKP7 = Keysym 0xffb7
pattern KeyKP8 = Keysym 0xffb8
pattern KeyKP9 = Keysym 0xffb9

pattern KeyKPSpace = Keysym 0xff80
pattern KeyKPTab = Keysym 0xff89
pattern KeyKPEnter = Keysym 0xff8d
pattern KeyKPF1 = Keysym 0xff91
pattern KeyKPF2 = Keysym 0xff92
pattern KeyKPF3 = Keysym 0xff93
pattern KeyKPF4 = Keysym 0xff94
pattern KeyKPHome = Keysym 0xff95
pattern KeyKPLeft = Keysym 0xff96
pattern KeyKPUp = Keysym 0xff97
pattern KeyKPRight = Keysym 0xff98
pattern KeyKPDown = Keysym 0xff99
pattern KeyKPPrior = Keysym 0xff9a
pattern KeyKPPageUp = Keysym 0xff9a
pattern KeyKPNext = Keysym 0xff9b
pattern KeyKPPageDown = Keysym 0xff9b
pattern KeyKPEnd = Keysym 0xff9c
pattern KeyKPBegin = Keysym 0xff9d
pattern KeyKPInsert = Keysym 0xff9e
pattern KeyKPDelete = Keysym 0xff9f
pattern KeyKPEqual = Keysym 0xffbd
pattern KeyKPMultiply = Keysym 0xffaa
pattern KeyKPAdd = Keysym 0xffab
pattern KeyKPSeparator = Keysym 0xffac
pattern KeyKPSubtract = Keysym 0xffad
pattern KeyKPDecimal = Keysym 0xffae
pattern KeyKPDivide = Keysym 0xffaf

pattern KeyHome = Keysym 0xff50
pattern KeyLeft = Keysym 0xff51
pattern KeyUp = Keysym 0xff52
pattern KeyRight = Keysym 0xff53
pattern KeyDown = Keysym 0xff54
pattern KeyPrior = Keysym 0xff55
pattern KeyPageUp = Keysym 0xff55
pattern KeyNext = Keysym 0xff56
pattern KeyPageDown = Keysym 0xff56
pattern KeyEnd = Keysym 0xff57
pattern KeyBegin = Keysym 0xff58

pattern KeyEnter = Keysym 0xFF0D

pattern KeySpace = Keysym 0x0020

pattern KeyTab = Keysym 0xFF09

pattern KeyMinus = Keysym 0x002d

pattern KeyEqual = Keysym 0x003d

pattern KeyGrave = Keysym 0x0060

pattern KeyDelete = Keysym 0xff9f

pattern KeyEscape = Keysym 0xff1b

pattern KeyXF86Calculator = Keysym 0x1008FF1D

pattern KeyXF86Standby = Keysym 0x1008FF10
pattern KeyXF86AudioLowerVolume = Keysym 0x1008FF11
pattern KeyXF86AudioMute = Keysym 0x1008FF12
pattern KeyXF86AudioRaiseVolume = Keysym 0x1008FF13
pattern KeyXF86AudioPlay = Keysym 0x1008FF14
pattern KeyXF86AudioStop = Keysym 0x1008FF15
pattern KeyXF86AudioPrev = Keysym 0x1008FF16
pattern KeyXF86AudioNext = Keysym 0x1008FF1
pattern KeyXF86MonBrightnessUp = Keysym 0x1008FF02
pattern KeyXF86MonBrightnessDown = Keysym 0x1008FF03
pattern KeyXF86KbdLightOnOff = Keysym 0x1008FF04
pattern KeyXF86KbdBrightnessUp = Keysym 0x1008FF05
pattern KeyXF86KbdBrightnessDown = Keysym 0x1008FF06
pattern KeyXF86AudioMicMute = Keysym 0x1008FFB2

-- Pointer
pattern BtnLeft = PointerBtn 0x110
pattern BtnRight = PointerBtn 0x111
pattern BtnMiddle = PointerBtn 0x112
pattern BtnSide = PointerBtn 0x113
pattern BtnExtra = PointerBtn 0x114

-- Modifiers
pattern ModNone = KeyMod 0x00

pattern ModShift = KeyMod 0x01

pattern ModControl = KeyMod 0x04

pattern ModAlt = KeyMod 0x08

pattern ModSuper = KeyMod 0x40

-- modSuper = 0x08

modSuperShift :: KeyMod
modSuperShift = ModSuper .|. ModShift

modSuperAlt :: KeyMod
modSuperAlt = ModSuper .|. ModAlt
