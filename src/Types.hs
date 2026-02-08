module Types where

import Data.IORef
import Data.Map
import Foreign

data Rect = Rect {rx, ry, rw, rh :: Int} deriving (Show)
type WMStateRef = IORef WMState
data WMState = WMState
  { manageQueue :: IO ()
  , renderQueue :: IO ()
  , allWindows :: Map (Ptr RiverWindow) Window
  , allOutputs :: Map (Ptr RiverOutput) Output
  , focusedWindow :: Ptr RiverWindow
  , focusedOutput :: Ptr RiverOutput
  , currentSeat :: Ptr RiverSeat
  }

data WlDisplay
data WlRegistry
data WlProxy
data RiverNode
data RiverWindow
data RiverOutput
data RiverSeat
data RiverShellSurface
data RiverPointer
data RiverWMManager

data Window = Window
  { winPtr :: Ptr RiverWindow
  , nodePtr :: Ptr RiverNode
  , isFloating :: Bool
  , isFullscreen :: Bool
  }
  deriving (Eq)

data Output = Output
  { outPtr :: Ptr RiverOutput
  , outWidth :: Int
  , outHeight :: Int
  , outX :: Int
  , outY :: Int
  }
  deriving (Eq)

newtype RiverEdge = RiverEdge Int
  deriving (Eq, Show, Bits)

edgeNone, edgeTop, edgeBottom, edgeLeft, edgeRight :: RiverEdge
edgeNone = RiverEdge 0
edgeTop = RiverEdge 1
edgeBottom = RiverEdge 2
edgeLeft = RiverEdge 4
edgeRight = RiverEdge 8
