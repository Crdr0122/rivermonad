module Types where

import Control.Concurrent.MVar
import Data.Map
import Foreign

data WMState = WMState
  { manageQueue :: MVar (IO ())
  , renderQueue :: MVar (IO ())
  , allWindows :: MVar (Map (Ptr RiverWindow) Window)
  , allOutputs :: MVar (Map (Ptr RiverOutput) Output)
  , focusedOutput :: MVar (Ptr RiverOutput)
  }

data WlDisplay
data WlRegistry
data WlProxy
data RiverNode
data RiverWindow
data RiverOutput
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
