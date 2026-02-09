module Types where

import Data.IORef
import Data.Map
import Data.Sequence as S
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
  , focusedWorkspace :: Int
  , currentSeat :: Ptr RiverSeat
  , allWorkspaces :: Map Int Workspace
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

data WlSurface

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

data Workspace = Workspace
  { workspaceNum :: Int
  , tileOrder :: Seq (Ptr RiverWindow) -- Ordering specific to this tag
  , layoutType :: LayoutType -- e.g., Stack, Monocle, Floating
  }

data LayoutType = Monocle | Stack | Floating | Deck

newtype RiverEdge = RiverEdge Int
  deriving (Eq, Show, Bits)

edgeNone, edgeTop, edgeBottom, edgeLeft, edgeRight :: RiverEdge
edgeNone = RiverEdge 0
edgeTop = RiverEdge 1
edgeBottom = RiverEdge 2
edgeLeft = RiverEdge 4
edgeRight = RiverEdge 8
