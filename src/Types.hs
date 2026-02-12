module Types where

import Data.IORef
import Data.Map.Strict
import Foreign
import Foreign.C
import Utils.BiMap

data Rect = Rect {rx, ry, rw, rh :: Int} deriving (Show)
type WorkspaceID = Int
type WMStateRef = IORef WMState
data WMState = WMState
  { manageQueue :: IO ()
  , renderQueue :: IO ()
  , allWindows :: Map (Ptr RiverWindow) Window
  , allOutputs :: Map (Ptr RiverOutput) Output
  , focusedWindow :: Maybe (Ptr RiverWindow)
  , focusedOutput :: Ptr RiverOutput
  , focusedWorkspace :: WorkspaceID
  , focusedSeat :: Ptr RiverSeat
  , workspaceLayouts :: Map WorkspaceID LayoutType
  , allWorkspaces :: BiMap WorkspaceID (Ptr RiverWindow)
  , currentXkbBindings :: Ptr RiverXkbBindings
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
data RiverXkbBinding
data RiverXkbBindings
type XkbCallback = Ptr () -> Ptr RiverXkbBinding -> IO ()


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

data KeyConfig = KeyConfig
  { keysym :: CUInt
  , mods :: CUInt
  , action :: WMState -> IO ()
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
