module Types where

import Data.IORef
import Data.Map.Strict
import Foreign
import Foreign.C
import Utils.BiSeqMap

data Rect = Rect {rx, ry, rw, rh :: CInt} deriving (Show, Eq)
type WorkspaceID = Int
type WMStateRef = IORef WMState
data WMState = WMState
  { manageQueue :: IO ()
  , renderQueue :: IO ()
  , allWindows :: Map (Ptr RiverWindow) Window
  , allOutputs :: Map (Ptr RiverOutput) Output
  , allLayerShellOutputs :: Map (Ptr RiverLayerShellOutput) (Ptr RiverOutput)
  , focusedWindow :: Maybe (Ptr RiverWindow)
  , focusedOutput :: Ptr RiverOutput
  , focusedWorkspace :: WorkspaceID
  , lastFocusedWorkspace :: WorkspaceID
  , focusedSeat :: Ptr RiverSeat
  , workspaceLayouts :: Map WorkspaceID LayoutType
  , workspaceRatios :: Map WorkspaceID Double
  , allWorkspacesTiled :: BiSeqMap WorkspaceID (Ptr RiverWindow)
  , allWorkspacesFloating :: BiSeqMap WorkspaceID (Ptr RiverWindow)
  , currentWmManager :: Ptr RiverWMManager
  , currentXkbBindings :: Ptr RiverXkbBindings
  , currentLayerShell :: Ptr RiverLayerShell
  , opDeltaState :: OpDeltaState
  , currentOpDelta :: (CInt, CInt, CInt, CInt)
  , cursorPosition :: (CInt, CInt)
  }

data OpDeltaState = Dragging | Resizing RiverEdge | ResizingTile | None

data WlDisplay
data WlRegistry
data WlProxy
data RiverNode
data RiverWindow
data RiverOutput
data RiverSeat
data RiverShellSurface
data RiverWMManager
data RiverXkbBinding
data RiverXkbBindings
data RiverXkbConfig
data RiverLayerShell
data RiverLayerShellOutput
data RiverLayerShellSeat
data RiverPointerBinding
type XkbCallback = Ptr () -> Ptr RiverXkbBinding -> IO ()
type PointerCallback = Ptr () -> Ptr RiverPointerBinding -> IO ()

data WlSurface

data Window = Window
  { winPtr :: Ptr RiverWindow
  , nodePtr :: Ptr RiverNode
  , isFloating :: Bool
  , isFullscreen :: Bool
  , floatingGeometry :: Maybe Rect
  }
  deriving (Eq)

data Output = Output
  { outPtr :: Ptr RiverOutput
  , outLayerShell :: Ptr RiverLayerShellOutput
  , outWidth :: CInt
  , outHeight :: CInt
  , outX :: CInt
  , outY :: CInt
  }
  deriving (Eq)

data KeyConfig = KeyConfig
  { keysym :: CUInt
  , mods :: CUInt
  , action :: WMState -> IO ()
  }

data LayoutType = LayoutType {layoutName :: String, layoutFun :: Double -> Rect -> [Window] -> [(Window, Rect)]}

type RiverEdge = CUInt

edgeNone
  , edgeTop
  , edgeBottom
  , edgeLeft
  , edgeRight
  , edgeAll
  , edgeTopRight
  , edgeTopLeft
  , edgeBottomRight
  , edgeBottomLeft ::
    RiverEdge
edgeAll = edgeBottom .|. edgeLeft .|. edgeTop .|. edgeRight
edgeTopRight = edgeTop .|. edgeRight
edgeTopLeft = edgeTop .|. edgeLeft
edgeBottomRight = edgeBottom .|. edgeRight
edgeBottomLeft = edgeBottom .|. edgeLeft
edgeNone = 0
edgeTop = 1
edgeBottom = 2
edgeLeft = 4
edgeRight = 8
