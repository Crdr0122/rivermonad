{-# LANGUAGE DeriveGeneric #-}

module Types where

import Control.Concurrent.MVar
import Data.Aeson
import Data.Bimap
import Data.Map.Strict
import Foreign
import Foreign.C
import GHC.Generics
import Utils.BiSeqMap

data Rect = Rect {rx, ry, rw, rh :: CInt} deriving (Show, Eq)
type WorkspaceID = Int
type WMStateRef = MVar WMState
data WMState = WMState
  { manageQueue :: IO ()
  , renderQueue :: IO ()
  , allWindows :: Map (Ptr RiverWindow) Window
  , allOutputs :: Map (Ptr RiverOutput) Output
  , allLayerShellOutputs :: Map (Ptr RiverLayerShellOutput) (Ptr RiverOutput)
  , focusedWindow :: Maybe (Ptr RiverWindow)
  , focusedOutput :: Ptr RiverOutput
  , allOutputWorkspaces :: Bimap (Ptr RiverOutput) WorkspaceID
  , lastFocusedWorkspace :: WorkspaceID
  , focusedSeat :: Ptr RiverSeat
  , workspaceLayouts :: Map WorkspaceID LayoutType
  , workspaceRatios :: Map WorkspaceID Double
  , allWorkspacesTiled :: BiSeqMap WorkspaceID (Ptr RiverWindow)
  , allWorkspacesFloating :: BiSeqMap WorkspaceID (Ptr RiverWindow)
  , allWorkspacesFullscreen :: BiSeqMap WorkspaceID (Ptr RiverWindow)
  , newWindowQueue :: [Ptr RiverWindow]
  , floatingQueue :: Map WorkspaceID [Ptr RiverWindow]
  , fullscreenQueue :: Map WorkspaceID [Ptr RiverWindow]
  , currentWmManager :: Ptr RiverWMManager
  , currentXkbBindings :: Ptr RiverXkbBindings
  , currentXkbConfig :: Ptr RiverXkbConfig
  , currentLayerShell :: Ptr RiverLayerShell
  , opDeltaState :: OpDeltaState
  , currentOpDelta :: (CInt, CInt, CInt, CInt)
  , cursorPosition :: (CInt, CInt)
  , persistedState :: Map String (WorkspaceID, WindowStatus)
  }

data OpDeltaState = Dragging | DraggingTile (Ptr RiverWindow) | Resizing RiverEdge | ResizingTile | None

data WlDisplay
data WlRegistry
data WlProxy
data RiverNode
data RiverWindow
data RiverOutput
data RiverSeat
data RiverShellSurface
data RiverWMManager
data RiverXkbBindings
data RiverXkbBinding
type XkbCallback = Ptr () -> Ptr RiverXkbBinding -> IO ()
data RiverLayerShell
data RiverLayerShellOutput
data RiverLayerShellSeat
data RiverPointerBinding
type PointerCallback = Ptr () -> Ptr RiverPointerBinding -> IO ()
data RiverXkbConfig
data RiverXkbKeyboard
data RiverXkbKeymap
data RiverInputManager
data RiverLibinputConfig

data WlSurface

data Window = Window
  { winPtr :: Ptr RiverWindow
  , nodePtr :: Ptr RiverNode
  , winIdentifier :: String
  , winTitle :: String
  , winAppID :: String
  , isFloating :: Bool
  , isFullscreen :: Bool
  , isPinned :: Bool
  , floatingGeometry :: Maybe Rect
  , tilingGeometry :: Maybe Rect
  , dimensionsHint :: (CInt, CInt, CInt, CInt)
  , parentWindow :: Maybe (Ptr RiverWindow)
  }

data Output = Output
  { outPtr :: Ptr RiverOutput
  , outLayerShell :: Ptr RiverLayerShellOutput
  , outWidth :: CInt
  , outHeight :: CInt
  , outX :: CInt
  , outY :: CInt
  }
  deriving (Generic, Eq)

data LayoutType = LayoutType
  { layoutName :: String
  , layoutFun :: Double -> Rect -> [Window] -> [(Window, Rect)]
  }

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

data PersistedState = PersistedState
  { persistedWindows :: Map String (WorkspaceID, WindowStatus)
  , persistedWorkspaceRatios :: Map WorkspaceID Double
  }
  deriving (Generic)

instance ToJSON PersistedState
instance FromJSON PersistedState

data WindowStatus = Tiled | Floating | Fullscreen | FullscreenFloating deriving (Show, Eq, Generic)
instance ToJSON WindowStatus
instance FromJSON WindowStatus
