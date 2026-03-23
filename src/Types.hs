{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

module Types where

import Control.Concurrent
import Control.Concurrent.STM.TQueue
import Data.Aeson
import Data.Bimap
import Data.Map.Strict
import Data.Sequence
import Data.Typeable
import Foreign
import Foreign.C
import GHC.Generics
import Network.Socket
import Utils.BiSeqMap

data Rect = Rect {rx, ry, rw, rh :: CInt} deriving (Show, Eq, Generic)
type WorkspaceID = Int
data WMState = WMState
  { manageQueue :: IO ()
  , renderQueue :: IO ()
  , allWindows :: Map (Ptr RiverWindow) Window
  , allOutputs :: Map (Ptr RiverOutput) Output
  , focusedWindow :: Maybe (Ptr RiverWindow)
  , focusedOutput :: Ptr RiverOutput
  , focusedSeat :: Ptr RiverSeat
  , seatXkbBindings :: Map (Ptr RiverSeat) [Ptr RiverXkbBinding]
  , seatPointerBindings :: Map (Ptr RiverSeat) [Ptr RiverPointerBinding]
  , allLayerShellOutputs :: Map (Ptr RiverLayerShellOutput) (Ptr RiverOutput)
  , allOutputWorkspaces :: Bimap (Ptr RiverOutput) WorkspaceID
  , lastFocusedWorkspace :: WorkspaceID
  , workspaceLayouts :: Map WorkspaceID SomeLayout
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
  , currentKeymapFd :: CInt
  , tQueue :: TQueue WMEvent
  , subscribers :: [Socket]
  }

data OpDeltaState = Dragging | DraggingTile (Ptr RiverWindow) | Resizing RiverEdge | ResizingTile | None

data WMEvent = IPCEvent String Socket

data WlDisplay
data WlRegistry
data WlProxy
data WlSurface
data WlArray
data RiverNode
data RiverWindow
data RiverOutput
data RiverSeat
data RiverShellSurface
data RiverWMManager
data RiverXkbBindings
data RiverXkbBindingsSeat
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
data RiverInputDevice
data RiverLibinputConfig
data RiverLibinputAccelConfig
data RiverLibinputDevice
data RiverLibinputResult
type WlFixedT = CInt

data Window = Window
  { winPtr :: Ptr RiverWindow
  , nodePtr :: Ptr RiverNode
  , winIdentifier :: String
  , winTitle :: String
  , winAppID :: String
  , isFloating :: Bool
  , isFullscreen :: Bool
  , isPinned :: Bool
  , isMaximized :: Bool
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

class (Typeable m) => Message m
data SomeMessage = forall m. (Message m) => SomeMessage m
fromMessage :: (Message m) => SomeMessage -> Maybe m
fromMessage (SomeMessage m) = cast m

data IncMasterFrac = IncMasterFrac Double deriving (Typeable)
data IncMasterN = IncMasterN Int deriving (Typeable)
data SetMasterFrac = SetMasterFrac Double deriving (Typeable)
data Next = Next deriving (Typeable)
instance Message Next
instance Message IncMasterFrac
instance Message IncMasterN
instance Message SetMasterFrac

data SomeLayout = forall l. (Layout l) => SomeLayout l

class Layout l where
  doLayout ::
    l ->
    Maybe Int -> -- index of focused window, or Nothing
    Rect -> -- available geometry
    Seq Window -> -- all windows on this workspace
    Seq (Window, Rect)

  -- Human readable name (shown in status bar, etc.)
  layoutName :: l -> String

  -- Handle messages → possibly produce new layout value
  -- Returns Nothing if message not understood → no change, no refresh
  handleMsg :: l -> SomeMessage -> Maybe l

layoutName' :: SomeLayout -> String
layoutName' (SomeLayout l) = layoutName l

applySomeLayout ::
  SomeLayout ->
  Maybe Int ->
  Rect ->
  Seq Window ->
  Seq (Window, Rect)
applySomeLayout (SomeLayout l) foc rect ws = doLayout l foc rect ws

handleSomeMsg :: SomeLayout -> SomeMessage -> Maybe SomeLayout
handleSomeMsg (SomeLayout l) msg =
  case handleMsg l msg of
    Nothing -> Nothing
    Just new_l -> Just (SomeLayout new_l)

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

data WindowDirection = WindowLeft | WindowRight | WindowUp | WindowDown

data RivermonadConfig = RivermonadConfig
  { defaultLayouts :: Map WorkspaceID SomeLayout
  , execOnStart :: [String]
  , gapPx :: CInt
  , borderPx :: CInt
  , borderColor :: Word32
  , focusedBorderColor :: Word32
  , pinnedBorderColor :: Word32
  , xCursorTheme :: (String, CUInt)
  , workspaceRules :: [(String, String, WorkspaceID)]
  , floatingRules :: [(String, String, WindowStatus)]
  , allPointerBindings :: Map (CUInt, CUInt) (Ptr RiverSeat -> MVar WMState -> IO (), Ptr RiverSeat -> MVar WMState -> IO ())
  , allKeyBindings :: Map (CUInt, CUInt) (Ptr RiverSeat -> MVar WMState -> IO ())
  , statePath :: FilePath
  , composeKeyMap :: String
  }

data PersistedState = PersistedState
  { persistedWindows :: Map String (WorkspaceID, WindowStatus)
  }
  deriving (Generic)

instance ToJSON PersistedState
instance FromJSON PersistedState

data WindowStatus = Tiled | Floating | Fullscreen | FullscreenFloating deriving (Show, Eq, Generic)
instance ToJSON WindowStatus
instance FromJSON WindowStatus
