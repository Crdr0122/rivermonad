{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

module Types where

import Control.Concurrent
import Control.Monad.State (MonadState)
import Data.Aeson
import Data.Bimap
import Data.Map.Strict
import Data.Sequence
import Data.Typeable
import Foreign
import Foreign.C
import GHC.Generics
import Network.Socket
import Optics.Core
import Optics.State
import Utils.BiSeqMap

data Rect = Rect {rx, ry, rw, rh :: CInt} deriving (Show, Eq, Generic)
type WorkspaceID = Int
data WMState = WMState
  { manageQueue :: IO ()
  , renderQueue :: IO ()
  , allWindows :: Map (Ptr RiverWindow) Window
  , allOutputs :: Map (Ptr RiverOutput) Output
  , allSeats :: Map (Ptr RiverSeat) Seat
  , allWlSeats :: Map CUInt WlSeatData
  , focusedWindow :: Maybe (Ptr RiverWindow)
  , focusedOutput :: Ptr RiverOutput
  , focusedSeat :: Ptr RiverSeat
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
  , currentWindowManager :: Ptr RiverWMManager
  , currentXkbBindings :: Ptr RiverXkbBindings
  , currentXkbConfig :: Ptr RiverXkbConfig
  , currentLayerShell :: Ptr RiverLayerShell
  , currentCursorShapeManager :: Ptr CursorShapeManager
  , opDeltaState :: OpDeltaState
  , currentOpDelta :: (CInt, CInt, CInt, CInt)
  , cursorPosition :: (CInt, CInt)
  , persistedState :: Map String (WorkspaceID, WindowStatus)
  , workspaceFocusHistory :: Map WorkspaceID (Ptr RiverWindow)
  , currentKeymapFd :: CInt
  , subscribers :: [Socket]
  }
  deriving (Generic)

data OpDeltaState = Dragging | DraggingTile | Resizing RiverEdge | ResizingTile | None deriving (Eq)

data WMEvent = IPCEvent String Socket

data WlDisplay
data WlRegistry
data WlProxy
data WlSurface
data WlInterface
data WlArray
data WlSeat
data WlPointer
type WlFixedT = CInt

data CursorShapeManager
data CursorShapeDevice

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
  deriving (Generic)

data Output = Output
  { outPtr :: Ptr RiverOutput
  , outLayerShell :: Ptr RiverLayerShellOutput
  , outGeometry :: Rect
  , outWlOutput :: CUInt
  }
  deriving (Generic, Eq)

data WlSeatData = WlSeatData
  { wlSeatPtr :: Ptr WlSeat
  , wlSeatCapabilities :: CUInt
  , wlPointer :: Maybe (Ptr WlPointer)
  , wlPointerSerial :: CUInt
  , wlCursorShapeDevice :: Maybe (Ptr CursorShapeDevice)
  }
  deriving (Generic)

data Seat = Seat
  { seatPtr :: Ptr RiverSeat
  , seatName :: CUInt
  , xkbBindings :: [Ptr RiverXkbBinding]
  , pointerBindings :: [Ptr RiverPointerBinding]
  }
  deriving (Generic)

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
  deriving (Generic)

data PersistedState = PersistedState
  { persistedWindows :: Map String (WorkspaceID, WindowStatus)
  -- , persistedWorkspaces :: Map (Ptr RiverOutput) WorkspaceID
  }
  deriving (Generic)



instance ToJSON PersistedState
instance FromJSON PersistedState

data WindowStatus = Tiled | Floating | Fullscreen | FullscreenFloating deriving (Show, Eq, Generic)
instance ToJSON WindowStatus
instance FromJSON WindowStatus

(<>~) :: (Is k A_Setter, Semigroup b) => Optic k is s t b b -> b -> s -> t
o <>~ m = over o (<> m)
infixr 4 <>~

(<>=) :: (Is k A_Setter, MonadState s m, Semigroup b) => Optic k is s s b b -> b -> m ()
o <>= m = modifying o (<> m)
infix 4 <>=

(%?~) :: (JoinKinds k1 A_Prism k2, Is k2 A_Setter) => Optic k1 is s t (Maybe a) (Maybe b) -> (a -> b) -> s -> t
o %?~ a = over (o % _Just) a
infix 4 %?~

(%?=) :: (JoinKinds k1 A_Prism k2, MonadState s m, Is k2 A_Setter) => Optic k1 is s s (Maybe a) (Maybe b) -> (a -> b) -> m ()
o %?= a = modifying (o % _Just) a
infix 4 %?=
