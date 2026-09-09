{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

module Types (module Types, Int32, Word32) where

import Control.Concurrent.MVar
import Control.Monad.Reader
import Control.Monad.State (MonadState)
import Data.Aeson hiding (Object)
import Data.Bimap
import Data.Int
import Data.Map.Strict
import Data.Sequence
import Data.Set qualified as S
import Data.Text (Text)
import Data.Typeable
import Data.Word
import Foreign.C (CInt)
import GHC.Generics
import Network.Socket
import Optics.Core
import Optics.State
import Protocols.Generated
import Utils.BiSeqMap
import Utils.Keysyms
import Wayland.Connection
import Wayland.Generated

data Rect = Rect {rx, ry, rw, rh :: Int32} deriving (Show, Eq, Generic)

nonObject :: Object a
nonObject = Object (ObjectId 0)

modifyMVarW :: MVar a -> (a -> W (a, b)) -> W b
modifyMVarW mvar f = do
  env <- ask
  liftIO $ modifyMVar mvar $ \x -> do
    (x', result) <- runReaderT (f x) env
    pure (x', result)

modifyMVarW_ :: MVar a -> (a -> W a) -> W ()
modifyMVarW_ mvar f = do
  env <- ask
  liftIO $ modifyMVar_ mvar $ \x -> do
    runReaderT (f x) env

type WorkspaceID = Int
data WMState = WMState
  { manageQueue :: W ()
  , renderQueue :: W ()
  , newWindowQueue :: [Object RiverWindowV1]
  , fullscreenQueue :: Map WorkspaceID [Object RiverWindowV1]
  , floatingQueue :: Map WorkspaceID [Object RiverWindowV1]
  , currentWM :: Object RiverWindowManagerV1
  , currentXkbBindings :: Object RiverXkbBindingsV1
  , currentXkbConfig :: Object RiverXkbConfigV1
  , currentLayerShell :: Object RiverLayerShellV1
  , currentCursorShapeManager :: Object WpCursorShapeManagerV1
  , allWindows :: Map (Object RiverWindowV1) Window
  , allOutputs :: Map (Object RiverOutputV1) Output
  , allSeats :: Map (Object RiverSeatV1) Seat
  , allLayerShellOutputs :: Map (Object RiverLayerShellOutputV1) (Object RiverOutputV1)
  , allWlSeats :: Map Word32 WlSeatData
  , workspaceLayouts :: Map WorkspaceID SomeLayout
  , focusedWin :: Maybe (Object RiverWindowV1)
  , focusedOut :: Object RiverOutputV1
  , focusedSeat :: Object RiverSeatV1
  , allOutputWorkspaces :: Bimap (Object RiverOutputV1) WorkspaceID
  , allWorkspacesTiled :: BiSeqMap WorkspaceID (Object RiverWindowV1)
  , allWorkspacesFloating :: BiSeqMap WorkspaceID (Object RiverWindowV1)
  , allWorkspacesFullscreen :: BiSeqMap WorkspaceID (Object RiverWindowV1)
  , workspaceFocusHistory :: Map WorkspaceID (Object RiverWindowV1)
  , lastFocusedWorkspace :: WorkspaceID
  , persistedStateOutputs :: Map Word32 WorkspaceID
  , cursorPosition :: (Int32, Int32)
  , opDeltaState :: OpDeltaState
  , currentOpDelta :: (Int32, Int32, Int32, Int32)
  , subscribers :: [Socket]
  , persistedStateWindows :: Map Text (WorkspaceID, WindowStatus)
  , currentKeymapFd :: Maybe CInt
  }
  deriving (Generic)

data OpDeltaState = Dragging | DraggingTile | Resizing (S.Set RiverWindowV1EdgesFlag) | ResizingTile | None deriving (Eq)

data WMEvent = IPCEvent String Socket

data HsXkbRuleNames = HsXkbRuleNames
  { hsXkbRules :: Maybe String
  , hsXkbModel :: Maybe String
  , hsXkbLayout :: Maybe String
  , hsXkbVariant :: Maybe String
  , hsXkbOptions :: Maybe String
  }

data Window = Window
  { winObj :: Object RiverWindowV1
  , winNodeObj :: Object RiverNodeV1
  , winIdentifier :: Text
  , winAppId :: Text
  , winTitle :: Text
  , winFloat :: Bool
  , winFull :: Bool
  , winPinned :: Bool
  , winMaximized :: Bool
  , winFloatGeo :: Maybe Rect
  , winTileGeo :: Maybe Rect
  , winSizeRule :: Maybe (Int32, Int32)
  , winDimHint :: (Int32, Int32, Int32, Int32)
  , winParent :: Maybe (Object RiverWindowV1)
  }
  deriving (Generic)

data Output = Output
  { outObj :: Object RiverOutputV1
  , outLayerShellObj :: Object RiverLayerShellOutputV1
  , outGeo :: Rect
  , outWlOut :: Word32
  }
  deriving (Generic, Eq)

data WlSeatData = WlSeatData
  { wlSeatPtr :: Object WlSeat
  , wlSeatCapabilities :: Word32
  , wlSeatListenerHsPtr :: Maybe (MVar WMState, Word32)
  , wlPointer :: Maybe (Object WlPointer)
  , wlPointerSerial :: Word32
  , wlCursorShapeDevice :: Maybe (Object WpCursorShapeDeviceV1)
  }
  deriving (Generic)

data Seat = Seat
  { seatObj :: Object RiverSeatV1
  , seatWlSeat :: Word32
  , seatXkbBinds :: [Object RiverXkbBindingV1]
  , seatPtrBinds :: [Object RiverPointerBindingV1]
  }
  deriving (Generic)

class (Typeable m) => Message m
data SomeMessage = forall m. (Message m) => SomeMessage m
fromMessage :: (Message m) => SomeMessage -> Maybe m
fromMessage (SomeMessage m) = cast m

data IncMasterFrac = IncMasterFrac Double deriving (Typeable)
data IncMasterN = IncMasterN Int deriving (Typeable)
data SetMasterFrac = SetMasterFrac Double deriving (Typeable)
data NextLayout = NextLayout deriving (Typeable)
instance Message NextLayout
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

data WindowDirection = WindowLeft | WindowRight | WindowUp | WindowDown

data RivermonadConfig = RivermonadConfig
  { gapPx :: Int32
  , borderPx :: Int32
  , xCursorTheme :: (Text, Word32)
  , allKeyBindings :: Map (Keysym, S.Set RiverSeatV1ModifiersFlag) (Object RiverSeatV1 -> MVar WMState -> W ())
  , allPointerBindings :: Map (PointerBtn, S.Set RiverSeatV1ModifiersFlag) (Object RiverSeatV1 -> MVar WMState -> W (), Object RiverSeatV1 -> MVar WMState -> W ())
  , workspaceRules :: [(Text, Text, WorkspaceID)]
  , floatingRules :: [(Text, Text, WindowStatus)]
  , windowSizeRules :: [(Text, Text, Int32, Int32)]
  , borderColor :: Word32
  , focusedBorderColor :: Word32
  , pinnedBorderColor :: Word32
  , statePath :: FilePath
  , defaultLayouts :: Map WorkspaceID SomeLayout
  , execOnStart :: [String]
  , keyboardOptions :: HsXkbRuleNames
  , keyboardRepeatInfo :: Maybe (Int32, Int32)
  }
  deriving (Generic)

data PersistedState = PersistedState
  { persistedWindows :: Map Text (WorkspaceID, WindowStatus)
  , persistedOutputs :: Map Word32 WorkspaceID
  }
  deriving (Generic)

instance ToJSON PersistedState
instance FromJSON PersistedState

data WindowStatus = Tiled | Floating | Fullscreen | FullscreenFloating deriving (Show, Eq, Generic)
instance ToJSON WindowStatus
instance FromJSON WindowStatus

(>>~) :: (Is k A_Setter, Monad m) => Optic k is s t (m b) (m b) -> (m b) -> s -> t
o >>~ m = over o (>> m)
infixr 4 >>~

(>>>=) :: (Is k A_Setter, MonadState s m, Monad m') => Optic k is s s (m' b) (m' b) -> (m' b) -> m ()
o >>>= m = modifying o (>> m)
infix 4 >>>=

(%?~) :: (JoinKinds k1 A_Prism k2, Is k2 A_Setter) => Optic k1 is s t (Maybe a) (Maybe b) -> (a -> b) -> s -> t
o %?~ a = over (o % _Just) a
infix 4 %?~

(%?=) :: (JoinKinds k1 A_Prism k2, MonadState s m, Is k2 A_Setter) => Optic k1 is s s (Maybe a) (Maybe b) -> (a -> b) -> m ()
o %?= a = modifying (o % _Just) a
infix 4 %?=
