{-# LANGUAGE RecordWildCards #-}

module Utils.Helpers (
  calculateFloatingPosition,
  calculateFloatingPositions,
  workspaceWindows,
  focusedWorkspace,
  setFocusedWindowAndHistory,
  focusedOutputGeom,
  pairOfGetter,
  pairOf,
  edgeRight,
  edgeTop,
  edgeLeft,
  edgeBottom,
  edgeTopRight,
  edgeTopLeft,
  edgeBottomRight,
  edgeBottomLeft,
  allEdges,
  modNone,
  modSuper,
  modAlt,
  modShift,
  modCtrl,
  modSuperShift,
  modSuperCtrl,
  modSuperAlt,
  modCtrlAlt,
  deleteWinObjs,
) where

import Control.Monad.State
import Data.Bimap qualified as B
import Data.List qualified as L
import Data.Map qualified as M
import Data.Sequence qualified as S
import Data.Set qualified as Se
import Optics.Core
import Optics.State.Operators
import Protocols.Generated
import Types
import Utils.BiSeqMap qualified as BS
import Wayland.Connection

setFocusedWindowAndHistory :: (MonadState WMState m) => WorkspaceID -> Object RiverWindowV1 -> m ()
setFocusedWindowAndHistory ws w = do
  #focusedWin ?= w
  #workspaceFocusHistory % at ws ?= w

deleteWinObjs :: (MonadState WMState m) => Object RiverWindowV1 -> m ()
deleteWinObjs win = do
  #allWorkspacesFloating %= BS.delete win
  #allWorkspacesTiled %= BS.delete win
  #allWorkspacesFullscreen %= BS.delete win
  #newWindowQueue %= L.delete win
  #floatingQueue %= M.map (filter (/= win))
  #fullscreenQueue %= M.map (filter (/= win))
  #workspaceFocusHistory %= M.filter (/= win)

calculateFloatingPositions :: Rect -> [Window] -> Int -> ([Rect], W (), W ())
calculateFloatingPositions o windows num = result
 where
  resultList = fmap (\(n, win) -> calculateFloatingPosition (winObj win) n win o) (zip [num ..] windows)
  result =
    foldl'
      (\(rects, ms, rs) (rect, m, r) -> (rect : rects, ms >> m, rs >> r))
      ([], pure (), pure ())
      resultList

calculateFloatingPosition :: Object RiverWindowV1 -> Int -> Window -> Rect -> (Rect, W (), W ())
calculateFloatingPosition
  win
  num
  Window{winFloatGeo, winNodeObj, winDimHint, winSizeRule}
  Rect{rh = outHeight, rw = outWidth, rx = outX, ry = outY} =
    let (resX, resY, resW, resH) = case winFloatGeo of
          Just Rect{rx, ry, rw, rh} -> (rx, ry, rw, rh)
          Nothing -> case winSizeRule of
            Just (rw, rh) -> ((outWidth - rw) `div` 2 + dx, (outHeight - rh) `div` 2 + dy, rw, rh)
            Nothing -> case winDimHint of
              (0, 0, _, _) -> (offsetX + dx, offsetY + dy, w, h)
              (minW, minH, 0, 0) ->
                let
                  maxW = max minW w
                  maxH = max minH h
                  minY = (outHeight - maxH) `div` 2
                  minX = (outWidth - maxW) `div` 2
                 in
                  (minX + dx, minY + dy, maxW, maxH)
              (_, _, maxW, maxH) ->
                let
                  minW = min maxW w
                  minH = min maxH h
                  maxY = (outHeight - minH) `div` 2
                  maxX = (outWidth - minW) `div` 2
                 in
                  (maxX + dx, maxY + dy, minW, minH)
     in ( Rect{rx = resX, ry = resY, rw = resW, rh = resH}
        , riverWindowV1ProposeDimensions win resW resH
        , riverNodeV1SetPosition winNodeObj (outX + resX) (outY + resY) >> riverNodeV1PlaceTop winNodeObj
        )
   where
    w = outWidth * 6 `div` 10
    h = outHeight * 6 `div` 10
    offsetX = (outWidth - w) `div` 2
    offsetY = (outHeight - h) `div` 2
    -- Bounded scatter offsets based on `num`
    step = num `mod` 8
    scaleX = min 36 (outWidth `div` 30)
    scaleY = min 28 (outHeight `div` 30)

    -- Scatters in all 4 directions around center (X, Y multipliers)
    (multX, multY) = case step of
      0 -> (0, 0) -- Center
      1 -> (1, 1) -- Bottom-Right
      2 -> (-1, 1) -- Bottom-Left
      3 -> (1, -1) -- Top-Right
      4 -> (-1, -1) -- Top-Left
      5 -> (2, 0) -- Far-Right
      6 -> (0, 2) -- Far-Bottom
      7 -> (-2, -2) -- Far-Top-Left
      _ -> (0, 0)

    dx = multX * scaleX
    dy = multY * scaleY

workspaceWindows :: WorkspaceID -> Getter WMState (S.Seq (Object RiverWindowV1))
workspaceWindows ws = to $ \s ->
  (s ^. #allWorkspacesFullscreen % to (BS.lookupBs ws))
    S.>< (s ^. #allWorkspacesTiled % to (BS.lookupBs ws))
    S.>< (s ^. #allWorkspacesFloating % to (BS.lookupBs ws))

focusedWorkspace :: Getter WMState (Maybe WorkspaceID)
focusedWorkspace = to $ \s -> s ^? #allOutputWorkspaces % to (B.lookup (s ^. #focusedOut)) % _Just

focusedOutputGeom :: Getter WMState (Maybe Rect)
focusedOutputGeom = to $ \s -> s ^? #allOutputs % at (s ^. #focusedOut) %? #outGeo

pairOf :: Lens' s a -> Lens' s b -> Lens' s (a, b)
pairOf la lb = lens getter setter
 where
  getter s = (s ^. la, s ^. lb)
  setter s (x, y) = s & la .~ x & lb .~ y

pairOfGetter :: (Is k A_Getter, Is l A_Getter) => Optic' k is s a -> Optic' l js s b -> Getter s (a, b)
pairOfGetter ga gb = to $ \s -> (s ^. ga, s ^. gb)

edgeRight, edgeTop, edgeLeft, edgeBottom, edgeTopRight, edgeTopLeft, edgeBottomRight, edgeBottomLeft :: Se.Set RiverWindowV1EdgesFlag
edgeRight = Se.fromList [RiverWindowV1EdgesRight]
edgeTop = Se.fromList [RiverWindowV1EdgesTop]
edgeLeft = Se.fromList [RiverWindowV1EdgesLeft]
edgeBottom = Se.fromList [RiverWindowV1EdgesBottom]
edgeTopRight = Se.fromList [RiverWindowV1EdgesRight, RiverWindowV1EdgesTop]
edgeTopLeft = Se.fromList [RiverWindowV1EdgesLeft, RiverWindowV1EdgesTop]
edgeBottomRight = (Se.fromList [RiverWindowV1EdgesRight, RiverWindowV1EdgesBottom])
edgeBottomLeft = (Se.fromList [RiverWindowV1EdgesLeft, RiverWindowV1EdgesBottom])
allEdges :: Se.Set RiverWindowV1EdgesFlag
allEdges = Se.fromList [RiverWindowV1EdgesLeft, RiverWindowV1EdgesBottom, RiverWindowV1EdgesTop, RiverWindowV1EdgesRight]

modNone, modSuper, modAlt, modShift, modCtrl, modSuperShift, modSuperCtrl, modSuperAlt, modCtrlAlt :: Se.Set RiverSeatV1ModifiersFlag
modNone = Se.singleton RiverSeatV1ModifiersNone
modSuper = Se.singleton RiverSeatV1ModifiersMod4
modAlt = Se.singleton RiverSeatV1ModifiersMod1
modShift = Se.singleton RiverSeatV1ModifiersShift
modCtrl = Se.singleton RiverSeatV1ModifiersCtrl
modSuperShift = Se.fromList [RiverSeatV1ModifiersMod4, RiverSeatV1ModifiersShift]
modSuperCtrl = Se.fromList [RiverSeatV1ModifiersMod4, RiverSeatV1ModifiersCtrl]
modSuperAlt = Se.fromList [RiverSeatV1ModifiersMod4, RiverSeatV1ModifiersMod1]
modCtrlAlt = Se.fromList [RiverSeatV1ModifiersCtrl, RiverSeatV1ModifiersMod1]
