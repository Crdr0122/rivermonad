module Utils.Helpers (
  calculateFloatingPosition,
  calculateFloatingPositions,
) where

import Data.Maybe
import Foreign
import Types
import Wayland.ImportedFunctions

calculateFloatingPositions :: (Functor f, Foldable f) => Output -> f Window -> ([Rect], IO (), IO ())
calculateFloatingPositions o windows = res
 where
  resInter = fmap (\win -> calculateFloatingPosition (winPtr win) win o) windows
  res =
    foldl'
      (\(rects, ms, rs) (rect, m, r) -> (rect : rects, ms >> m, rs >> r))
      ([], pure (), pure ())
      resInter

calculateFloatingPosition :: Ptr RiverWindow -> Window -> Output -> (Rect, IO (), IO ())
calculateFloatingPosition
  win
  Window{floatingGeometry, nodePtr, dimensionsHint, parentWindow}
  Output{outHeight, outWidth, outX, outY} = case floatingGeometry of
    Nothing -> case dimensionsHint of
      (0, 0, _, _) ->
        ( Rect{rx = offsetX, ry = offsetY, rw = w, rh = h}
        , riverWindowProposeDimensions win w h
        , riverNodeSetPosition nodePtr (offsetX + outX) (offsetY + outY)
        )
      (minW, minH, _, _)
        | isJust parentWindow ->
            let minY = (outHeight - minH) `div` 2
                minX = (outWidth - minW) `div` 2
             in ( Rect{rx = minX, ry = minY, rw = minW, rh = minH}
                , riverWindowProposeDimensions win minW minH
                , riverNodeSetPosition nodePtr (minX + outX) (minY + outY)
                )
      (_, _, _, _) ->
        ( Rect{rx = offsetX, ry = offsetY, rw = w, rh = h}
        , riverWindowProposeDimensions win w h
        , riverNodeSetPosition nodePtr (offsetX + outX) (offsetY + outY)
        )
    Just g@Rect{rx, ry, rw, rh} ->
      ( g
      , riverWindowProposeDimensions win rw rh
      , riverNodeSetPosition nodePtr (outX + rx) (outY + ry)
      )
   where
    w = outWidth * 6 `div` 10
    h = outHeight * 6 `div` 10
    offsetX = (outWidth - w) `div` 2
    offsetY = (outHeight - h) `div` 2
