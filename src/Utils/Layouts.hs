module Utils.Layouts (
  stackLayout,
  monocleLayout,
  twoPaneLayout,
  circleLayout,
  layoutIfMax,
  roledexLayout,
) where

import Data.Sequence as S
import Types

stackLayout :: LayoutType
stackLayout = LayoutType{layoutName = "Stack", layoutFun = applyStack}
 where
  applyStack _ _ _ Empty = empty
  applyStack _ _ total (w :<| Empty) = singleton (w, total) -- Only one window? It gets the whole screen
  applyStack _ r total (master :<| slaves@(slaveHead :<| slavesTail)) =
    let masterWidth = truncate $ fromIntegral (rw total) * r
        masterRect = total{rw = masterWidth}
        stackRect = total{rx = rx total + masterWidth, rw = rw total - masterWidth}
        slaveHeight = rh stackRect `div` (fromIntegral $ S.length slaves)
        leftOverHeight = rh stackRect `mod` (fromIntegral $ S.length slaves)

        -- First slave window gets the leftover height
        slaveHeadGeo = (slaveHead, stackRect{rh = slaveHeight + leftOverHeight})

        -- Map over slaves to give them each a slice of the stack height
        slaveGeos =
          mapWithIndex
            (\i w -> (w, stackRect{ry = ry stackRect + (fromIntegral i * slaveHeight) + leftOverHeight, rh = slaveHeight}))
            slavesTail
     in (master, masterRect) <| slaveHeadGeo <| slaveGeos

monocleLayout :: LayoutType
monocleLayout = LayoutType{layoutName = "Monocle", layoutFun = applyMonocle}
 where
  applyMonocle _ _ total ws = fmap (\w -> (w, total)) ws

twoPaneLayout :: LayoutType
twoPaneLayout = LayoutType{layoutName = "TwoPane", layoutFun = applyTwoPane}
 where
  applyTwoPane _ _ _ Empty = empty
  applyTwoPane _ _ total (w :<| Empty) = singleton (w, total)
  applyTwoPane _ r total (master :<| slaves) =
    let masterWidth = truncate $ fromIntegral (rw total) * r
        masterRect = total{rw = masterWidth}
        stackRect = total{rx = rx total + masterWidth, rw = rw total - masterWidth}
        slaveGeos = fmap (\w -> (w, stackRect)) slaves
     in (master, masterRect) <| slaveGeos

circleLayout :: LayoutType
circleLayout = LayoutType{layoutName = "Circle", layoutFun = applyCircle}
 where
  applyCircle _ _ _ Empty = empty
  applyCircle _ _ Rect{rx, ry, rw, rh} (master :<| slaves) =
    let mW = rw * 4 `div` 5
        mH = rh * 4 `div` 5
        centerX = rx + (rw `div` 2)
        centerY = ry + (rh `div` 2)
        mX = centerX - (mW `div` 2)
        mY = centerY - (mH `div` 2)
        masterRect = Rect{rw = mW, rh = mH, rx = mX, ry = mY}

        w = rw * 3 `div` 5
        h = rh * 3 `div` 5
        radiusX = (rw `div` 2) - (w `div` 2)
        radiusY = (rh `div` 2) - (h `div` 2)
        createRect :: Int -> Rect
        createRect i =
          let angle :: Double = (2 * pi * fromIntegral i) / 4.5
              -- Calculate center of window on the ellipse
              x = centerX + round (fromIntegral radiusX * cos angle)
              y = centerY + round (fromIntegral radiusY * sin angle)
           in Rect{rx = (x - w `div` 2), ry = (y - h `div` 2), rw = w, rh = h}
        slaveGeos =
          mapWithIndex
            (\i win -> (win, createRect i))
            slaves
     in (master, masterRect) <| slaveGeos

roledexLayout :: LayoutType
roledexLayout = LayoutType{layoutName = "Roledex", layoutFun = applyRoledex}
 where
  applyRoledex _ _ _ Empty = empty
  applyRoledex _ _ Rect{rx, ry, rw, rh} (w :<| Empty) =
    let mW = rw * 8 `div` 15
        mH = rh * 8 `div` 15
        mX = rx + (rw `div` 2) - (mW `div` 2)
        mY = ry + (rh `div` 2) - (mH `div` 2)
        masterRect = Rect{rw = mW, rh = mH, rx = mX, ry = mY}
     in singleton (w, masterRect)
  applyRoledex _ _ Rect{rx, ry, rw, rh} wins =
    let mW = rw * 8 `div` 15
        mH = rh * 8 `div` 15
        iW = (rw - mW) `div` (fromIntegral (S.length wins) - 1)
        iH = (rh - mH) `div` (fromIntegral (S.length wins) - 1)
        gapW = (rw - iW * (fromIntegral (S.length wins) - 1) - mW) `div` 2
        gapH = (rh - iH * (fromIntegral (S.length wins) - 1) - mH) `div` 2
        createRect i =
          Rect
            { rw = mW
            , rh = mH
            , rx = rx + rw - mW - gapW - i * iW
            , ry = ry + rh - mH - gapH - i * iH
            }
        res = mapWithIndex (\i win -> (win, createRect $ fromIntegral i)) wins
     in res

layoutIfMax :: Int -> LayoutType -> LayoutType -> LayoutType
layoutIfMax threshold l1 l2 = LayoutType{layoutName = title, layoutFun = applyLayout}
 where
  title = "Either " ++ layoutName l1 ++ " or " ++ layoutName l2
  applyLayout focused r total xs
    | S.length xs <= threshold = layoutFun l1 focused r total xs
    | otherwise = layoutFun l2 focused r total xs
