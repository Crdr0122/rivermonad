module Utils.Layouts (
  stackLayout,
  monocleLayout,
  twoPaneLayout,
  circleLayout,
  layoutIfMax,
  roledexLayout,
) where

import Types

stackLayout :: LayoutType
stackLayout = LayoutType "Stack" applyStack
 where
  applyStack _ _ [] = []
  applyStack _ total [w] = [(w, total)] -- Only one window? It gets the whole screen
  applyStack r total (master : slaves) =
    let masterWidth = truncate $ fromIntegral (rw total) * r
        masterRect = total{rw = masterWidth}
        stackRect = total{rx = rx total + masterWidth, rw = rw total - masterWidth}
        slaveHeight = rh stackRect `div` (fromIntegral $ length slaves)

        -- Map over slaves to give them each a slice of the stack height
        slaveGeos =
          zipWith
            (\w i -> (w, stackRect{ry = ry stackRect + (i * slaveHeight), rh = slaveHeight}))
            slaves
            [0 ..]
     in (master, masterRect) : slaveGeos

monocleLayout :: LayoutType
monocleLayout = LayoutType "Monocle" applyMonocle
 where
  applyMonocle _ total ws = map (\w -> (w, total)) ws

twoPaneLayout :: LayoutType
twoPaneLayout = LayoutType "TwoPane" applyTwoPane
 where
  applyTwoPane _ _ [] = []
  applyTwoPane _ total [w] = [(w, total)]
  applyTwoPane r total (master : slaves) =
    let masterWidth = truncate $ fromIntegral (rw total) * r
        masterRect = total{rw = masterWidth}
        stackRect = total{rx = rx total + masterWidth, rw = rw total - masterWidth}
        slaveGeos = map (\w -> (w, stackRect)) slaves
     in (master, masterRect) : slaveGeos

circleLayout :: LayoutType
circleLayout = LayoutType "Circle" applyCircle
 where
  applyCircle _ _ [] = []
  applyCircle _ Rect{rx, ry, rw, rh} (master : slaves) =
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
          zipWith
            (\win i -> (win, createRect i))
            slaves
            [0 ..]
     in (master, masterRect) : slaveGeos

roledexLayout :: LayoutType
roledexLayout = LayoutType "Roledex" applyRoledex
 where
  applyRoledex _ _ [] = []

layoutIfMax :: Int -> LayoutType -> LayoutType -> LayoutType
layoutIfMax threshold l1 l2 = LayoutType title applyLayout
 where
  title = "Either " ++ layoutName l1 ++ " or " ++ layoutName l2
  applyLayout r total xs
    | length xs <= threshold = layoutFun l1 r total xs
    | otherwise = layoutFun l2 r total xs
