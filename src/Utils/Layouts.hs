module Utils.Layouts where

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
  applyCircle _ total [w] = [(w, total)]
  applyCircle _ total@Rect{rx, ry, rw, rh} xs@(master : slaves) =
    let masterWidth = truncate $ fromIntegral rw * 0.6
        masterHeight = truncate $ fromIntegral rh * 0.6
        masterRect = total{rw = masterWidth, rh = masterHeight}
        centerX = rx + (rw `div` 2)
        centerY = ry + (rh `div` 2)
        radiusX = (rw `div` 2) - 100
        radiusY = (rh `div` 2) - 100
        winW = 200
        winH = 150
        createRect i =
          let angle = (2 * pi * fromIntegral i) / fromIntegral (length xs)
              -- Calculate center of window on the ellipse
              x = centerX + round (fromIntegral radiusX * cos angle)
              y = centerY + round (fromIntegral radiusY * sin angle)
           in Rect (x - winW `div` 2) (y - winH `div` 2) winW winH
        slaveGeos =
          zipWith
            (\w i -> (w, createRect i))
            slaves
            [0 ..]
     in slaveGeos

layoutIfMax :: Int -> LayoutType -> LayoutType -> LayoutType
layoutIfMax threshold l1 l2 = LayoutType title applyLayout
 where
  title = "Either " ++ layoutName l1 ++ " or " ++ layoutName l2
  applyLayout r total xs
    | length xs <= threshold = layoutFun l1 r total xs
    | otherwise = layoutFun l2 r total xs
