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
