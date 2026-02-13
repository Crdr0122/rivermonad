module Utils.Layouts where

import Types

stackLayout :: LayoutType
stackLayout = LayoutType "Stack" applyStack
 where
  applyStack _ [] = []
  applyStack total [w] = [(w, total)] -- Only one window? It gets the whole screen
  applyStack total (master : slaves) =
    let masterWidth = rw total `div` 2
        masterRect = total{rw = masterWidth}
        stackRect = total{rx = rx total + masterWidth, rw = rw total - masterWidth}
        slaveHeight = rh stackRect `div` length slaves

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
  applyMonocle total ws = map (\w -> (w, total)) ws

twoPaneLayout :: LayoutType
twoPaneLayout = LayoutType "TwoPane" applyTwoPane
 where
  applyTwoPane _ [] = []
  applyTwoPane total [w] = [(w, total)]
  applyTwoPane total (master : slaves) =
    let masterWidth = rw total `div` 2
        masterRect = total{rw = masterWidth}
        stackRect = total{rx = rx total + masterWidth, rw = rw total - masterWidth}
        slaveGeos = map (\w -> (w, stackRect)) slaves
     in (master, masterRect) : slaveGeos
