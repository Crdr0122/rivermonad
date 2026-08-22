{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RecordWildCards #-}

module Layouts.Overview (
  overview,
  ToggleOverview (..),
) where

import Control.Monad (msum)
import Data.Sequence as S
import Data.Typeable

import Types

overview :: Bool -> SomeLayout -> SomeLayout
overview toggle c = SomeLayout $ OverviewLayout toggle c
data OverviewLayout = OverviewLayout
  { overviewToggled :: Bool
  , childLayout :: SomeLayout
  }
instance Layout OverviewLayout where
  doLayout _ _ _ Empty = empty
  doLayout OverviewLayout{overviewToggled = False, childLayout} focused total xs =
    applySomeLayout childLayout focused total xs
  doLayout OverviewLayout{overviewToggled = True} _ Rect{rx, ry, rw, rh} wins =
    let
      nwins = fromIntegral $ S.length wins
      cols = ceiling (sqrt (fromIntegral nwins :: Double))
      rows = (nwins + cols - 1) `div` cols

      -- Base cell dimensions
      cellW = rw `div` cols
      cellH = rh `div` rows

      -- Border/padding inside each grid cell (e.g., ~6% padding with a 6px minimum)
      padX = max 6 (cellW `div` 16)
      padY = max 6 (cellH `div` 16)

      winW = cellW - (2 * padX)
      winH = cellH - (2 * padY)

      createRect i =
        let col = fromIntegral $ i `mod` cols
            row = fromIntegral $ i `div` cols
            cellX = fromIntegral $ rx + (col * cellW)
            cellY = fromIntegral $ ry + (row * cellH)
         in Rect
              { rx = cellX + padX
              , ry = cellY + padY
              , rw = winW
              , rh = winH
              }
     in
      mapWithIndex (\i win -> (win, createRect $ fromIntegral i)) wins

  layoutName OverviewLayout{childLayout = o} = "Overview or " ++ layoutName' o

  handleMsg o@(OverviewLayout t l) m =
    msum
      [ fmap toggle (fromMessage m)
      , goInner
      , Nothing
      ]
   where
    toggle ToggleOverview = o{overviewToggled = not t}
    goInner = case handleSomeMsg l m of
      Nothing -> Nothing
      Just newInner -> Just o{childLayout = newInner}

data ToggleOverview = ToggleOverview deriving (Typeable)
instance Message ToggleOverview
