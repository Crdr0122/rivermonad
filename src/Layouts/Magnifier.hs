{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RecordWildCards #-}

module Layouts.Magnifier (
  magnifier,
  magnifier',
  magnifierNum,
  magnifierNum',
) where

-- import Control.Monad (msum)
import Data.Sequence as S
import Types

magnifier :: Double -> SomeLayout -> SomeLayout
magnifier ratio child = SomeLayout $ MagnifierLayout ratio child (AllWins 1)

magnifier' :: Double -> SomeLayout -> SomeLayout
magnifier' ratio child = SomeLayout $ MagnifierLayout ratio child (StackWins 1)

magnifierNum :: Double -> SomeLayout -> Int -> SomeLayout
magnifierNum ratio child num = SomeLayout $ MagnifierLayout ratio child (StackWins num)

magnifierNum' :: Double -> SomeLayout -> Int -> SomeLayout
magnifierNum' ratio child num = SomeLayout $ MagnifierLayout ratio child (StackWins num)

data MagnifyThis = AllWins !Int | StackWins !Int deriving (Read, Show)

data MagnifierLayout = MagnifierLayout
  { magnifierRatio :: Double
  , childLayout :: SomeLayout
  , magnifyThis :: MagnifyThis
  }
instance Layout MagnifierLayout where
  layoutName l = "Magnified " ++ layoutName' (childLayout l)
  handleMsg l msg = case handleSomeMsg (childLayout l) msg of
    Nothing -> Nothing
    Just layout -> Just l{childLayout = layout}
  doLayout l focused total@Rect{rx, ry, rh, rw} ws = case focused of
    Nothing -> applySomeLayout (childLayout l) focused total ws
    Just i ->
      let len = S.length ws
          res = applySomeLayout (childLayout l) focused total ws
          focusedWinDeleted = S.deleteAt i res
          (focusedWindow, Rect{rx = x, ry = y, rh = h, rw = w}) = S.index res i
          newW = min rw (truncate (fromIntegral w * magnifierRatio l))
          newH = min rh (truncate (fromIntegral h * magnifierRatio l))
          newX = min (rx + rw - newW) (max (x - ((newW - w) `div` 2)) rx)
          newY = min (ry + rh - newH) (max (y - ((newH - h) `div` 2)) ry)
       in case magnifyThis l of
            AllWins cutoff | len >= cutoff -> (focusedWindow, Rect{rx = newX, ry = newY, rw = newW, rh = newH}) S.<| focusedWinDeleted
            StackWins cutoff
              | len - 1 >= cutoff && i /= 0 -> -- Assume one master window, focused window should not be master
                  (focusedWindow, Rect{rx = newX, ry = newY, rw = newW, rh = newH}) S.<| focusedWinDeleted
            _ -> res
