{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RecordWildCards #-}

module Layouts.Magnifier (
  magnifier,
  magnifier',
  magnifier2',
) where

-- import Control.Monad (msum)
import Data.Sequence as S
import Types

magnifier :: Double -> SomeLayout -> SomeLayout
magnifier ratio child = SomeLayout $ MagnifierLayout ratio child (AllWins 1)

magnifier' :: Double -> SomeLayout -> SomeLayout
magnifier' ratio child = SomeLayout $ MagnifierLayout ratio child (StackWins 1)

magnifier2' :: Double -> SomeLayout -> SomeLayout
magnifier2' ratio child = SomeLayout $ MagnifierLayout ratio child (StackWins 2)

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
       in case magnifyThis l of
            AllWins cutoff
              | len >= cutoff ->
                  let res = applySomeLayout (childLayout l) focused total ws
                      focusedWinDeleted = S.deleteAt i res
                      (focusedWindow, Rect{rx = x, ry = y, rh = h, rw = w}) = S.index res i
                      newW = min rw (truncate (fromIntegral w * magnifierRatio l))
                      newH = min rh (truncate (fromIntegral h * magnifierRatio l))
                      newX = min (rx + rw) (max (x - ((newW - w) `div` 2)) rx)
                      newY = min (ry + rh) (max (y - ((newH - h) `div` 2)) ry)
                   in (focusedWindow, Rect{rx = newX, ry = newY, rw = newW, rh = newH}) S.<| focusedWinDeleted
            StackWins cutoff
              | len - 1 >= cutoff && i /= 0 -> -- Assume one master window, focused window should not be master
                  let res = applySomeLayout (childLayout l) focused total ws
                      focusedWinDeleted = S.deleteAt i res
                      (focusedWindow, Rect{rx = x, ry = y, rh = h, rw = w}) = S.index res i
                      newW = min rw (truncate (fromIntegral w * magnifierRatio l))
                      newH = min rh (truncate (fromIntegral h * magnifierRatio l))
                      newX = min (rx + rw) (max (x - ((newW - w) `div` 2)) rx)
                      newY = min (ry + rh) (max (y - ((newH - h) `div` 2)) ry)
                   in (focusedWindow, Rect{rx = newX, ry = newY, rw = newW, rh = newH}) S.<| focusedWinDeleted
            _ -> applySomeLayout (childLayout l) focused total ws
