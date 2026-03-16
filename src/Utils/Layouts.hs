{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RecordWildCards #-}

module Utils.Layouts (
  TallLayout (..),
  MonocleLayout (..),
  TwoPaneLayout (..),
  CircleLayout (..),
  RoledexLayout (..),
  IfMaxLayout (..),
  MirrorLayout (..),
  ChooseLayout (..),
  MagnifierLayout (..),
) where

import Data.Sequence as S
import Types

data TallLayout = TallLayout
  { masterWindowRatio :: Double
  , masterWindowNum :: Int
  }
  deriving (Show)

instance Layout TallLayout where
  doLayout _ _ _ Empty = empty
  doLayout _ _ total (w :<| Empty) = singleton (w, total)
  doLayout TallLayout{masterWindowRatio = r} _ total (master :<| slaves@(slaveHead :<| slavesTail)) =
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
            (\i w -> (w, stackRect{ry = ry stackRect + (fromIntegral (i + 1) * slaveHeight) + leftOverHeight, rh = slaveHeight}))
            slavesTail
     in (master, masterRect) <| slaveHeadGeo <| slaveGeos

  layoutName _ = "Tall"

  handleMsg TallLayout{..} msg = case msg of
    IncMasterFrac d -> Just TallLayout{masterWindowRatio = let m = masterWindowRatio + d in if m > 0.15 && m < 0.85 then m else masterWindowRatio, masterWindowNum = masterWindowNum}
    _ -> Nothing

data MonocleLayout = MonocleLayout deriving (Show)

instance Layout MonocleLayout where
  doLayout _ _ total ws = fmap (\w -> (w, total)) ws
  layoutName _ = "Monocle"
  handleMsg _ _ = Nothing

data TwoPaneLayout = TwoPaneLayout
  { masterWindowRatio :: Double
  , masterWindowNum :: Int
  }
  deriving (Show)

instance Layout TwoPaneLayout where
  doLayout _ _ _ Empty = empty
  doLayout _ _ total (w :<| Empty) = singleton (w, total)
  doLayout TwoPaneLayout{masterWindowRatio = r} _ total (master :<| slaves) =
    let masterWidth = truncate $ fromIntegral (rw total) * r
        masterRect = total{rw = masterWidth}
        stackRect = total{rx = rx total + masterWidth, rw = rw total - masterWidth}
        slaveGeos = fmap (\w -> (w, stackRect)) slaves
     in (master, masterRect) <| slaveGeos
  layoutName _ = "Two Pane"
  handleMsg TwoPaneLayout{..} msg = case msg of
    IncMasterFrac d -> Just TwoPaneLayout{masterWindowRatio = let m = masterWindowRatio + d in if m > 0.15 && m < 0.85 then m else masterWindowRatio, masterWindowNum = masterWindowNum}
    _ -> Nothing

data CircleLayout = CircleLayout deriving (Show)

instance Layout CircleLayout where
  doLayout _ _ _ Empty = empty
  doLayout _ _ Rect{rx, ry, rw, rh} (master :<| slaves) =
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
  layoutName _ = "Circle"
  handleMsg _ _ = Nothing

data RoledexLayout = RoledexLayout deriving (Show)

instance Layout RoledexLayout where
  doLayout _ _ _ Empty = empty
  doLayout _ _ Rect{rx, ry, rw, rh} (w :<| Empty) =
    let mW = rw * 8 `div` 15
        mH = rh * 8 `div` 15
        mX = rx + (rw `div` 2) - (mW `div` 2)
        mY = ry + (rh `div` 2) - (mH `div` 2)
        masterRect = Rect{rw = mW, rh = mH, rx = mX, ry = mY}
     in singleton (w, masterRect)
  doLayout _ _ Rect{rx, ry, rw, rh} wins =
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

  layoutName _ = "Roledex"
  handleMsg _ _ = Nothing

data IfMaxLayout = IfMaxLayout
  { firstChildLayout :: SomeLayout
  , secondChildLayout :: SomeLayout
  , windowThreshold :: Int
  }

instance Show IfMaxLayout where
  show i = "Either " ++ layoutName' (firstChildLayout i) ++ " or " ++ layoutName' (secondChildLayout i)

instance Layout IfMaxLayout where
  doLayout IfMaxLayout{..} focused total xs
    | S.length xs <= windowThreshold = applySomeLayout firstChildLayout focused total xs
    | otherwise = applySomeLayout secondChildLayout focused total xs
  handleMsg _ _ = Nothing

data MirrorLayout = MirrorLayout
  { horizontal :: Bool
  , vertical :: Bool
  , mirrorChildLayout :: SomeLayout
  }

instance Show MirrorLayout where
  show l = "Mirrored " ++ layoutName' (mirrorChildLayout l)

instance Layout MirrorLayout where
  doLayout MirrorLayout{..} focused total@Rect{rx, ry, rh, rw} xs =
    let before = applySomeLayout mirrorChildLayout focused total xs
        mirror rect@Rect{rx = x, ry = y, rh = h, rw = w}
          | horizontal && vertical = rect{rx = rx + rw - x - w + rx, ry = ry + rh - y - h + ry}
          | horizontal = rect{rx = rx + rw - x - w + rx}
          | vertical = rect{ry = ry + rh - y - h + ry}
          | otherwise = rect
     in fmap (\(win, rect) -> (win, mirror rect)) before

  handleMsg l msg = case handleSomeMsg (mirrorChildLayout l) msg of
    Nothing -> Nothing
    Just layout -> Just l{mirrorChildLayout = layout}

data ChooseLayout = ChooseLayout
  { currentLayout :: Int
  , layoutOptions :: [SomeLayout]
  }

instance Show ChooseLayout where
  show c = layoutName' (layoutOptions c !! currentLayout c)

instance Layout ChooseLayout where
  doLayout c foc rect ws =
    applySomeLayout (layoutOptions c !! currentLayout c) foc rect ws

  handleMsg c@(ChooseLayout i opts) Next =
    Just $ c{currentLayout = (i + 1) `mod` Prelude.length opts}
  handleMsg c@(ChooseLayout i opts) msg =
    let inner = opts !! currentLayout c
     in case handleSomeMsg inner msg of
          Nothing -> Nothing
          Just newInner ->
            let (before, after) = Prelude.splitAt i opts
                rest = Prelude.drop 1 after
             in Just $ c{layoutOptions = before ++ [newInner] ++ rest}

data MagnifierLayout = MagnifierLayout
  { magnifierRatio :: Double
  , mChildLayout :: SomeLayout
  }

instance Show MagnifierLayout where
  show l = "Magnified " ++ layoutName' (mChildLayout l)

instance Layout MagnifierLayout where
  handleMsg l msg = case handleSomeMsg (mChildLayout l) msg of
    Nothing -> Nothing
    Just layout -> Just l{mChildLayout = layout}
  doLayout l focused total@Rect{rx, ry, rh, rw} ws = case focused of
    Nothing -> applySomeLayout (mChildLayout l) focused total ws
    Just i ->
      let res = applySomeLayout (mChildLayout l) focused total ws
          deleted = S.deleteAt i res
          (focusedWindow, Rect{rx = x, ry = y, rh = h, rw = w}) = S.index res i
          newW = min rw (truncate (fromIntegral w * magnifierRatio l))
          newH = min rh (truncate (fromIntegral h * magnifierRatio l))
          newX = min (rx + rw) (max (x - ((newW - w) `div` 2)) rx)
          newY = min (ry + rh) (max (y - ((newH - h) `div` 2)) ry)
       in (focusedWindow, Rect{rx = newX, ry = newY, rw = newW, rh = newH}) S.<| deleted
