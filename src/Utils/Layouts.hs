{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RecordWildCards #-}

module Utils.Layouts (
  tall,
  monocle,
  twoPane,
  circle,
  roledex,
  ifMax,
  mirror,
  choose,
  magnifier,
) where

import Control.Monad (msum)
import Data.Sequence as S
import Types

tall :: Double -> Int -> SomeLayout
tall frac n = SomeLayout $ TallLayout frac n
data TallLayout = TallLayout
  { masterWindowRatio :: Double
  , masterWindowNum :: Int
  }
instance Layout TallLayout where
  layoutName _ = "Tall"
  doLayout _ _ _ Empty = empty
  doLayout _ _ total (w :<| Empty) = singleton (w, total)
  doLayout TallLayout{masterWindowRatio = r} _ total (master :<| slaves@(slaveHead :<| slavesTail)) =
    let masterWidth = truncate $ fromIntegral (rw total) * r
        masterRect = total{rw = masterWidth}
        stackRect = total{rx = rx total + masterWidth, rw = rw total - masterWidth}
        slaveHeight = rh stackRect `div` (fromIntegral $ S.length slaves)
        leftOverHeight = rh stackRect `mod` (fromIntegral $ S.length slaves)

        slaveHeadGeo = (slaveHead, stackRect{rh = slaveHeight + leftOverHeight})

        slaveGeos =
          mapWithIndex
            (\i w -> (w, stackRect{ry = ry stackRect + (fromIntegral (i + 1) * slaveHeight) + leftOverHeight, rh = slaveHeight}))
            slavesTail
     in (master, masterRect) <| slaveHeadGeo <| slaveGeos

  handleMsg TallLayout{..} m =
    msum
      [ fmap increaseFrac (fromMessage m)
      , fmap setFrac (fromMessage m)
      , fmap increaseN (fromMessage m)
      , Nothing
      ]
   where
    setFrac (SetMasterFrac d) = TallLayout{masterWindowRatio = d, masterWindowNum = masterWindowNum}
    increaseN (IncMasterN i) = TallLayout{masterWindowRatio = masterWindowRatio, masterWindowNum = max 0 (masterWindowNum + i)}
    increaseFrac (IncMasterFrac d) =
      TallLayout
        { masterWindowRatio =
            let clamp = masterWindowRatio + d in if clamp > 0.15 && clamp < 0.85 then clamp else masterWindowRatio
        , masterWindowNum = masterWindowNum
        }

monocle :: SomeLayout
monocle = SomeLayout MonocleLayout
data MonocleLayout = MonocleLayout
instance Layout MonocleLayout where
  doLayout _ _ total ws = fmap (\w -> (w, total)) ws
  layoutName _ = "Monocle"
  handleMsg _ _ = Nothing

twoPane :: Double -> Int -> SomeLayout
twoPane frac n = SomeLayout $ TwoPaneLayout frac n
data TwoPaneLayout = TwoPaneLayout
  { masterWindowRatio :: Double
  , masterWindowNum :: Int
  }
instance Layout TwoPaneLayout where
  doLayout _ _ _ Empty = empty
  doLayout _ _ total (w :<| Empty) = singleton (w, total)
  doLayout TwoPaneLayout{masterWindowRatio = r} _ total (master :<| slaves) =
    let masterWidth = truncate $ fromIntegral (rw total) * r
        masterRect = total{rw = masterWidth}
        stackRect = total{rx = rx total + masterWidth, rw = rw total - masterWidth}
        slaveGeos = fmap (\w -> (w, stackRect)) slaves
     in (master, masterRect) <| slaveGeos
  layoutName _ = "TwoPane"
  handleMsg TwoPaneLayout{..} m =
    msum
      [ fmap increaseFrac (fromMessage m)
      , fmap setFrac (fromMessage m)
      , fmap increaseN (fromMessage m)
      , Nothing
      ]
   where
    setFrac (SetMasterFrac d) = TwoPaneLayout{masterWindowRatio = d, masterWindowNum = masterWindowNum}
    increaseN (IncMasterN i) = TwoPaneLayout{masterWindowRatio = masterWindowRatio, masterWindowNum = max 0 (masterWindowNum + i)}
    increaseFrac (IncMasterFrac d) =
      TwoPaneLayout
        { masterWindowRatio =
            let clamp = masterWindowRatio + d in if clamp > 0.15 && clamp < 0.85 then clamp else masterWindowRatio
        , masterWindowNum = masterWindowNum
        }

circle :: SomeLayout
circle = SomeLayout CircleLayout
data CircleLayout = CircleLayout
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

roledex :: SomeLayout
roledex = SomeLayout RoledexLayout
data RoledexLayout = RoledexLayout
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

ifMax :: SomeLayout -> SomeLayout -> Int -> SomeLayout
ifMax l1 l2 n = SomeLayout $ IfMaxLayout l1 l2 n
data IfMaxLayout = IfMaxLayout
  { firstChildLayout :: SomeLayout
  , secondChildLayout :: SomeLayout
  , windowThreshold :: Int
  }
instance Layout IfMaxLayout where
  doLayout IfMaxLayout{..} focused total xs
    | S.length xs <= windowThreshold = applySomeLayout firstChildLayout focused total xs
    | otherwise = applySomeLayout secondChildLayout focused total xs
  layoutName i = "Either " ++ layoutName' (firstChildLayout i) ++ " or " ++ layoutName' (secondChildLayout i)
  handleMsg l m = Just $ l{firstChildLayout = l1, secondChildLayout = l2}
   where
    l1 = case handleSomeMsg (firstChildLayout l) m of
      Nothing -> firstChildLayout l
      Just layout -> layout
    l2 = case handleSomeMsg (secondChildLayout l) m of
      Nothing -> secondChildLayout l
      Just layout -> layout

mirror :: Bool -> Bool -> SomeLayout -> SomeLayout
mirror hori vert child = SomeLayout $ MirrorLayout hori vert child
data MirrorLayout = MirrorLayout
  { horizontal :: Bool
  , vertical :: Bool
  , mirrorChildLayout :: SomeLayout
  }
instance Layout MirrorLayout where
  doLayout MirrorLayout{..} focused total@Rect{rx, ry, rh, rw} xs =
    let before = applySomeLayout mirrorChildLayout focused total xs
        calc rect@Rect{rx = x, ry = y, rh = h, rw = w}
          | horizontal && vertical = rect{rx = rx + rw - x - w + rx, ry = ry + rh - y - h + ry}
          | horizontal = rect{rx = rx + rw - x - w + rx}
          | vertical = rect{ry = ry + rh - y - h + ry}
          | otherwise = rect
     in fmap (\(win, rect) -> (win, calc rect)) before

  layoutName l = "Mirrored " ++ layoutName' (mirrorChildLayout l)
  handleMsg l msg = case handleSomeMsg (mirrorChildLayout l) msg of
    Nothing -> Nothing
    Just layout -> Just l{mirrorChildLayout = layout}

choose :: Int -> [SomeLayout] -> SomeLayout
choose i opts = SomeLayout $ ChooseLayout i opts
data ChooseLayout = ChooseLayout
  { currentLayout :: Int
  , layoutOptions :: [SomeLayout]
  }
instance Layout ChooseLayout where
  doLayout c foc rect ws =
    applySomeLayout (layoutOptions c !! currentLayout c) foc rect ws

  layoutName c = layoutName' (layoutOptions c !! currentLayout c)

  handleMsg c@(ChooseLayout i opts) m =
    msum
      [ fmap changeIndex (fromMessage m)
      , goInner
      , Nothing
      ]
   where
    changeIndex Next =
      c{currentLayout = (i + 1) `mod` Prelude.length opts}
    goInner =
      let inner = opts !! currentLayout c
       in case handleSomeMsg inner m of
            Nothing -> Nothing
            Just newInner ->
              let (before, after) = Prelude.splitAt i opts
                  rest = Prelude.drop 1 after
               in Just $ c{layoutOptions = before ++ [newInner] ++ rest}

magnifier :: Double -> SomeLayout -> SomeLayout
magnifier ratio child = SomeLayout $ MagnifierLayout ratio child
data MagnifierLayout = MagnifierLayout
  { magnifierRatio :: Double
  , mChildLayout :: SomeLayout
  }
instance Layout MagnifierLayout where
  layoutName l = "Magnified " ++ layoutName' (mChildLayout l)
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
