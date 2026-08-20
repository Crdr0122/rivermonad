{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RecordWildCards #-}

module Layouts.Basic (
  tall,
  monocle,
  twoPane,
  circle,
  roledex,
  ifMax,
  reflect,
  choose,
  centerMaster,
  threeCol,
  PassInner (..),
) where

import Control.Monad (msum)
import Data.Sequence as S
import Data.Typeable

import Types

tall :: Double -> Int -> SomeLayout
tall frac n = SomeLayout $ TallLayout frac n
data TallLayout = TallLayout
  { tallRatio :: Double
  , tallNMaster :: Int
  }
instance Layout TallLayout where
  layoutName _ = "Tall"
  doLayout _ _ _ Empty = empty
  doLayout _ _ total (w :<| Empty) = singleton (w, total)
  doLayout TallLayout{tallRatio = r, tallNMaster = n} _ total wins
    | S.length wins <= n = splitRect total wins
    | otherwise =
        let masterWidth = truncate $ fromIntegral (rw total) * r
            masterRect = total{rw = masterWidth}
            slaveRect = total{rx = rx total + masterWidth, rw = rw total - masterWidth}
            (masters, slaves) = S.splitAt n wins
            masterGeos = splitRect masterRect masters
            slaveGeos = splitRect slaveRect slaves
         in masterGeos >< slaveGeos
   where
    splitRect _ S.Empty = S.empty
    splitRect rect ws@(w :<| rest) =
      let height = rh rect `div` (fromIntegral $ S.length ws)
          leftOverHeight = rh rect `mod` (fromIntegral $ S.length ws)
          headGeo = (w, rect{rh = height + leftOverHeight})
          slaveGeos =
            mapWithIndex
              (\i win -> (win, rect{ry = ry rect + (fromIntegral (i + 1) * height) + leftOverHeight, rh = height}))
              rest
       in headGeo <| slaveGeos

  handleMsg l@TallLayout{..} m =
    msum
      [ fmap increaseFrac (fromMessage m)
      , fmap setFrac (fromMessage m)
      , fmap increaseN (fromMessage m)
      , Nothing
      ]
   where
    setFrac (SetMasterFrac d) = l{tallRatio = d}
    increaseN (IncMasterN i) = l{tallNMaster = max 1 (tallNMaster + i)}
    increaseFrac (IncMasterFrac d) = l{tallRatio = let clamp = tallRatio + d in if clamp > 0.15 && clamp < 0.85 then clamp else tallRatio}

monocle :: SomeLayout
monocle = SomeLayout MonocleLayout
data MonocleLayout = MonocleLayout
instance Layout MonocleLayout where
  doLayout _ _ total ws = fmap (\w -> (w, total)) ws
  layoutName _ = "Monocle"
  handleMsg _ _ = Nothing

twoPane :: Double -> SomeLayout
twoPane frac = SomeLayout $ TwoPaneLayout frac
data TwoPaneLayout = TwoPaneLayout
  { twoPaneRatio :: Double
  }
instance Layout TwoPaneLayout where
  doLayout _ _ _ Empty = empty
  doLayout _ _ total (w :<| Empty) = singleton (w, total)
  doLayout TwoPaneLayout{twoPaneRatio = r} _ total (master :<| slaves) =
    let masterWidth = truncate $ fromIntegral (rw total) * r
        masterRect = total{rw = masterWidth}
        stackRect = total{rx = rx total + masterWidth, rw = rw total - masterWidth}
        slaveGeos = fmap (\w -> (w, stackRect)) slaves
     in (master, masterRect) <| slaveGeos
  layoutName _ = "TwoPane"
  handleMsg l@TwoPaneLayout{..} m =
    msum
      [ fmap increaseFrac (fromMessage m)
      , fmap setFrac (fromMessage m)
      , Nothing
      ]
   where
    setFrac (SetMasterFrac d) = l{twoPaneRatio = d}
    increaseFrac (IncMasterFrac d) = l{twoPaneRatio = let clamp = twoPaneRatio + d in if clamp > 0.15 && clamp < 0.85 then clamp else twoPaneRatio}

threeCol :: Double -> SomeLayout
threeCol frac = SomeLayout $ ThreeColLayout 1 frac
data ThreeColLayout = ThreeColLayout
  { threeColNMaster :: Int
  , threeColFrac :: Double
  }
instance Layout ThreeColLayout where
  doLayout _ _ _ Empty = empty
  doLayout _ _ total (w :<| Empty) = singleton (w, total)
  doLayout ThreeColLayout{threeColFrac = r, threeColNMaster = n} _ total wins
    | S.length wins <= n = splitRect total wins
    | S.length wins == (n + 1) = splitRect masterRectSingle masters >< splitRect slaveRectSingle slaves
    | otherwise = masterGeos >< combineAlternating (slaveGeosRight, slaveGeosLeft)
   where
    splitRect _ S.Empty = S.empty
    splitRect rect ws@(w :<| rest) =
      let height = rh rect `div` (fromIntegral $ S.length ws)
          leftOverHeight = rh rect `mod` (fromIntegral $ S.length ws)
          headGeo = (w, rect{rh = height + leftOverHeight})
          slaveG = mapWithIndex (\i win -> (win, rect{ry = ry rect + (fromIntegral (i + 1) * height) + leftOverHeight, rh = height})) rest
       in headGeo <| slaveG

    splitAlternating Empty = (Empty, Empty)
    splitAlternating (w :<| Empty) = (singleton w, S.Empty)
    splitAlternating (x :<| y :<| zs) =
      let (xs, ys) = splitAlternating zs
       in (x <| xs, y <| ys)

    combineAlternating (Empty, ys) = ys
    combineAlternating (xs, Empty) = xs
    combineAlternating (x :<| xs, y :<| ys) = x <| y <| combineAlternating (xs, ys)

    (masterWidth, slaveWidth, slaveWidthSingle) =
      let m = truncate $ fromIntegral (rw total) * r
          masterW = (rw total - m) `mod` 2 + m
          singleRemain = rw total - masterW
          halfRemain = singleRemain `div` 2
       in (masterW, halfRemain, singleRemain)

    masterRectSingle = total{rw = masterWidth}
    masterRect = total{rx = rx total + slaveWidth, rw = masterWidth}
    slaveRectSingle = total{rx = rx total + masterWidth, rw = slaveWidthSingle}
    slaveRectLeft = total{rw = slaveWidth}
    slaveRectRight = total{rx = rx total + masterWidth + slaveWidth, rw = slaveWidth}

    (masters, slaves) = S.splitAt n wins
    masterGeos = splitRect masterRect masters
    (slavesRight, slavesLeft) = splitAlternating slaves
    slaveGeosRight = splitRect slaveRectRight slavesRight
    slaveGeosLeft = splitRect slaveRectLeft slavesLeft

  layoutName _ = "Three Col"

  handleMsg l@ThreeColLayout{..} m =
    msum
      [ fmap increaseFrac (fromMessage m)
      , fmap increaseN (fromMessage m)
      , fmap setFrac (fromMessage m)
      , Nothing
      ]
   where
    setFrac (SetMasterFrac d) = l{threeColFrac = d}
    increaseN (IncMasterN i) = l{threeColNMaster = max 1 (threeColNMaster + i)}
    increaseFrac (IncMasterFrac d) = l{threeColFrac = let clamp = threeColFrac + d in if clamp > 0.15 && clamp < 0.85 then clamp else threeColFrac}

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

centerMaster :: SomeLayout -> SomeLayout
centerMaster c = SomeLayout $ CenterMasterLayout c
data CenterMasterLayout = CenterMasterLayout
  { originalLayout :: SomeLayout
  }
instance Layout CenterMasterLayout where
  doLayout _ _ _ Empty = empty
  doLayout CenterMasterLayout{originalLayout = o} focused output@Rect{rx, ry, rw, rh} (master :<| wins) =
    let
      behind = case focused of
        Just 0 -> applySomeLayout o Nothing output wins
        Nothing -> applySomeLayout o Nothing output wins
        Just idx -> applySomeLayout o (Just (idx - 1)) output wins

      -- Base cell dimensions
      winW = rw * 6 `div` 10
      winH = rh * 6 `div` 10

      winX = (rw - winW) `div` 2 + rx
      winY = (rh - winH) `div` 2 + ry
     in
      (master, Rect{rx = winX, ry = winY, rw = winW, rh = winH}) :<| behind

  layoutName CenterMasterLayout{originalLayout = o} = "Centered Master on " ++ layoutName' o

  handleMsg o@(CenterMasterLayout l) m = case handleSomeMsg l m of
    Nothing -> Nothing
    Just newInner -> Just o{originalLayout = newInner}

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
        nwins = S.length wins
        iW = (rw - mW) `div` (fromIntegral nwins - 1)
        iH = (rh - mH) `div` (fromIntegral nwins - 1)
        gapW = (rw - iW * (fromIntegral nwins - 1) - mW) `div` 2
        gapH = (rh - iH * (fromIntegral nwins - 1) - mH) `div` 2
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

reflect :: Bool -> Bool -> SomeLayout -> SomeLayout
reflect hori vert child = SomeLayout $ ReflectLayout hori vert child
data ReflectLayout = ReflectLayout
  { horizontal :: Bool
  , vertical :: Bool
  , reflectChildLayout :: SomeLayout
  }
instance Layout ReflectLayout where
  doLayout ReflectLayout{..} focused total@Rect{rx, ry, rh, rw} xs =
    let before = applySomeLayout reflectChildLayout focused total xs
        calc rect@Rect{rx = x, ry = y, rh = h, rw = w}
          | horizontal && vertical = rect{rx = rx + rw - x - w + rx, ry = ry + rh - y - h + ry}
          | horizontal = rect{rx = rx + rw - x - w + rx}
          | vertical = rect{ry = ry + rh - y - h + ry}
          | otherwise = rect
     in fmap (\(win, rect) -> (win, calc rect)) before

  layoutName l = "Reflected " ++ layoutName' (reflectChildLayout l)
  handleMsg l msg = case handleSomeMsg (reflectChildLayout l) msg of
    Nothing -> Nothing
    Just layout -> Just l{reflectChildLayout = layout}

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
      , fromMessage m >>= passInner
      , goInner
      , Nothing
      ]
   where
    changeIndex Next = c{currentLayout = (i + 1) `mod` Prelude.length opts}
    passInner (PassInner innerM) = toInner innerM
    goInner = toInner m
    toInner message =
      let inner = opts !! currentLayout c
       in case handleSomeMsg inner message of
            Nothing -> Nothing
            Just newInner ->
              let (before, after) = Prelude.splitAt i opts
                  rest = Prelude.drop 1 after
               in Just $ c{layoutOptions = before ++ (newInner : rest)}

data PassInner = PassInner SomeMessage deriving (Typeable)
instance Message PassInner
