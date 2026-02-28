module Layout (startLayout) where

import Config
import Data.Foldable
import Data.Map.Strict qualified as M
import Data.Sequence qualified as S
import Foreign
import Foreign.C
import Types
import Utils.BiSeqMap qualified as BS
import Wayland.ImportedFunctions

startLayout :: WMState -> IO (IO ())
startLayout state = do
  let mOut = M.lookup (focusedOutput state) (allOutputs state)
  case mOut of
    Nothing -> pure (pure ())
    Just Output{outX, outY, outHeight, outWidth} -> do
      let (tileable, fullscreened, allNeededWindowPtrs) = getTileableWindows state
          geometry = Rect{rx = outX, ry = outY, rh = outHeight, rw = outWidth}
          ratio = workspaceRatios state M.! focusedWorkspace state
          layout = layoutFun (workspaceLayouts state M.! focusedWorkspace state) ratio geometry (toList tileable)
          borderedLayout = shrinkWindows borderPx layout

      -- Step A: Send manage requests immediately
      -- River expects propose_dimensions during the manage cycle
      mapM_
        (\(Window{winPtr}, Rect{rw, rh}) -> riverWindowProposeDimensions winPtr rw rh)
        borderedLayout

      -- Step B: Return the render actions for the renderQueue
      -- These will be executed when the compositor sends a render event
      let renderNodeActions =
            mapM_
              (\(Window{nodePtr}, Rect{rx, ry}) -> riverNodeSetPosition nodePtr rx ry)
              borderedLayout

          renderBorderActions =
            mapM_ (renderBorder (focusedWindow state) bColor fColor borderPx) allNeededWindowPtrs

      return $
        renderNodeActions >> lowerAllWindows tileable >> raiseAllWindows fullscreened >> renderBorderActions

getTileableWindows :: WMState -> (S.Seq Window, S.Seq Window, S.Seq (Ptr RiverWindow))
getTileableWindows state =
  let
    windowPtrs = (BS.lookupBs (focusedWorkspace state) (allWorkspacesTiled state))
    windowFloatingPtrs = (BS.lookupBs (focusedWorkspace state) (allWorkspacesFloating state))
    allW = allWindows state
    windows = fmap (allW M.!) windowPtrs
    floatingWindows = fmap (allW M.!) windowFloatingPtrs
    tileable = S.filter (not . isFullscreen) windows
    -- floating = filter (not . isFullscreen) floatingWindows
    fullscreened = S.filter isFullscreen (windows S.>< floatingWindows)
   in
    (tileable, fullscreened, windowPtrs S.>< windowFloatingPtrs)

raiseAllWindows :: (Functor f, Foldable f) => f Window -> IO ()
raiseAllWindows = mapM_ (riverNodePlaceTop . nodePtr)

lowerAllWindows :: (Functor f, Foldable f) => f Window -> IO ()
lowerAllWindows = mapM_ (riverNodePlaceBottom . nodePtr)

shrinkWindows :: CInt -> [(Window, Rect)] -> [(Window, Rect)]
shrinkWindows b =
  map
    ( \(w, r) ->
        ( w
        , r
            { rx = rx r + b
            , ry = ry r + b
            , rh = rh r - 2 * b
            , rw = rw r - 2 * b
            }
        )
    )

renderBorder :: Maybe (Ptr RiverWindow) -> (CUInt, CUInt, CUInt, CUInt) -> (CUInt, CUInt, CUInt, CUInt) -> CInt -> Ptr RiverWindow -> IO ()
renderBorder Nothing (r, g, b, a) _ bPx w = riverWindowSetBorders w edgeAll bPx r g b a
renderBorder (Just focused) (r, g, b, a) (fr, fg, fb, fa) bPx w
  | w == focused = riverWindowSetBorders w edgeAll bPx fr fg fb fa
  | otherwise = riverWindowSetBorders w edgeAll bPx r g b a

translateColor :: Word32 -> (CUInt, CUInt, CUInt, CUInt)
translateColor rgba = (toCU r', toCU g', toCU b', toCU a')
 where
  -- Extract 8-bit components
  r = (rgba `shiftR` 24) .&. 0xFF
  g = (rgba `shiftR` 16) .&. 0xFF
  b = (rgba `shiftR` 8) .&. 0xFF
  a = rgba .&. 0xFF

  -- 1. Scale to 32-bit range (0xFF -> 0xFFFFFFFF)
  -- We use (x * 0x01010101) to distribute the 8 bits across 32 bits evenly
  scale8to32 x = fromIntegral x * 0x01010101 :: Word64

  a32 = scale8to32 a

  -- 2. Pre-multiply (Color * Alpha / MaxAlpha)
  -- We use Word64 to prevent overflow during multiplication
  premult c = (scale8to32 c * a32) `div` 0xFFFFFFFF

  r' = fromIntegral (premult r) :: Word32
  g' = fromIntegral (premult g) :: Word32
  b' = fromIntegral (premult b) :: Word32
  a' = fromIntegral a32 :: Word32

  toCU = fromIntegral

bColor :: (CUInt, CUInt, CUInt, CUInt)
bColor = translateColor borderColor
fColor :: (CUInt, CUInt, CUInt, CUInt)
fColor = translateColor focusedBorderColor
