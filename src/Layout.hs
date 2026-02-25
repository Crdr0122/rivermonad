module Layout (startLayout) where

import Config
import Data.Foldable
import Data.Map.Strict qualified as M
import Foreign
import Foreign.C
import Types
import Utils.BiMap qualified as B
import Wayland.ImportedFunctions

startLayout :: WMState -> IO (IO ())
startLayout state = do
  let mOut = M.lookup (focusedOutput state) (allOutputs state)
  case mOut of
    Nothing -> pure (pure ())
    Just out -> do
      let (tileable, fullscreened, allNeededWindowPtrs) = getTileableWindows state
          geometry = Rect{rx = outX out, ry = outY out, rh = outHeight out, rw = outWidth out}
          ratio = workspaceRatios state M.! focusedWorkspace state
          layout = layoutFun (workspaceLayouts state M.! focusedWorkspace state) ratio geometry tileable
          borderedLayout = shrinkWindows (fromIntegral borderPx) layout

      -- Step A: Send manage requests immediately
      -- River expects propose_dimensions during the manage cycle
      mapM_
        ( \(w, rect) ->
            riverWindowProposeDimensions (winPtr w) (fromIntegral $ rw rect) (fromIntegral $ rh rect)
        )
        borderedLayout

      -- Step B: Return the render actions for the renderQueue
      -- These will be executed when the compositor sends a render event
      let renderNodeActions =
            mapM_
              ( \(w, rect) ->
                  riverNodeSetPosition (nodePtr w) (fromIntegral $ rx rect) (fromIntegral $ ry rect)
              )
              borderedLayout

          renderBorderActions =
            mapM_ (renderBorder (focusedWindow state) bColor fColor) allNeededWindowPtrs

      return $
        renderNodeActions >> lowerAllWindows tileable >> raiseAllWindows fullscreened >> renderBorderActions

getTileableWindows :: WMState -> ([Window], [Window], [Ptr RiverWindow])
getTileableWindows state =
  let
    windowPtrs = toList (B.lookupBs (focusedWorkspace state) (allWorkspacesTiled state))
    windowFloatingPtrs = toList (B.lookupBs (focusedWorkspace state) (allWorkspacesFloating state))
    windows = fmap ((allWindows state) M.!) windowPtrs
    floatingWindows = fmap ((allWindows state) M.!) windowFloatingPtrs
    tileable = filter (not . isFullscreen) windows
    -- floating = filter (not . isFullscreen) floatingWindows
    fullscreened = filter isFullscreen windows ++ filter isFullscreen floatingWindows
   in
    (tileable, fullscreened, windowPtrs ++ windowFloatingPtrs)

raiseAllWindows :: [Window] -> IO ()
raiseAllWindows = mapM_ (\w -> riverNodePlaceTop (nodePtr w))

lowerAllWindows :: [Window] -> IO ()
lowerAllWindows = mapM_ (\w -> riverNodePlaceBottom (nodePtr w))

shrinkWindows :: Int -> [(Window, Rect)] -> [(Window, Rect)]
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

renderBorder :: Maybe (Ptr RiverWindow) -> (CUInt, CUInt, CUInt, CUInt) -> (CUInt, CUInt, CUInt, CUInt) -> Ptr RiverWindow -> IO ()
renderBorder Nothing (r, g, b, a) _ w = riverWindowSetBorders w edgeAll borderPx r g b a
renderBorder (Just focused) (r, g, b, a) (fr, fg, fb, fa) w
  | w == focused = riverWindowSetBorders w edgeAll borderPx fr fg fb fa
  | otherwise = riverWindowSetBorders w edgeAll borderPx r g b a

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
