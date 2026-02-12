module Config where

import Data.IORef
import Data.Map qualified as M
import Foreign
import Foreign.C
import Types
import Utils.KeyDispatches
import Utils.Keysyms
import Wayland.ImportedFunctions

allKeyBindings :: [((CUInt, CUInt), IORef WMState -> IO ())]
allKeyBindings = [((keyQ, modAlt), closeCurrentWindow)]

allKeyBindingdsTransformed :: [((CUInt, CUInt), XkbCallback)]
allKeyBindingdsTransformed =
  map
    ( \((k, m), f) ->
        ( (k, m)
        , (\dataPtr _ -> deRefStablePtr (castPtrToStablePtr dataPtr) >>= f)
        )
    )
    allKeyBindings
