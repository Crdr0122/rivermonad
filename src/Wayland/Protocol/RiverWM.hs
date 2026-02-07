{-# LANGUAGE CApiFFI #-}

module Wayland.Protocol.RiverWM where

import Foreign

foreign import ccall unsafe "get_river" get_river :: Ptr ()
