{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Protocols.Generated where

import Wayland.Generated

$(generateModules "protocols")
