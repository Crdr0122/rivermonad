module Types where

import Control.Concurrent.MVar
import Foreign

allActions :: IO (MVar [IO ()])
allActions = newMVar []

allWindows :: IO (MVar [Window])
allWindows = newMVar []

allOutputs :: IO (MVar [Output])
allOutputs = newMVar []

data Window = Window
  { winPtr :: Ptr ()
  -- , globalName :: T.Text
  }

data Output = Output
  { outPtr :: Ptr ()
  -- , globalName :: T.Text
  }
