module Sandbox where

import Prelude
import UnliftIO.Async (race)

test :: IO ()
test = do
  a <- newEmptyMVar
  b <- newEmptyMVar
  c <- newEmptyTMVarIO

  atomically $ do
    j <- newTVar undefined
    race (readMVar a >>= writeTVar j) (readMVar b >>= writeTVar j)

  pure ()
