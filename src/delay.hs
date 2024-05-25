module Delay where

-- Module that exposes mimicking network delay

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async
import Control.Monad.IO.Class (MonadIO, liftIO)

-- Type alias for delay in microseconds
type DelayUS = Int

-- | Delay function, simply halts the computation for the given number of microseconds.
-- | Useful for simulating network delays in testing.
doDelay :: (MonadIO m) => m DelayUS -> m ()
doDelay us = do
  us_ <- us
  liftIO $ threadDelay us_

-- | Takes some operator and makes it delayed based on some distribution of delays.
-- | Useful for mimicking network delays in testing.
wrapDelay ::
  -- | Function to generate a random delay
  (() -> IO DelayUS) ->
  -- | Operator to wrap
  (a -> b -> c) ->
  -- | Wrapped operator
  (a -> b -> IO c)
wrapDelay gen op a b =
  let delayUS = gen ()
   in do
        asyncRes <- async $ do
          doDelay delayUS
          return $ a `op` b
        wait asyncRes