module Delay where

-- Module that exposes mimicking network delay

import Control.Concurrent (threadDelay)
import Control.Monad.IO.Class (MonadIO, liftIO)

-- Type alias for delay in microseconds
type DelayUS = Int

-- | Custom type for delayed values with a configurable delay. Were monads before but this does not seem to be necessary nor make sense.
-- | Delay is wrapped in IO to allow for generation of random delays more easily, could be changed in the future.
data Delayed a = Delayed
  { delay :: IO DelayUS,
    result :: a
  }

-- | Delay function, simply halts the computation for the given number of microseconds.
-- | Useful for simulating network delays in testing.
doDelay :: (MonadIO m) => m DelayUS -> m ()
doDelay us = do
  us_ <- us
  liftIO $ threadDelay us_

-- | Run the delayed computation with a configurable max delay
runDelayedComputation :: Delayed a -> IO a
runDelayedComputation (Delayed delayUS a) = do
  doDelay delayUS
  return a

-- | Takes some operator and makes it delayed based on some distribution of delays.
wrapDelay ::
  -- ^ Function to generate a random delay
  (() -> IO DelayUS) ->
  -- ^ Operator to wrap
  (a -> b -> c) ->
  -- ^ Wrapped operator
  (a -> b -> Delayed c)
wrapDelay gen op a b = Delayed (gen ()) (a `op` b)