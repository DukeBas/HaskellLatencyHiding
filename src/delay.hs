module Delay where

-- Module that exposes mimicking network delay

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async
import Control.Monad.IO.Class (MonadIO, liftIO)
import System.Random.MWC
import System.Random.MWC.Distributions (exponential)

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
  (a -> b -> IO (Async c))
wrapDelay gen op a b =
  let delayUS = gen ()
   in async $ do
        us <- delayUS
        threadDelay us
        return $ op a b

-- | Generate a constant delay, input is in seconds
constantDelay :: Double -> () -> IO DelayUS
constantDelay = const . return . toUS

-- | Generate a random delay between lo and hi in seconds with a uniform distribution
uniformDelay :: Double -> Double -> () -> IO DelayUS
uniformDelay lo hi _ = do
  gen <- createSystemRandom
  uniformR (toUS lo, toUS hi) gen

-- | Generate a random delay, with an exponential distribution with lambda rate
exponentialDelay :: Double -> () -> IO DelayUS
exponentialDelay rate _ = do
  gen <- createSystemRandom
  delay <- exponential rate gen
  return $ toUS delay

-- \| Converts seconds to microseconds
toUS :: Double -> DelayUS
toUS = floor . (* 1000000)