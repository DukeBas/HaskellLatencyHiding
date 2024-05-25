module Delay where

-- Module that exposes mimicking network delay

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async
import Control.Monad.IO.Class (MonadIO, liftIO)
import System.Random.MWC

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
  (a -> b -> IO(Async c))
wrapDelay gen op a b =
  let delayUS = gen ()
   in async $
        do
          us <- delayUS
          threadDelay us
          return $ op a b

constantDelay :: Double -> () -> IO DelayUS
constantDelay = const . return . toS

-- | Generate a random delay between 0.5 and 1 second
uniformDelay :: () -> IO DelayUS
uniformDelay _ = do
  gen <- createSystemRandom
  uniformR (toS 0.5, toS 1) gen

-- \| Converts seconds to microseconds
toS :: Double -> DelayUS
toS = floor . (* 1000000)