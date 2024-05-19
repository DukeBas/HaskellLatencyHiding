module Delay where

  -- Module that exposes mimicking network delay


import Control.Concurrent (threadDelay)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.State

-- Type alias for delay in microseconds
type DelayUS = Int

-- Custom monad type alias for a computation with delays, uses state monad transformer. Automatically a monad stack with IO.
type DelayMonad a = StateT DelayUS IO a

-- Delay function, simply halts the computation for the given number of microseconds.
-- Useful for simulating network delays in testing.
delay :: MonadIO m => DelayUS -> m ()
delay us = do
    liftIO $ threadDelay us

-- Run the delayed computation with a configurable max delay
runDelayedComputation :: DelayUS -> DelayMonad a -> IO a
runDelayedComputation delayUS comp = evalStateT comp delayUS


-- Takes some operator and makes it delayed
wrapDelay :: (a -> a -> a) -> (a -> a -> DelayMonad a)
wrapDelay op a b = do
    delayUS <- get
    delay delayUS
    return (a `op` b)
