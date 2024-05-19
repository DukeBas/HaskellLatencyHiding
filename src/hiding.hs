module Hiding where

-- Communication efficient reduce, where the operation done has some inherent latency (for example due to network communication)

import Control.Concurrent.Async (async, wait)
import Delay (DelayMonad, DelayUS, runDelayedComputation)

-- Takes a base element, which is only returned for the empty list, a curried associative operation and a list of input elements
simpleReduce :: a -> (a -> a -> a) -> [a] -> a
simpleReduce base op list = r list []
  where
    r (x1 : x2 : xs) o = r xs (x1 `op` x2 : o) -- Main case, combine two elements
    r [x1] o = r [] (x1 : o) -- Odd number of elements, push the last one as is
    r [] [] = base -- No input, return base. Base makes sense to be the identity element of the operation
    r [] [x] = x -- Computation finished, return the result
    r [] xs = r xs [] -- Recurse on the output list to combine the results again

-- Like simpleReduce, but with a constant delay in the operation
constantDelayReduce :: a -> (a -> a -> DelayMonad a) -> DelayUS -> [a] -> IO a
constantDelayReduce base op delayUS list = r (map return list) []
  where
    r (x1 : x2 : xs) as = do
      -- Main case, combine two elements
      ax1 <- x1
      ax2 <- x2
      asyncOp <- async $ runDelayedComputation delayUS (op ax1 ax2)
      r xs (asyncOp : as)
    r [x1] as = do
      -- Odd number of elements, push the last one wrapped to the output list
      ax1 <- x1
      asyncOp <- async $ return ax1
      r [] (asyncOp : as)
    r [] [] = return base -- No input, return base. Base makes sense to be the identity element of the operation
    r [] [x] = wait x -- Computation finished, return the result
    r [] xs = r (map wait xs) [] -- Recurse on the asyncs list, wait for them to finish before combining the results again

-- todo: more recursive tree like thing like this as to not wait on all results
