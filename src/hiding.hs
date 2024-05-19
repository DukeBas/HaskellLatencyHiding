module Hiding where

-- Communication efficient reduce, where the operation done has some inherent latency (for example due to network communication)

import System.IO.Unsafe (unsafePerformIO)

import Control.Monad.IO.Class
import Control.Concurrent.Async (async, wait, Async)
import Delay (DelayMonad, DelayUS, delay, runDelayedComputation)

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
-- constantDelayReduce :: a -> (a -> a -> DelayMonad a) -> DelayUS -> [a] -> IO a
-- constantDelayReduce base op delayUS list = r (map return list) []
--   where
--     r :: [IO a] -> [Async a] -> IO a
--     r (x1 : x2 : xs) o = r xs (async $ runDelayedComputation delayUS (op x1 x2) : o) -- Main case, combine two elements, adds delayed
--     r [x1] o = r [] (x1 : o) -- Odd number of elements, push last element wrapped in delay
--     r [] [] = return base -- No input, return base.
--     r [] [x] =  wait x -- Computation finished, return the result
--     r [] xs = r (map wait xs) [] -- Recurse on the output list to combine the results again