module Hiding where

import Control.Concurrent.Async

-- Communication efficient reduce, where the operation done has some inherent latency (for example due to network communication)

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
delayedReduce :: a -> (a -> a -> IO (Async a)) -> [a] -> IO a
delayedReduce base op list = r (map return list) []
  where
    -- r :: [IO a] -> [Async a] -> IO a
    r (IOx1 : IOx2 : xs) as = do
      -- Main case, combine two elements
      x1 <- IOx1
      x2 <- IOx2
      asyncRes <- op x1 x2
      r xs (asyncRes : as)
    r [x] as = do
      -- Odd number of elements, push the last one wrapped to the output list
      ax <- async x
      r [] (ax : as)
    r [] [] = return base -- No input, return base. Base makes sense to be the identity element of the operation
    r [] [x] = wait x -- Computation finished, return the result
    r [] xs = r (map wait xs) [] -- Recurse on the asyncs list, wait for them to finish before combining the results again
      

-- Like constantDelayReduce, but with a tree structure to not wait for the whole list to be processed
-- Simpler, divide and conquer approach.
delayedTreeReduce :: a -> (a -> a -> IO (Async a)) -> [a] -> IO a
delayedTreeReduce base op list =
  if null list
    then return base
    else red list
  where
    red [x] = return x
    red xs = do
      let (xs_l, xs_r) = splitAt (length xs `div` 2) xs -- Split the list in half
      -- Recurse on the left and right halves
      asyncL <- async $ red xs_l
      asyncR <- async $ red xs_r
      -- Wait for results
      resL <- wait asyncL
      resR <- wait asyncR
      -- Combine the results
      opRes <- op resL resR
      wait opRes


-- Naive foldL based implementation to compare against
delayedFoldLReduce :: a -> (a -> a -> IO(Async a)) -> [a] -> IO a
delayedFoldLReduce base op = foldl f (return base)
  where
    f ioAcc x = do
      acc <- ioAcc           -- Unwrap accumulated value
      asyncOp <- op acc x    -- Do async operation with accumulated value and current element
      wait asyncOp           -- Wait for the async operation to complete

-- Naive foldR based implementation to compare against
delayedFoldRReduce :: a -> (a -> a -> IO (Async a)) -> [a] -> IO a
delayedFoldRReduce base op = foldr f (return base)
  where
    f x ioAcc = do
      acc <- ioAcc           -- Unwrap accumulated value
      asyncOp <- op x acc    -- Do async operation with accumulated value and current element
      wait asyncOp           -- Wait for the async operation to complete

