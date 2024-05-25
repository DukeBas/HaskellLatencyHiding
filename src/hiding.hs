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
    r (x1 : x2 : xs) as = do
      -- Main case, combine two elements
      ax1 <- x1
      ax2 <- x2
      asyncRes <- op ax1 ax2
      r xs (asyncRes : as)
    r [x1] as = do
      -- Odd number of elements, push the last one wrapped to the output list
      ax1 <- async x1
      r [] (ax1 : as)
    r [] [] = return base -- No input, return base. Base makes sense to be the identity element of the operation
    r [] [x] =
      do
        -- Computation finished, return the result
        wait x
    r [] xs = 
      -- Recurse on the asyncs list, wait for them to finish before combining the results again
      r (map wait xs) []
      

-- -- Like constantDelayReduce, but with a tree structure to not wait for the whole list to be processed
-- -- Simpler, divide and conquer approach.
-- delayedTreeReduce :: a -> (a -> a -> IO(Async a)) -> [a] -> IO a
-- delayedTreeReduce base op list =
--   if null list
--     then return base
--     else red list
--   where
--     -- red :: [t] -> IO t
--     red [x] = return x
--     red xs = do
--       let (xs_l, xs_r) = splitAt (length xs `div` 2) xs -- Split the list in half
--       resL <- red xs_l
--       resR <- red xs_r
--       asyncRes <- op resL resR
--       wait asyncRes

-- -- Naive foldL based implementation to compare against
-- delayedFoldLReduce :: a -> (a -> a -> Async a) -> [a] -> IO a
-- delayedFoldLReduce base op = foldl f (return base)
--   where
--     -- f :: IO a -> a -> IO a
--     f acc x = do
--       ax <- acc
--       asyncOp <- async $ runDelayedComputation (ax `op` x)
--       wait asyncOp

-- -- Naive foldR based implementation to compare against
-- delayedFoldRReduce :: a -> (a -> a -> Async a) -> [a] -> IO a
-- delayedFoldRReduce base op = foldr f (return base)
--   where
--     -- f :: IO a -> a -> IO a
--     f x acc = do
--       ax <- acc
--       asyncOp <- async $ runDelayedComputation (ax `op` x)
--       wait asyncOp
