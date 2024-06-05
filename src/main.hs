module Main where

-- Testing and benchmarking

import Control.Concurrent.Async
import Delay
import Hiding

-- Benchmarking
import Criterion.Main

main :: IO ()
main =
  defaultMain
    [ 
      bgroup "test" [
        bench "delayedTreeReduce" $ whnfIO (delayedTreeReduce 0 delayedPlus [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15]),
        bench "delayedReduce" $ whnfIO (delayedReduce 0 delayedPlus [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15]),
        bench "delayedFoldLReduce" $ whnfIO (delayedFoldLReduce 0 delayedPlus [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15]),
        bench "delayedFoldRReduce" $ whnfIO (delayedFoldRReduce 0 delayedPlus [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15])
      ]
    ]

-- Helper function to create a benchmark group for a given reducer (always with DelayedPlus)
-- tests the 4 variants of the reducer with on the same input list
createBenchGroupPlus :: String -> [Int] -> [Benchmark]
createBenchGroupPlus name input_list = [
  bench (name ++ "-delayedTreeReduce") $ whnfIO (delayedTreeReduce 0 delayedPlus input_list),
  bench (name ++ "-delayedReduce") $ whnfIO (delayedReduce 0 delayedPlus input_list),
  bench (name ++ "-delayedFoldLReduce") $ whnfIO (delayedFoldLReduce 0 delayedPlus input_list),
  bench (name ++ "-delayedFoldRReduce") $ whnfIO (delayedFoldRReduce 0 delayedPlus input_list)
  ]


-- Example operator, the + with a uniform random delay between 0.1 and 0.2 seconds
delayedPlus :: Int -> Int -> IO (Async Int)
delayedPlus = wrapDelay (uniformDelay 0.1 0.2) (+)
