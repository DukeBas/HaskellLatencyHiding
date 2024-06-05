{-# OPTIONS_GHC -Wno-type-defaults #-}

module Main where

-- Testing and benchmarking

import Control.Concurrent.Async
-- Benchmarking

import qualified Control.Monad
import Criterion.Main
import Delay
import Hiding
import System.Random

main :: IO ()
main =
  do
    -- Define the ranges for the random lists
    let lo = 0
        hi = 100 -- Watch out for overflow! Keep these reasonable
        -- Generate test lists
    short <- generateRandomList lo hi 100
    medium <- generateRandomList lo hi 1000
    long <- generateRandomList lo hi 10000
    extreme <- generateRandomList lo hi 100000
    -- Setup operators -- Note that we only use + as the operator itself is not so relevant but moreso that it is wrapped with delay
    -- We differentiate in what distribution the delays are generated
    -- First constant delays
    let constant_1_ms = wrapPlus (constantDelay 0.001)
        constant_10_ms = wrapPlus (constantDelay 0.01)
        constant_100_ms = wrapPlus (constantDelay 0.1)
        constant_1000_ms = wrapPlus (constantDelay 1)
    -- Then delays with uniform distribution, scaling the range of the delays
    let uniform_1_11_ms = wrapPlus (uniformDelay 0.001 0.011)
        uniform_10_110_ms = wrapPlus (uniformDelay 0.01 0.11)
        uniform_100_1100_ms = wrapPlus (uniformDelay 0.1 1.1)
    -- Lastly, delays with an exponential distribution to more accurately mimic real network delays
    let exponential_1 = wrapPlus (exponentialDelay 1000)
        exponential_10 = wrapPlus (exponentialDelay (1000 :: Double))
        exponential_100 = wrapPlus (exponentialDelay (10 :: Double))
        exponential_1000 = wrapPlus (exponentialDelay (1 :: Double))

    -- Do benchmarks
    defaultMain
      [ -- First do 1, 10, 100 for shorter input lists and all operators
        createBenchMatrix
          [ ("constant_1_ms", constant_1_ms),
            ("constant_10_ms", constant_10_ms),
            ("constant_100_ms", constant_100_ms)
          ]
          [("short", short), ("medium", medium)],
        createBenchMatrix
          [ ("uniform_1_11_ms", uniform_1_11_ms),
            ("uniform_10_110_ms", uniform_10_110_ms)
          ]
          [("short", short), ("medium", medium)],
        createBenchMatrix
          [ ("exponential_1", exponential_1),
            ("exponential_10", exponential_10),
            ("exponential_100", exponential_100)
          ]
          [("short", short), ("medium", medium)],
        -- Then only run treeReduce and reduce for the longer lists
        createFastBenchMatrix
          [ ("constant_1_ms", constant_1_ms),
            ("constant_10_ms", constant_10_ms),
            ("constant_100_ms", constant_100_ms),
            ("constant_1000_ms", constant_1000_ms)
          ]
          [("long", long), ("extreme", extreme)],
        createFastBenchMatrix
          [ ("uniform_1_11_ms", uniform_1_11_ms),
            ("uniform_10_110_ms", uniform_10_110_ms),
            ("uniform_100_1100_ms", uniform_100_1100_ms)
          ]
          [("long", long), ("extreme", extreme)],
        createFastBenchMatrix
          [ ("exponential_1", exponential_1),
            ("exponential_10", exponential_10),
            ("exponential_100", exponential_100),
            ("exponential_1000", exponential_1000)
          ]
          [("long", long), ("extreme", extreme)]
      ]

-- Gives combined benchmark for combinations of operators and input lists
createBenchMatrix :: (Num a) => [(String, a -> a -> IO (Async a))] -> [(String, [a])] -> Benchmark
createBenchMatrix operators input_lists = bgroup "benchmarks" $ map (\(name, operator) -> bgroup name $ map (\(list_name, list) -> createBenchGroup list_name operator list) input_lists) operators

createFastBenchMatrix :: (Num a) => [(String, a -> a -> IO (Async a))] -> [(String, [a])] -> Benchmark
createFastBenchMatrix operators input_lists = bgroup "benchmarks" $ map (\(name, operator) -> bgroup name $ map (\(list_name, list) -> createBenchGroupFast list_name operator list) input_lists) operators

-- Helper function to create a benchmark group for a given reducer (always with DelayedPlus),
-- tests the 4 variants of the reducer with on the same input list
-- Assumes 0 is identity element for the operator, though correctness of answer does not matter for us now anyways.
createBenchGroup :: (Num a) => String -> (a -> a -> IO (Async a)) -> [a] -> Benchmark
createBenchGroup name operator input_list =
  bgroup
    name
    [ bench "delayedTreeReduce" $ whnfIO (delayedTreeReduce 0 operator input_list),
      bench "-delayedReduce" $ whnfIO (delayedReduce 0 operator input_list),
      bench "delayedFoldLReduce" $ whnfIO (delayedFoldLReduce 0 operator input_list),
      bench "delayedFoldRReduce" $ whnfIO (delayedFoldRReduce 0 operator input_list)
    ]

-- Like createBenchGroup, but only does the delayedTreeReduce and delayedReduce benchmarks as these are the most interesting
createBenchGroupFast :: (Num a) => String -> (a -> a -> IO (Async a)) -> [a] -> Benchmark
createBenchGroupFast name operator input_list =
  bgroup
    name
    [ bench "delayedTreeReduce" $ whnfIO (delayedTreeReduce 0 operator input_list),
      bench "delayedReduce" $ whnfIO (delayedReduce 0 operator input_list)
    ]

-- Generates a random list of n integers of some length, where all integers are in the range [lo, hi]
generateRandomList :: Int -> Int -> Int -> IO [Int]
generateRandomList lo hi n = Control.Monad.replicateM n (randomRIO (lo, hi))

-- Simple wrapper for wrapDelay that uses + as the operator
wrapPlus ::
  -- \| Function to generate a random delay
  (Num a) =>
  (() -> IO DelayUS) ->
  -- | Wrapped operator
  (a -> a -> IO (Async a))
wrapPlus gen = wrapDelay gen (+)
