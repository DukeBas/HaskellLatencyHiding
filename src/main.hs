module Main where

-- Testing and benchmarking

import Delay (DelayUS, Delayed, wrapDelay)
import Hiding
import System.Random.MWC

main :: IO ()
main = do
  result <- delayedTreeReduce 0 delayedPlus [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15]
  putStrLn "Result: "
  print result

-- Example operator, the + with a uniform random delay between 0 and 100 microseconds
delayedPlus :: Int -> Int -> Delayed Int
delayedPlus = wrapDelay uniformDelay (+)

-- TODOs:
-- benchmark with http://www.serpentine.com/criterion/tutorial.html#be-careful-with-lazy-io
-- Add Readme with instructions on how to use

-- | Generate a random delay between 0.5 and 1 second
uniformDelay :: () -> IO DelayUS
uniformDelay _ = do
  gen <- createSystemRandom
  uniformR (toS 0.5, toS 1) gen

-- | Converts seconds to microseconds 
toS :: Double -> DelayUS
toS = floor . (* 1000000)