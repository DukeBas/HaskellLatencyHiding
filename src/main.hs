module Main where

-- Testing and benchmarking

import Delay (constantDelay, wrapDelay)
import Hiding (delayedTreeReduce)
import Control.Concurrent.Async

main :: IO ()
main = do
  result <- delayedTreeReduce 0 delayedPlus [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15]
  putStrLn "Result: "
  print result

-- Example operator, the + with a uniform random delay between 0 and 100 microseconds
delayedPlus :: Int -> Int -> IO(Async Int)
delayedPlus = wrapDelay (constantDelay 1) (+)

-- TODOs:
-- benchmark with http://www.serpentine.com/criterion/tutorial.html#be-careful-with-lazy-io
-- Add Readme with instructions on how to use
