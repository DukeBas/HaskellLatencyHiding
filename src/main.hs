module Main where

-- Testing and benchmarking

import Delay (DelayMonad, wrapDelay)
import Hiding (constantDelayFoldRReduce)

main :: IO ()
main = do
  result <- constantDelayFoldRReduce 0 delayedPlus 1000000 [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15]
  putStrLn "Result: "
  print result

-- Example operator that uses delay
delayedPlus :: Int -> Int -> DelayMonad Int
delayedPlus = wrapDelay (+)

-- benchmark with http://www.serpentine.com/criterion/tutorial.html#be-careful-with-lazy-io