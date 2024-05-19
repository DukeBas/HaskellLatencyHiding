module Main where
-- Testing and benchmarking

import Hiding
import Delay

-- Simple testing code
main = do
  putStrLn "todo"

-- Example operator that uses delay
delayedPlus :: Int -> Int -> DelayMonad Int
delayedPlus a b = wrapDelay (+) a b