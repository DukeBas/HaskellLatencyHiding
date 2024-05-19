module Main where
-- Testing and benchmarking

import Hiding

-- Simple testing code
main = do
  let list = [1 .. 1000] :: [Int]
  let result = reduce 0 (+) list
  putStrLn $ "Result: " ++ show result
  return ()