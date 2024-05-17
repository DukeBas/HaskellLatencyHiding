-- Communication efficient reduce, where the operation done has some inherent latency (for example due to network communication)

-- Takes a base element, which is only returned for the empty list, an associative operation and a list of input elements
reduce :: a -> (a -> a -> a) -> [a] -> a
reduce base op list = r list []
  where
    r (x1 : x2 : xs) o = r xs ((x1 op x2) : o) -- Main case, combine two elements --TODO force strictness to make operation latency take place
    r (x1 : []) o = r [] (x1 : o) -- Odd number of elements, push the last one as is
    r [] [] = base -- No input, return base
    r [] [x] = x -- Computation finished, return the result
    r [] xs = r xs [] -- Recurse on the output list to combine the results again
