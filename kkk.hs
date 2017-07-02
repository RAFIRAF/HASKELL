fifi :: (Num a, Ord a) => (a->Bool) -> [a] -> [a]
fifi = filter (\x -> x + 2 == 3)