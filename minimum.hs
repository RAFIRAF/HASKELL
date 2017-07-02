minimum' :: (Ord a) => [a] -> a
minimum' [x] = x
minimum' (x:xs) = if x <= minimum' xs then x else minimum' xs