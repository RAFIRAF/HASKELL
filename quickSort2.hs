quickSort :: (Ord a) => [a] -> [a]
quickSort [] = []
quickSort (x:xs) = quickSort [a | a <- xs, a <= x] ++ [x] ++ quickSort [b | b <- xs, b > x]

-- quickSort [3,2,1] ++ [4] ++ [] ++ [5] ++ []
-- quickSort [2,1] ++ [3] ++ [] ++ [4] ++ []
-- quickSort [1] ++ [2] ++ [] ++ [3] ++ [4] ++ []
-- [] ++ [1] ++ [] ++ [2] ++ [] ++ [3] ++ [4] ++ []