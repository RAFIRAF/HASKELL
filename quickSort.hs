quickSort :: (Ord a) => [a] -> [a]
quickSort [] = []
quickSort [x] = [x]
quickSort [x,y] = if x > y then [y,x] else [x,y]
quickSort (x:xs) = quickSort (filter (<=x) xs) ++ [x] ++ quickSort $ (filter > x) xs