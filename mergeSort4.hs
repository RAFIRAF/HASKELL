mergeSort :: (Ord a) => [a] -> [a]
mergeSort = merge . divide

divide :: [a] -> [[a]]
divide = map (\x -> [x])

merge :: (Ord a) => [[a]] -> [a]
merge xs = concat $ merge' (length xs) xs

merge' :: (Ord a) => Int -> [[a]] -> [[a]]
merge' n xs | n <= 1 = xs
merge' n xs = merge' (n-1) (mergeIter xs)

mergeIter :: (Ord a) => [[a]] -> [[a]]
mergeIter [] = []
mergeIter [x] = [x]
mergeIter (x:y:t) = mergeTwo x y : mergeIter t

mergeTwo :: (Ord a) => [a] -> [a] -> [a]
mergeTwo x [] = x
mergeTwo [] y = y
mergeTwo (x:xs) (y:ys) = if x <= y then x:mergeTwo xs (y:ys) else y:mergeTwo (x:xs) ys