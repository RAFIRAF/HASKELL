mergeSort :: (Ord a) => [a] -> [a]
mergeSort xs = merge . divide $ xs

divide :: [a] -> [[a]]
divide = map (\x -> [x])

mergeTwo :: (Ord a) => [a] -> [a] -> [a]
mergeTwo [] ys = ys
mergeTwo xs [] = xs
mergeTwo (x:xs) (y:ys) = if x <= y then x:mergeTwo xs (y:ys) else y:mergeTwo (x:xs) ys

mergeIter :: (Ord a) => [[a]] -> [[a]]
mergeIter [] = []
mergeIter [x] = [x]
mergeIter (x:y:t) = mergeTwo x y : mergeIter t

merge' :: (Ord a) => Int -> [[a]] -> [[a]]
merge' n xs | n <= 2 = mergeIter xs
merge' n xs = merge' (n-1) (mergeIter xs)

merge :: (Ord a) => [[a]] -> [a]
merge xs = concat $ merge' (length xs) xs