bubbleSort :: (Ord a) => [a] -> [a]
bubbleSort xs = bubbleSort' (length xs) xs

bubbleSort' :: (Ord a) => Int -> [a] -> [a]
bubbleSort' n xs | n <= 1 = xs
bubbleSort' n xs = bubbleSort' (n-1) (bubbleSortIter xs)

bubbleSortIter :: (Ord a) => [a] -> [a]
bubbleSortIter [] = []
bubbleSortIter [x] = [x]
bubbleSortIter (x:y:zs) = if x <= y then x:bubbleSortIter (y:zs) else y:bubbleSortIter (x:zs)