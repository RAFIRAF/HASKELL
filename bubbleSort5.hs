bubbleSort :: (Ord a) => [a] -> [a]
bubbleSort xs = bubbleSort' (length xs) xs

bubbleSortIter :: (Ord a) => [a] -> [a]
bubbleSortIter [] = []
bubbleSortIter [x] = [x]
bubbleSortIter (x:y:t) = if x <= y then x:bubbleSortIter (y:t) else y:bubbleSortIter (x:t)

bubbleSort' :: (Ord a) => Int -> [a] -> [a]
bubbleSort' n xs | n <= 1 = xs
bubbleSort' n xs = bubbleSort' (n-1) (bubbleSortIter xs)