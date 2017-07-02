import Data.List

bubbleSort :: (Ord a) => [a] -> [a]
bubbleSort xs = bubbleSort' (genericLength xs) xs

bubbleSort' :: (Ord a) => Integer -> [a] -> [a]
bubbleSort' n xs | n <= 2 = bubbleSortIter xs
bubbleSort' n xs = bubbleSort' (n-1) (bubbleSortIter xs)

bubbleSortIter :: (Ord a) => [a] -> [a]
bubbleSortIter [] = []
bubbleSortIter [x] = [x]
bubbleSortIter (x:y:t) = if x <= y then x:bubbleSortIter (y:t) else y:bubbleSortIter (x:t)