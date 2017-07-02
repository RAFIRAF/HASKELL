bubbleSort :: (Ord a) => [a] -> [a]
bubbleSort xs = bubbleSort' (length xs) xs where
  bubbleSort' n xs | n <= 2 = bubbleSortIteration xs
  bubbleSort' n xs = bubbleSort' (n-1) (bubbleSortIteration xs)
  bubbleSortIteration [] = []
  bubbleSortIteration [x] = [x]
  bubbleSortIteration (x:y:t) = if x <= y then x:bubbleSortIteration (y:t) else y:bubbleSortIteration (x:t)