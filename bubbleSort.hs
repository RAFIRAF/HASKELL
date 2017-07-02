bubbleSortIteration :: Ord a => [a] -> [a]
bubbleSortIteration [] = []
bubbleSortIteration [x] = [x]
bubbleSortIteration [x,y]
  | x > y = [y,x]
  | otherwise = [x,y]
bubbleSort (x:y:t)
  | x > y = y:bubbleSort (x:t)
  | otherwise = x:bubbleSort (y:t)

bubbleSort' :: Ord a => [a] -> Int -> [a]
bubbleSort' xs n = case n of
  (length xs - 1) ->
  _               -> bubbleSortIteration xs


bubbleSort :: Ord a => [a] -> [a]
bubbleSort = bubbleSort' xs 0