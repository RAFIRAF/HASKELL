insertionSort :: Ord a => [a] -> [a]
insertionSort [] = []
insertionSort (x:xs) = insert x (insertionSort xs) where
  insert x [] = [x]
  insert x (y:ys)
    | x <= y = x:y:ys
    | otherwise = y:insert x ys