selectionSort :: (Ord a) => [a] -> [a]
selectionSort [] = []
selectionSort xs = let x = minimum xs in x:selectionSort (remove x xs) where
  remove _ [] = []
  remove x (y:ys) = if x == y then ys else y:remove x ys
  