bsort :: (Ord a) => [a] -> [a]
bsort xs = bsort'' xs (length xs - 1)

bsort'' :: (Ord a) => [a] -> Int -> [a]
bsort'' xs 0 = xs
bsort'' xs n = bsort'' (bsort' xs) (n-1)

bsort' :: (Ord a) => [a] -> [a]
bsort' (x:y:t)
  | x > y = y:bsort' (x:t)
  | x <= y = x:bsort' (y:t)
bsort' [x,y] = if x > y then [x,y] else [y,x]  
bsort' [x] = [x]
bsort' [] = []