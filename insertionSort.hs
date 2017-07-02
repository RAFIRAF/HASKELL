-- insertionSort :: (Ord a) => [a] + [a]
-- insertionSort xs = 
insertionSort :: (Ord a) => [a] -> [a]
insertionSort xs = insertionSort' (length xs) xs

insertionSort' :: (Ord a) => Int -> [a] -> [a]
insertionSort' n xs | n <= 1 = xs
insertionSort' n (x:xs) = insert x (insertionSort' (n-1) xs)

-- insertionSortIter :: (Ord a) => [a] -> [a] -> [a]
-- insertionSortIter xs ys = insertToSorted (head xs) ys

insert :: (Ord a) => a -> [a] -> [a]
insert x [] = [x]
insert x [y] = if x <= y then [x,y] else [y,x]
insert x (y:ys) = if x <= y then x:y:ys else y:insert x ys
