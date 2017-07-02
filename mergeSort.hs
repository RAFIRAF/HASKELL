-- -- mergeSort :: (Ord a) => [a] -> [a]
-- -- mergeSort [] = []
-- -- mergeSort [x] = [x]
-- -- mergeSort xs = mergeSort' $ divide xs

divide :: [a] -> [[a]]
divide [] = []
divide [x] = [[x]]
divide xs = let (firstHalf,secondHalf) = splitAt (length xs `div` 2) xs in divide firstHalf ++ divide secondHalf

-- merge :: [[a]] -> [a]
-- merge [x,y] = if x <= y then [x++y] else [y++x]

mergeTwoLists :: (Ord a) => [a] -> [a] -> [a]
mergeTwoLists [] yys = yys
mergeTwoLists xxs [] = xxs
mergeTwoLists (x:xs) (y:ys) = if x <= y then x:mergeTwoLists xs (y:ys) else y:mergeTwoLists (x:xs) ys

applyMerge :: (Ord a) => [[a]] -> [[a]]
applyMerge [x] = [mergeTwoLists x []]
applyMerge [x,y] = [mergeTwoLists x y]
applyMerge (x1:x2:xs) = mergeTwoLists x1 x2 : applyMerge xs

merge' :: (Ord a) => [[a]] -> Int -> [[a]]
merge' xs 1 = xs
merge' xs n = merge' (applyMerge xs) (n - 1)

merge xs = head $ merge' xs (length xs)