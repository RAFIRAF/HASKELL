import Data.List
selectionSortMax :: (Ord a) => [a] -> [a]
selectionSortMax [] = []
selectionSortMax xs = let x = maximum xs in selectionSortMax (delete x xs) ++ [x]