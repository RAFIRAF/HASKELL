import Data.List
selsort :: (Ord a) => [a] -> [a]
selsort [] = []
selsort xs = let x = maximum xs in selsort (delete x xs) ++ [x]