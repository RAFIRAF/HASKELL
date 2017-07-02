foldQs :: Ord a => [a] -> [a]
foldQs [] = []
foldQs (x:xs) = foldQs (foldFilter (<=x) xs) ++ [x] ++ foldQs (foldFilter (>x) xs)
--foldQs (x:xs) = foldr ()

foldFilter p = foldr (\x acc -> if p x then x : acc else acc) []
foldlFilter p = foldl (\acc x -> if p x then acc++[x] else acc) []

