import Data.List

selsort :: (Ord a) => [a] -> [a]
selsort [] = []
selsort [x] = [x]
selsort [x,y] = if x <= y then [x,y] else [y,x]
selsort (x:xs) = let min  = minimum xs in 
  if x <= min then x : selsort xs else min : selsort (delete min (x:xs)) 
