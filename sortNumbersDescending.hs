import Data.List

sortNumbersDescending :: [Int] -> [Int]
sortNumbersDescending [] = []
sortNumbersDescending xs = {-reverseR $ -} sortR xs where
  sortR = quicksort where
    quicksort [] = []
    quicksort (x:xs) = quicksort [y | y<-xs,y>=x ] ++ [x] ++ quicksort [z | z <- xs, z<x]
  -- reverseR [] = []
  -- reverseR (x:xs) = reverseR xs++[x]