import Data.List

strangeSort :: [Int] -> [Int]
strangeSort [] = []
strangeSort xs = sort oddXs ++ reverse (sort evenXs) where
  oddXs = [x | x<-xs, odd x]
  evenXs = xs \\ oddXs