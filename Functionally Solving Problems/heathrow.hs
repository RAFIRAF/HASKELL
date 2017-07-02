import Data.List
data Section = Section {getA :: Int, getB :: Int, getC :: Int} deriving Show
type RoadSystem = [Section]
data Label = A | B | C deriving Show
type Path = [(Label, Int)]

heathrowToLondon :: RoadSystem
heathrowToLondon = [Section 50 10 30, Section 5 90 20, Section 40 2 25, Section 10 8 0]

groupsOf :: Int -> [a] -> [[a]]
groupsOf 0 _  = []
groupsOf n [] = []
groupsOf n xs  =  take n xs : groupsOf n ys
                  where ys = drop n xs

main = do
   contents <-getContents
   let c = words contents
       roads = map read c :: [Int]
       threes = (groupsOf 3) roads
       rs = map (\[a,b,c] -> Section a b c) threes
   putStr "The best path is "
   putStr $ intercalate " -> " $ map show $ map fst $ optimalPath rs
   putStr $ ". It takes " ++ (show $ sum $ map snd $optimalPath rs) ++ " units."
   


roadStep :: (Path,Path)->Section->(Path,Path)
roadStep (pathA,pathB) (Section a b c) =
  let
    sumA = sum $ map snd pathA
    sumB = sum $ map snd pathB
    forwardA = sumA + a
    forwardB = sumB + b
    crossA = sumB + b + c
    crossB = sumA + a + c
    newPathA = if forwardA <= crossA then (A,a):pathA else (C,c):(B,b):pathB
    newPathB = if forwardB <= crossB then (B,b):pathB else (C,c):(A,a):pathA
  in
    (newPathA, newPathB)

optimalPath :: RoadSystem -> Path
optimalPath rs = 
  let
    (pA, pB) = foldl roadStep ([],[]) rs
  in
    if (sum $ map snd pA) < (sum $ map snd pB) then reverse pA else reverse pB

concatMap' :: ([a]->b)->[a]->b
concatMap' f = concat . (map f)
