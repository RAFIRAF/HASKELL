import Data.List
{-baby x=x+x
doubleSmallNumber x = if x <= 100
			then 2*x 
			else x-}
boomBangs xs = [if x < 10 then (x,"BOOM!") else (x,"BANG!")|x<-xs,odd x]
data Lists a = List [a] | ListOfLists [Lists a]
flatten :: Lists a -> [a]
flatten (List xs) = xs
flatten (ListOfLists xss) = concatMap flatten xss 
circumference :: Float -> Float  
circumference r = 2 * pi * r  

-- CHAPTER 4 SYNTAX IN FUNCTIONS

lucky :: (Integral a) => a -> String  
lucky 7 = "LUCKY NUMBER SEVEN!"  
lucky x = "Sorry, you're out of luck, pal!"

factorial :: (Integral a) => a -> a
factorial n = product [1..n]

factorialr :: (Integral a) => a -> a
factorialr 0 = 1
factorialr n = n * factorialr (n-1)

addVectors' :: (Num a) => (a, a) -> (a, a) -> (a, a)
addVectors' a b = (fst a + fst b, snd a + snd b)

addVectors :: (Num a) => (a, a) -> (a, a) -> (a, a)
addVectors (x1,y1) (x2,y2) = (x1+x2,y1+y2)

first :: (a, b, c) -> a
first (x,_,_) = x

second :: (a, b, c) -> b
second (_,y,_) = y 

third :: (a, b, c) -> c
third (_,_,z) = z

tell :: (Show a) => [a] -> String 
tell [] = "The list is empty"  
tell (x:[]) = "The list has one element: " ++ show x  
tell (x:y:[]) = "The list has two elements: " ++ show x ++ " and " ++ show y  
tell (x:y:_) = "This list is long. The first two elements are: " ++ show x ++ " and " ++ show y

length'' :: [a] -> Integer
length'' xs = sum [1 | _ <- xs]

-- lengthr :: (Num b) => [a] -> b -- też OK
lengthr :: [a] -> Integer
lengthr [] = 0
lengthr (_:xs) = 1 + lengthr xs

sum' :: (Num a) => [a] -> a
sum' [] = 0
sum' (x:xs) = x + sum xs

capital :: String -> String  
capital "" = "Empty string, whoops!"  
capital s@(x:xs) = "The first letter of " ++ s ++ " is " ++ [x]  

bmiTell :: Double -> Double -> String  
bmiTell w h  
    | bmi <= skinny = "You're underweight, you emo, you!"  
    | bmi <= normal = "You're supposedly normal. Pffft, I bet you're ugly!"  
    | cmi <= 30 = "You're fat! Lose some weight, fatty!"++" "++show cmi
    | otherwise = "You're a whale, congratulations!"
    where bmi = w/h^2; cmi = w/h^2
          [skinny, normal] = [18.5,25]

bmiTell' :: Double -> Double -> String  
bmiTell' w h =
    if w/h^2 <= 18.5 then "You're underweight, you emo, you!"  
    else if w/h^2 <= 25.0 then "You're supposedly normal. Pffft, I bet you're ugly!"  
      else if w/h^2 <= 30.0 then "You're fat! Lose some weight, fatty!"  
        else  "You're a whale, congratulations!"

max' :: (Ord a) => a -> a -> a
max' x y
  | x > y     = x
  | otherwise = y

max'' :: (Ord a) => a -> a -> a
max'' x y =
  if x > y 
    then x
    else y

compare' :: (Ord a) => a -> a -> Ordering
x `compare'` y
  | x > y     = GT
  | x == y    = EQ
  | otherwise = LT

inicjaly :: String -> String  -> String
inicjaly [] nazwisko@(_:_) = error "Nie podano imienia!"
inicjaly imie@(_:_) []     = error "Nie podano nazwiska!"
inicjaly [] []             = error "Nie podano imienia i nazwiska!"
inicjaly imiee nazwiskoo   = [x]++[y]
  where (x:_) = imiee
        (y:_) = nazwiskoo

listaBMI = [(80.0,1.8),(50,1.7),(95,1.9)]
xxs = [[1,3,5,2,3,1,2,4,5],[1,2,3,4,5,6,7,8,9],[1,2,4,2,1,6,3,1,3,2,3,6]]

calcBMIs :: (RealFloat a) =>[(a, a)] -> [a]
calcBMIs [] = error "WTF a gdzie bmi???"
calcBMIs listaBMI = [bmi w h|(w,h)<-listaBMI]
  where bmi weight height = weight/height^2

calcBMIs' :: Fractional a => [(Double,Double)] -> [Double]
calcBMIs' [] = error "BRAK DANHY"
calcBMIs' listaBMI = [bmi w h |(w,h)<-listaBMI]
  where 
    bmi weight height = weight/height^2

cylinder :: (RealFloat a) => a -> a -> a
cylinder r h = 
    let sideArea = 2 * pi * r * h
        topArea  = pi * r^2
    in  sideArea + 2 * topArea

poleKola :: (RealFloat a) => a -> a
poleKola r = 
  let pole = pi * r^2
  in pole

calcBMIsl :: (RealFloat a) => [(a,a)] -> [a]
calcBMIsl bmis = [bmi |(w,h)<-bmis, let bmi = w/h^2]

headc :: [a] -> a
headc xs = case xs of []    -> error "empty list"
                      (x:_) -> x

describeList :: [a] -> String
describeList xs = "The list is " ++ case xs of []  -> "empty."
                                               [x] -> "a singleton list."
                                               xs  -> "a longer list."

-- CHAPTER 5 RECURSION

maximum' :: (Ord a) => [a] -> a
maximum' []     = error "empty list"
maximum' (x:[]) = x
maximum' (x:xs)
  | x > maxTail = x
  | otherwise   = maxTail
  where maxTail = maximum' xs

maximumMax :: (Ord t) => [t] -> t
maximumMax [] = error "empty list"
maximumMax [x] = x
maximumMax (x:xs) = max x (maximumMax xs)

replicate' :: (Num a, Ord a) => a -> t -> [t]
replicate' n x 
  | n <= 0    = []
  | otherwise = x:replicate' (n-1) x

take' :: (Num a, Ord a) => a -> [t] -> [t]
take' n _ | n <= 0    = []
take' _ []            = []
take' n (x:xs)        = x:(take' (n-1) xs)

reverse' :: [t] -> [t]
reverse' [] = []
reverse' (x:xs) = reverse' xs ++ [x]

repeat' :: t -> [t]
repeat' t = [t]++repeat' t

repeat'' :: t -> [t]
repeat'' t = t:repeat' t

replicateTake :: (Integral a) => a -> t -> [t]
replicateTake n x = take' n (repeat' x)

myZip :: [a] -> [b] -> [(a,b)]
myZip [] _ = []
myZip _ [] = []
myZip (x:xs) (y:ys) = (x,y):myZip xs ys

myElem :: (Eq t) => t -> [t] -> Bool
myElem _ [] = False
myElem e (x:xs)
  | e==x      = True
  | otherwise = e `myElem` xs

myQuicksort :: (Ord t) => [t] -> [t]
myQuicksort [] = []
myQuicksort (x:xs) = myQuicksort [a | a <- xs,  a <= x] ++ [x] ++ myQuicksort [a | a <- xs, a > x]

--  let quickSort [] = []; quickSort (x:xs) = let smallerSorted = quickSort $ filter (<=x) xs; biggerSorted = quickSort $ filter (>x) xs in smallerSorted ++ [x] ++ biggerSorted

-- CHAPTER 6 HIGHER ORDER FUNCTIONS

multThree :: (Num a) => a -> a -> a -> a
multThree x y z = x * y * z 

compareWithHundred :: (Num a, Ord a) => a -> Ordering
compareWithHundred = compare 100 

divideByTen :: (RealFloat a) => a -> a
divideByTen = (/10)

isUpperAlphanum :: Char -> Bool
isUpperAlphanum = (`elem` ['A'..'Z'])

isLowerAlphanum :: Char -> Bool
isLowerAlphanum x= not (isUpperAlphanum x)

odejmij4 :: (Num a) => a->a
odejmij4 = subtract 4

applyTwice :: (t -> t) -> t -> t
applyTwice f x = f (f x)

zipWith' :: (t1->t2->t)->[t1]->[t2]->[t]
zipWith' _ [] _ = []
zipWith' _ _ [] = []
zipWith' f (x:xs) (y:ys) = (f x y) :(zipWith' f xs ys)

isEmpty :: [t] -> Bool
isEmpty = null

flip' :: (t1->t2->t)->t2->t1->t
flip' f x y = f y x

map' :: (t1->t2)->([t1]->[t2])
map' _ [] = []
map' f (x:xs) = f x : map' f xs

filter' :: (t->Bool)->[t]->[t]
filter' _ [] = []
filter' p (x:xs)
  | p x       = x:filter' p xs
  | otherwise =   filter' p xs 

quicksort' :: (Ord t) => [t] -> [t]
quicksort' [] = []
quicksort' (x:xs) = smallerSorted ++ [x] ++ biggerSorted
  where
    smallerSorted = quicksort' (filter (<=x) xs)
    biggerSorted  = quicksort' (filter ( >x) xs)

largestDivisible ::  Integral a => a
largestDivisible = headc (filter p {-(\n-> n `mod` 3829==0)-} [100000,99999..])
  where
    p x = x `mod` 3829 == 0

takeWhile' :: (t->Bool)->[t]->[t]
takeWhile' _ [] = []
takeWhile' p (x:xs)
  | p x       = x:takeWhile' p xs
  | otherwise = [] 

collatz :: Integer -> [Integer]
collatz x = 
   case x of 
    1    -> 1:[]
    n
      | even n -> x:collatz (x `div` 2)
      | odd  n -> x:collatz (3*x+1)

collatz' :: Integer -> [Integer]
collatz' x 
    | x==1          = 1:[]
    | even x     = x:collatz' (x `div` 2)
    | otherwise  = x:collatz' (3*x+1)

--collatzif :: (Integral a) => a -> [a]
--collatzif x = 
--  if (x==1) 
--    then x:[] 
--    else 
--      if (even x) 
--        then x:collatzif (x`div`2)
--         else x:collatzif (3*x+1)

collatzif :: (Integral a) => a -> [a]
collatzif x = if x==1 then x:[] else if even x then x:collatzif (x`div`2) else x:collatzif (3*x+1)

collatza :: (Integral a) => a -> [a]
collatza 1 = [1]
collatza x
  | even x = x:collatza (x `div` 2)
  | odd  x = x:collatza (3*x+1)

--numLongChains :: Int  
--numLongChains = length (filter isLong (map chain [1..100]))      
--  where isLong xs = length xs > 15  

--length (filter (>15) (map length (map collatz [1..100])))
numLongChains :: Int
numLongChains = length (filter isLong (map collatza [1..100]))
  where isLong xs = length xs > 15

numLongChains' :: (Num a) => a
numLongChains' = 
  let
    longer x = length x > 15
  in
    fromIntegral(length (filter longer(map collatz [1..100])))

numLongChains'' :: Int
numLongChains'' = length (filter (\n->length n > 15) (map collatza [1..100]))

addThree ::(Num a)=>a->a->a->a
addThree = \x-> \y-> \z-> x+y+z

flip'' :: (t1->t2->t)->t2->t1->t
flip'' f = \x y -> f y x

sum'' :: (Num a) => [a] -> a
sum'' xs = foldl (\acc x -> acc + x) 0 xs

sumf :: (Num a) => [a] -> a
sumf = foldl (+) 0

elemf :: (Eq t) => t -> [t] -> Bool
--elemf _ [] = False
--elemf e (x:xs)
--  | e == x    = True
--  | otherwise = elemf e xs
elemf e es = foldl (\acc x -> if x == e then True else acc) False es

suma :: (Num a) => [a] -> a
suma = foldl1 (+)

nalezyDo :: (Eq t) => t -> [t] -> Bool
nalezyDo e = foldr (\x acc -> if x == e then True else acc) False

mapf :: (Foldable t) => (a->b) -> t a -> [b]
mapf f = foldr (\x acc -> f x : acc) [] 

mapfl :: (t1->t2)->[t1]->[t2]
mapfl f = foldl (\acc x -> acc++[f x]) []

maksimum :: (Ord a) => [a] -> a
maksimum []  = error "Pusta lista"
maksimum [x] = x
maksimum (x:xs)
  | x >= maxTail = x
  | otherwise        = maxTail
  where maxTail = maksimum xs 


maksimumif :: (Ord a) => [a] -> a
maksimumif []  = error "Pusta lista"
maksimumif [x] = x
maksimumif (x:xs) = let maxTail = maksimumif xs in 
  if x > maxTail
    then x
    else maxTail

maximumf :: (Ord a, Foldable t) => t a -> a
maximumf = foldr1 (\x acc -> if x>acc then x else acc)

reversef :: (Foldable t) => t a -> [a]
reversef = foldl (\ acc x -> x:acc) []

reversefr :: (Foldable t) => t a -> [a]
reversefr = foldr (\x acc -> acc++[x]) []

--reversef2 :: (Foldable t) => t a -> [a]
--reversef2 = foldr1 (\ x acc -> [x]++acc)

productf :: (Num a, Foldable t) => t a -> a
productf = foldr1 (*)

filtruj :: (t->Bool)->[t]->[t]
filtruj _ [] = []
filtruj p (x:xs) 
  | p x       = x:filtruj p xs
  | otherwise =   filtruj p xs


filterf :: (Foldable t) =>  (a->Bool)->t a ->[a]
filterf p  = foldr (\x acc -> if (p x) then x:acc else acc) []

glowa :: (Foldable t) => t a -> a
glowa = foldr1 (\x _ -> x)

ostatni :: (Foldable t) => t a -> a
ostatni = foldl1 (\_ x -> x)

glowka :: [a]->a
glowka [] = error "glowka w mur"
glowka (x:_)= x

odwrocListe :: [t]->[t]
odwrocListe [] = []
odwrocListe (x:xs) = odwrocListe xs ++ [x]

revfol :: (Foldable t) => t a -> [a]
revfol = foldl (flip (:)) []

--($) :: (a->b)->a->b
--f $ x = f x

--map ($3) $ map (*) [1..10]
--map ($2)[(*3),(+3),(/3),(^2),sqrt]

--(.) :: (b->c)->(a->b)->a->c
--f . g = \x -> f (g x)

--(.) :: (b->c)->(a->b)->a->c
--f . g = \x -> f (g x)

--numUniques :: (Eq a) => [a] -> Int
--numUniques = \xs -> length $ nub xs

--import qualified Data.List as concatMap

--- CHAPTER 8 MAKING OUR OWN TYPES AND TYPECLASSES ---

data Shape = Circle Point Float | Rectangle Point Point deriving Show
data Point = Point Float Float deriving Show
surface :: Shape -> Float
surface (Circle _ r) = pi * r^2
surface (Rectangle (Point x1 y1) (Point x2 y2)) = (abs $ x2-x1)*(abs $ y2-y1)

nudge :: Float -> Float -> Shape -> Shape
nudge x y (Circle (Point x0 y0) r) = (Circle (Point (x0+x) (y0+y)) r)
nudge x y (Rectangle (Point x1 y1) (Point x2 y2)) = (Rectangle (Point (x1+x) $y1+y) $Point (x2+x) $y2+y) 

baseCircle :: Float -> Shape
baseCircle r = Circle (Point 0 0) r

baseRect :: Float -> Float -> Shape
baseRect w h = Rectangle (Point 0 0) $ Point (w) h

--data Person = Person String String Int Float String String deriving (Show)   
data Person = Person { firstName    :: String
                     , lastName     :: String
                     , age          :: Int
                     , height       :: Float
                     , phoneNumber  :: String
                     , flavor       :: String
                     } deriving Show

data Car = Car {company :: String, model :: String, year :: Int} deriving (Show)  
                     




























      