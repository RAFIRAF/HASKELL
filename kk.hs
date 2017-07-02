import Data.Tree
import Data.Char
foo3 :: (Ord a, Num a) => [a] -> [a]
foo3 = filter (==0)

foo4 v1 v2 = v1 * v2

foo5 [] = ([], [])
foo5 ((x1, x2):xs) = (x1:ys, x2:zs)
  where (ys, zs)  = foo5 xs

pobierz3napisy = do
  napis1 <- getLine
  napis2 <- getLine
  napis3 <- getLine
  print $ reverse napis3
  print $ reverse napis2
  print $ reverse napis1    

filter' :: (a->Bool)->[a]->[a]
filter' _ [] = []
filter' pred (x:xs)
  | pred x = x : filter' pred xs
  | otherwise = filter' pred xs

repl :: (Integral a) => a -> b -> [b]
repl a b
  | a == 0 = []
  | a > 0  = b : repl (a - 1) b

flip' f x y = f y x  

zamieniajNaDuze = do
  s <- getLine
  if s /= ""  
    then do
      print . map toUpper $ s
      zamieniajNaDuze
    else return ()

zipWith' :: (a->b->c)->[a]->[b]->[c]
zipWith' _ [] _ = []
zipWith' _ _ [] = []
zipWith' f (x:xs) (y:ys) = 
  f x y : zipWith' f xs ys

wypiszCoDrugi = do   
  s <- getLine
  p <- getLine
  print s
  wypiszCoDrugi  

dwaW :: [[t]] -> [t]
dwaW = concat


--getN (Node l v r) n
--  | n < 1
--   n == 1 = L v
--   to = Node (getN l (n-1)) v (getN r (n-1))

wypiszCoDrugi2 = do
  s <- getLine
  if s /= ""
    then do
      r <- getLine
      if r /= ""
        then do
          print r
          wypiszCoDrugi2
        else return ()
    else return ()

--instance Monad Either where
--  return = Right
--  Right m >>= k = k m
--  Left e  >>= _ = Left e   

drzewoWListe :: Tree a -> [a]
drzewoWListe (Node x xs) = x:(concat $ map drzewoWListe xs)

--drzewoWListe2 Empty = []
--drzewoWListe2 (Node x children) = [x]++(concatMap drzewoWListe2 children)

--filter :: (a->Bool) -> [a] -> [a]
--zip :: [a]->[b]->[(a,b)]

{-PRAWA MONAD
1) (m >>= f) >>= g == m >>= (\x -> f x >>= g)
2) return x >>= f == f x
3) m >>= return == m-}

--(=<<) :: Monad m => (a -> m b) -> m a -> m b
--f =<< x = x >>= f

zamieniajNaDuze2 = do
  s <- getLine
  if s /= ""
    then do
      print . map toUpper $ s
      zamieniajNaDuze2
    else return ()

--instance Monad Maybe where
--  (Just a) >>= f = f a
--  Nothing >>= f = Nothing
--  return a = Just a    
--  fail _ = Nothing

--data Tree = L a | N (Tree a) (Tree a)
--preOrder :: Tree a -> [a]
--preOrder L a = [a]
--preOrder (N l a r) = [a] ++ (preOrder l) ++ (preOrder r)

-- PRAWA MONAD DLA MONADY IDENTITY
--instance Monad Identity where
--  (Identity a) >>= f = f a
--  return x = Identity x

--1. (Identity x >>= f) >>= g == f x >>= g == Identity x >>= (\x -> f x >>= g)
--2. return x >>= f == Identity x >>= f == f x
--3. Identity x >>= return x == Identity x

null' :: [a] -> Bool
null' x = case x of
  [] -> True
  (_:_) -> False

--(>=>) :: Monad m => (a -> m b) -> (b -> m c) -> a -> m c
--f >=> g = \x -> f x >>= g

--(.) :: (b->c)->(a->b)->a->c
--f . g = \x -> f (g x)
--inxfir . 9

--size N a [] = 1
--size N a children = 1 + map size children

--PRAWA MONAD
--1) (m >>= f) >>= g == m >>= (\x -> f x >>= g) -- łączność nawiasów
--2) return x >>= f == f x -- użycie f na x opakowanym w monadę da to samo co zwykłe użycie f na x
--3) m >>= return == m -- jeżeli opakujemy monadę m, to dostaniemy tą samą monadę

--MONADA IDENTYCZNOŚCIOWA
--instance Monad Identity where
--return a = Identity a
--(Identity x) >>= f = f x

--PRAWA MONAD DLA MONADY IDENTYCZNOŚCIOWEJ
--1) (Identity x >>= f) >>= g == f x >>= g == Identity x >>= (\x -> f x >>= g)
--2) return x >>= f == Identity x >>= f == f x
--3) Identity x >>= return x == Identity x

--MONADA MAYBE
--instance Monad Maybe where
--  Nothing >>= f = Nothing
--  Just a >>= f = f a
--  fail _ = Nothing
--  return a = Just a

--(.) :: (b->c)->(a->b)->a->c
--f . g = \x -> f (g x)
--infixr 9 .  

--instance Monad Either where
--  return = Right
--  Right m >>= k = k m
--  Left e >>= _ = Left e

--drzewoWListe :: Tree a -> [a]
--drzewoWListe E = []
--drzewoWListe (Node x children) = x : concat $ map drzewoWListe children

--preOrder :: Tree a -> [a]
--preOrder L a = [a]
--preOrder (N l a r) = [a] ++ preOrder (l) ++ preOrder (r)

--(>=>) :: Moand m => (a-> m b)->(b -> m c)-> a -> m c
--f >=> g = \x -> f x >>= g

--postOrder :: Tree a -> [a]
--postOrder E = []
--postOrder L a = [a]
--postOrder (N l a r) = (postOrder l) ++ (postOrder r) ++ [a]

--size :: Tree a -> Int
--size N a [] = 1
--size N a children = 1 + sum $ map size children

--genTree a = Node (genTree a) a (genTree a)

--getN (Node l v r) n
--  | n < 1 = error "n to small"
--  | n == 1 = L v
--  | otherwise = Node (getN l (n-1)) v (getN r (n-1))

--instance Monad Problem where
--  return = Ok
--  Error s >>= _ = Error s
--  Ok a >>= f = f a

--instance Monad CosTam where
--  return  a = Ok a
--  Error m i >>= _ = Error m i
--  Ok a >>= f = f a

foo1 :: (b->a->b)->b->[a]->b
foo1 f v [] = v
foo1 f v (x:xs) = foo1 f (f v x) xs

--foo5 :: [(a,b)]->([a],[b])

wypiszDrzewo (Node x xs) = print x >> mapM_ wypiszDrzewo xs
drzewoWListe (Node x xs) = x:(concat $ map drzewoWListe xs)
listaWDrzewo (x:xs) = 
  let dzielNaTrzyListy a = foldr (\a [x,y,z] -> [a:z,x,y]) [[],[],[]]
  in Node  x (map listaWDrzewo $ filter (/=[]) $ dzielNaTrzyListy xs) 