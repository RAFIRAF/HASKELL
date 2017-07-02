import Control.Monad
type Pole = (Int,Int)

landLeft :: Int -> Pole -> Maybe Pole
landLeft n (l,r) = if (abs (l+n-r)) < 4 then Just (l+n,r) else Nothing

landRight :: Int -> Pole -> Maybe Pole
landRight n (l,r) = if abs (r+n-l) < 4 then Just (l,r+n) else Nothing

routine :: Maybe Pole
routine = do
  a <- return (0,0)
  b <- landRight 3 a
  _ <- Nothing
  landLeft 6 b


routine0 :: Maybe Pole
routine0 = 
  case landLeft 3 (0,0) of
    Nothing -> Nothing
    Just x -> case landRight 6 x of
      Nothing -> Nothing
      Just x -> case landLeft 5 x of
        Nothing -> Nothing
        Just x -> Just x

wordsCount =
    putStr "Write a line and press [enter]:\n" >>
    getLine >>= \k -> return $ length . words $ k

cosTam :: [(Int,Char)]
cosTam = do
  a <- [1,2]
  b <- ['a','b']
  return (a,b)

--instance Monad [] where
--  return x = [x]
--  xs >>= f = concat (map f xs)
--  fail _ = []

sevensOnly :: [Int]
sevensOnly = do
  x <- [1..50]
  guard ('7'`elem`show x)
  return x