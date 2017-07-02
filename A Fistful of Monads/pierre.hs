type Birds = Int
type Pole = (Int, Int)

landLeft :: Int -> Pole -> Maybe Pole
landLeft n (l,r)
  | abs (r-(l+n)) > 2 = Nothing
  | otherwise = Just (l+n,r)

landRight :: Int -> Pole -> Maybe Pole
landRight n (l,r)
  | abs (r+n-l) > 2 = Nothing
  | otherwise = Just (l,r+n)

x -: f = f x

banana :: Pole -> Maybe Pole
banana _ = Nothing