import MyTree
--class Eq a where
--  (==) :: a -> a -> Bool
--  (/=) :: a -> a -> Bool
--  x == y = not (x /= y)
--  x /= y = not (x == y)

data TrafficLight = Red | Yellow | Green  

instance Eq TrafficLight where
  Red == Red = True
  Yellow == Yellow = True
  Green == Green = True
  _ == _ = False

instance Show TrafficLight where
  show Red = "Red Light"
  show Yellow = "Yellow Light"
  show Green = "Green Light"

--instance Eq m => Eq (Maybe m) where
--  Just x == Just y = x == y
--  Nothing == Nothing = True
--  _ == _ = False

class YesNo a where
  yesno :: a -> Bool

instance YesNo Int where
  yesno 0 = False
  yesno _ = True

instance YesNo [a] where
  yesno [] = False
  yesno _ = True

instance YesNo Bool where
  yesno = id

instance YesNo (Maybe a) where
  yesno Nothing = False
  yesno (Just _) = True

instance YesNo (Tree a) where
  yesno EmptyTree = False
  yesno (Node _ _ _) = True

instance YesNo TrafficLight where
  yesno Red = False
  yesno Green = True  

yesnoIf :: (YesNo y) => y -> a -> a -> a
yesnoIf yesnoVal yesResult noResult = if yesno yesnoVal then yesResult else noResult

instance Functor Tree where
  fmap _ EmptyTree = EmptyTree
  fmap f (Node x l r) = Node (f x) (fmap f l) (fmap f r)

--instance Functor (Either a) where
--  fmap f (Right x) = Right (f x)
--  fmap f (Left x)  = Left x

--class Functor f where
--  fmap :: (a->b) -> f a -> f b

class Tofu t where
  tofu :: j a -> t a j

data Frank a b = Frank {frankField :: b a} deriving