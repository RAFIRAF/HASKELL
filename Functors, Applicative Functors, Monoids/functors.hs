class Functor f where
  fmap :: (a -> b) -> f a -> f b

instance Functor [] where
  fmap = map

instance Functor Maybe where
  fmap f (Just x) = Just (f x)
  fmap f Nothing = Nothing

instance Functor Tree where
  fmap f Empty = Empty  
  fmap f (Node x l r) = Node (f x) (fmap f l) (fmap f r)
  
instance Functor (Either a) where
  fmap f (Right x) = Right (f x)
  fmap f (Left x) = Left x

instance Functor (Map k) where
  fmap f (fromList []) = fromList []
  fmap f (fromList [(k,v)]) = fromList [(k,f v)]

instance Ord k => Functor (Map k) where
   fmap f m = fromList (map modify (toList m))
         where modify (k,v) = (k, f v)

instance Ord k => Functor (Map k) where
   fmap f = fromList . map (\(k,v) -> (k,f v)) . toList

newtype MyMap k v = MyMap { unMyMap :: Map k v }

instance Functor (MyMap k) where
   fmap f = MyMap . fromList . map (\(k,v) -> (k,f v)) . toList . unMyMap

instance Functor IO where   
  fmap f action = do
    result <- action
    return (f result)

instance Functor ((->) r) where
  fmap f g = (\x -> f (g x))

instance Functor ((->) r) where
  fmap = (.)  

class Functor f where
  fmap :: (a->b)-> f a-> f b

instance Functor [] where
  fmap = map

instance Functor Maybe where
  fmap f (Just x) = Just (f x)
  fmap f Nothing = Nothing

instance Functor MyTree where
  fmap f EmptyTree = EmptyTree
  fmap (Node x l r) = Node (f x) (fmap f l) (fmap f r)

instance Functo (Either a) where
  fmap f (Right x) = Right (f x)
  fmap f (Left x ) = Left x

data Either a b:v = Left a | Right b
























