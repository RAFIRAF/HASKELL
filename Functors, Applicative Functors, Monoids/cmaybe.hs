data CMaybe a = CNothing | CJust Int a deriving Show
instance Functor CMaybe where
	fmap f CNothing = CNothing
	fmap f (CJust x y) = CJust (succ x) (f y)