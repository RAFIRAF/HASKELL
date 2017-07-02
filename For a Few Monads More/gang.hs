isBigGang :: Int -> (Bool,String)
isBigGang x = (x > 9, "Compared gang size to 9.")

applyGang :: Monoid m => (a,m)->(a->(b,m))->(b,m)
applyGang (x,s) f = let (y,s')=f x in (y,s`mappend`s')