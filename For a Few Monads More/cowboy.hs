import Data.Monoid

type Food = String
type Price = Sum Int

addDrink :: Food -> (Food,Price)
addDrink "beans" = ("Milk", Sum 25)
addDrink "jerky" = ("whiskey", Sum 99)
addDrink _ = ("beer", Sum 30)

applyLog :: Monoid m => (a,m)->(a->(b,m))->(b,m)
applyLog (x,z) f = let (y,z')=f x in (y,z`mappend`z')