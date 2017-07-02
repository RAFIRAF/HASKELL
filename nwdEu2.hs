nwdEu2 :: Integer -> Integer -> Integer
nwdEu2 x y
  | x > y = if x-y>y then nwdEu2 (x-y) y else nwdEu2 y (x-y)
  | x == y = x
  | x < y = nwdEu2 y x