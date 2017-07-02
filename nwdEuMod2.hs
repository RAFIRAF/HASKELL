nwd :: Integer -> Integer -> Integer
nwd 0 y = y
nwd x 0 = x
nwd x y
  | x >= y = if x`mod`y>y then nwd (x`mod`y) y else nwd y (x`mod`y)
  | x < y  = nwd y x