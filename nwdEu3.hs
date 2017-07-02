nwd :: Integer -> Integer -> Integer
nwd 0 y = abs y
nwd x 0 = abs x
nwd x y
  | x < 0 = nwd (abs x) y
  | y < 0 = nwd x (abs y)
  | x == y = x
  | x > y = if (x - y) > y then nwd (x - y) y else nwd y (x - y)
  | x < y = nwd y x