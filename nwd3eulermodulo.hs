nwd :: Integer -> Integer -> Integer
nwd 0 y = abs y
nwd x 0 = abs x
nwd x y
  | x < 0 = nwd (abs x) y
  | y < 0 = nwd x (abs y)
  | x > y = if x `mod` y > y then nwd (x `mod` y) y else nwd y (x `mod` y)
  | x == y = abs x
  | x < y = nwd y x