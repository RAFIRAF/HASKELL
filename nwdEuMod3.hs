nwd :: Integer -> Integer -> Integer
nwd 0 y = abs y
nwd x 0 = abs x
nwd x y
  | x < 0 = nwd (abs x) y
  | y < 0 = nwd x (abs y)
  | x == y = abs x
  | x > y = nwd (x `mod` y) y
  | x < y = nwd y x

-- nwd 32 20 -> nwd 20 12 -> nwd 12 8 -> nwd 8 4 -> nwd 4 0 -> 4  