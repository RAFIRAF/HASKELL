nwd' :: Int -> Int -> Int
nwd' x y
  | x < 0 = nwd' (abs x) y
  | y < 0 = nwd' x (abs y)
  | otherwise = nwdEu x y

nwdEu :: Int -> Int -> Int
nwdEu 0 y = abs y
nwdEu x 0 = abs x
nwdEu x y
  | x < y = nwdEu y x
  | otherwise = nwdEu (x `mod` y) y