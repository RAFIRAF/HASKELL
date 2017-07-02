nwdEu :: Int -> Int -> Int
nwdEu 0 y = abs y
nwdEu x 0 = abs x
nwdEu x y
  | x < 0 || y < 0 = nwdEu (abs x) (abs y)
  | x < y = nwdEu y x
  | otherwise = nwdEu (subtract y x) y