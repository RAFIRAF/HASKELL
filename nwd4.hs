nwd :: Integer -> Integer -> Integer
nwd x 0 = abs x
nwd 0 y = abs y
nwd x y = maximum $ filter (\xx -> x `mod` xx == 0 && y `mod` xx == 0) $ if x > y then [1..abs y] else [1..abs x]