nwd3 :: Integer -> Integer -> Integer
nwd3 0 y = abs y
nwd3 x 0 = abs x
nwd3 x y = maximum $ filter (\xx -> x `mod` xx == 0) . filter (\yy -> y `mod` yy == 0) $ if x > y then [1..abs y] else [1..abs x]