nwd :: Integer -> Integer -> Integer
nwd x y = maximum $ filter (\yy -> y `mod` yy == 0) . filter (\xx -> x `mod` xx == 0) $ if x > y then [1..y] else [1..x]