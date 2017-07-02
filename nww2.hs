nww :: Integer -> Integer -> Integer
nww x y = minimum $ filter (\xx -> xx `mod` x == 0) . filter (\yy -> yy `mod` y == 0) $ if x > y then [y..x*y] else [x..y*x]