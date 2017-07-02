nww :: Integer -> Integer -> Integer
nww x y = minimum $ filter (\yy -> yy `mod` y == 0) (filter (\xx -> xx `mod` x ==0) (if x > y then [x..x*y] else [y..y*x]))