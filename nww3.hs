nww :: Integer -> Integer -> Integer
nww x y = head $ filter (\xx -> xx `mod` y == 0 && xx `mod` x == 0) $ if x > y then [x..x*y] else [y..y*x]
