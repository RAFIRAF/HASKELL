nwd :: Int -> Int -> Int
nwd 0 y = abs y
nwd x 0 = abs x
nwd x y | x < 0 = nwd (abs x) y
nwd x y | y < 0 = nwd x (abs y)
nwd x y = maximum . filter (\xx-> x `mod` xx == 0 && y `mod` xx == 0) $ if x <= y then [1..x] else [1..y]