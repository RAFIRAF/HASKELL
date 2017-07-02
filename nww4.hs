nww :: Int -> Int -> Int
nww 0 y = 0
nww x 0 = 0
nww x y = head . filter (\xx -> mod xx x == 0 && mod xx y ==0) $ if x < y then [x..x*y] else [y..x*y]