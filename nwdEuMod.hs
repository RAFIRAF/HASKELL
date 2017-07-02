nwdEu :: Integer -> Integer -> Integer
nwdEu 0 y = y
nwdEu x 0 = x
nwdEu x y
  | x == y = x
  | x > y = nwdEu' x y 
  | x < y = nwdEu' y x where
      nwdEu' x' y' = (if x'`mod`y'>y' then (nwdEu (x'`mod`y') y') else (nwdEu y' (x'`mod`y')))