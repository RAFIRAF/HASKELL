nwdEu :: Integer -> Integer -> Integer
nwdEu 0 _ = error "0 nie ma dzielników... chyba"
nwdEu _ 0 = error "0 nie ma dzielników... chyba"
nwdEu x y
  | x == y = x
  | x > y = nwdEu' x y 
  | x < y = nwdEu' y x where
      nwdEu' x' y' = (if x'-y'>y' then (nwdEu (x'-y') y') else (nwdEu y' (x'-y')))
