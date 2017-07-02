mple :: Int -> Int -> Int
mple m n 
  | m < 1 || n < 1 = error "m and n must be >= 1"
  | n > m = error "n cannot be > m"
  | n == m = 1
  | n == 1 = m
  | n <= m `div` 2 = m
  | otherwise = 2 * m - 2 * n

mplo :: Int -> Int -> Int
mplo m n 
  | m < 1 || n < 1 = error "m and n must be >= 1"
  | n > m = error "n cannot be > m"
  | n == m = 1
  | n == 1 = m
  | n <= (m `div` 2 + 1) = m
  | otherwise = m + 1 - n