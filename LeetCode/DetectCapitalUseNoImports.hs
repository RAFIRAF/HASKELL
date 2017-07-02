detectCapitalUse :: String -> Bool
detectCapitalUse [] = error "Function accepts non-empty Strings only."
detectCapitalUse s
  | onlyFirstUpper s = True
  | allUpper       s = True
  | allLower       s = True
  | otherwise        = False
  where
    onlyFirstUpper (x:xs) = isUpper x && allLower xs
    allUpper = all isUpper
    allLower = all isLower
    isUpper = (`elem` ['A'..'Z'])
    isLower = (`elem` ['a'..'z'])