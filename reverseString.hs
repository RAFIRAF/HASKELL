import Data.List  

reverseString :: String -> String
reverseString s = if (length s) <= 1 then s else l++r where
  r = reverseString(take( (genericLength s) `div` 2) s)
  l = reverseString(drop( (genericLength s) `div` 2) s)