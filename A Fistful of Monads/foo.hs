foo :: Maybe String
foo = Just 3   >>= \x -> 
      Just "!" >>= \y -> 
      Just (show x ++ y)

bar :: Maybe String
bar = do
  let a = 3
  let b = "!"
  return (show a++b)

barbar :: Maybe String
barbar = do
  x <- Just 3
  y <- Nothing    
  Just (show x ++ y)

