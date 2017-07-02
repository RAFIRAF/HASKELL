import Control.Monad
type KnightPos = (Int,Int)

moves :: KnightPos -> [KnightPos]
moves (c,r) = do
  (c',r') <- [(c+2,r+1),(c+2,r-1),(c-2,r+1),(c-2,r-1),(c+1,r+2),(c+1,r-2),(c-1,r+2),(c-1,r-2)]
  guard (c'`elem`[0..7]&&r'`elem`[0..7])
  return (c',r')

moves' :: KnightPos -> [KnightPos]
moves' (c,r) = filter (\(c',r')->(c'==c+2&&r'==r-1)||(c'==c+2&&r'==r+1)||(c'==c+1&&r'==r-2)||
  (c'==c+1&&r'==r+2)||(c'==c-2&&r'==r-1)||(c'==c-2&&r'==r+1)||(c'==c-1&&r'==r-2)||(c'==c-1&&r'==r+2)) [(c,r)|c<-[0..7],r<-[0..7]]

moves'' :: KnightPos -> [KnightPos]
moves'' (c,r) = filter onBoard [(c+2,r-1),(c+2,r+1),(c-2,r+1),(c-2,r-1),(c+1,r+2),(c+1,r-2),(c-1,r+2),(c-1,r-2)]
  where onBoard (c,r) = c>=0&&c<8&&r>=0&&r<8

in3 :: KnightPos -> [KnightPos]
in3 (c,r) = do
  first <- moves (c,r)
  second <- moves first
  third <- moves second
  return third

in3' :: KnightPos -> [KnightPos]
in3' p = moves p >>= \x -> moves x >>= \y -> moves y

in3'' :: KnightPos -> [KnightPos]
in3'' p = pure p >>= moves >>= moves >>= moves

getIn3 ::  KnightPos -> KnightPos -> [KnightPos]
getIn3 p1 p2 = do
  first <- moves p1
  second <- moves first
  moves second
