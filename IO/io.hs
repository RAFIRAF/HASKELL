import System.IO
import Control.Monad
import Data.Char  
import System.Random

putStr' :: String -> IO ()
putStr' [] = return ()
putStr' (x:xs) = do putChar x;putStr xs

putStrLn' :: String -> IO ()
putStrLn' [] = putChar '\n'
putStrLn' (x:xs) = do putChar x; putStrLn' xs

majin = do
  c <- getChar
  when (c /= ' ') $ do
      putChar c
      majin 

f = forever $ do  
    putStr "Give me some input: "  
    l <- getLine  
    putStrLn $ map toUpper l       

a = take 100 $ iterate (+1) 1
--b = map (\x -> if x `mod` 15 == 0 then putStr "FizzBuzz " else if x `mod` 5 == 0 then putStr "Buzz " else if x `mod` 3 == 0 then putStr "Fizz " else putStr $ show x) a

c [] = putChar '\n'
c (x:xs) = do 
  if x `mod` 15 == 0 
    then putStr "FizzBuzz " else if x `mod` 5 == 0 
      then putStr "Buzz " else if x `mod` 3 == 0 
        then putStr "Fizz " else putStr $ show x ++ " "; c xs

cc = 
  putStrLn $ concatMap (++ " ") [if x `mod` 15 == 0 
   then "FizzBuzz" else if x `mod` 5 == 0 
    then "Buzz" else if x `mod` 3 == 0 
      then "Fizz" else show x|x<-[1..100]]

hey = do 
  name <- getLine
  let nameTag = "Hello, my name is " ++ name
  putStrLn nameTag 
--do name <- getLine; let nameTag = "Hello, my name is " ++ name in putStrLn nameTag   

majinbu = do
  foo <- putStrLn "wotz ur name bro"
  name <- getLine
  putStrLn $ "heyo " ++ name ++ " my mate!"


data Tree a = EmptyTree | Node a (Tree a) (Tree a) deriving (Show,Read,Eq)
singleton :: Ord a => a -> Tree a
singleton x = Node x EmptyTree EmptyTree
insertTree :: Ord a => a -> Tree a -> Tree a
insertTree x EmptyTree = singleton x
insertTree x (Node n l r)
  | x == n = Node n l r
  | x > n = Node n l (insertTree x r)
  | x < n = Node n (insertTree x l) r

inTree :: Ord a => a -> Tree a -> Bool
inTree x EmptyTree = False
inTree x (Node n l r)
  | x == n = True
  | x > n = inTree x r
  | x < n = inTree x l

listToTree :: Ord a => [a] -> Tree a
listToTree = foldr insertTree EmptyTree
    
main = do      
  let kfc = getLine
  _ <- putStrLn "Hello, what's your name?"    
  --putStrLn $ show foo  
  name <- kfc      
  putStrLn ("Hey " ++ name ++ ", you rock!")     

dahek = putStrLn ("Hey " ++ "s" ++ ", you rock!")     

--do let {a=2;b=3}; return $ a+b

naOpak = do
  putStrLn "Wpiszzce cos do cholery!"
  cos <- getLine
  if null cos then return () else do
  putStrLn "Po piwie:"
  putStrLn $ unwords . map reverse . words $ cos
  naOpak

returny = do      
  return ()      
  f <- return "HAHAHA"      
  line <- getLine      
  return "BLAH BLAH BLAH"      
  return 4
  putStrLn f  

lol = do    
  a <- return "hell"    
  b <- return "yeah!"   
  putStrLn $ a ++ " " ++ b   

putStr2 :: String -> IO ()  
putStr2 "" = return ()
putStr2 (c:cs) = do putChar c; putStr2 cs

putStrLn2 :: String -> IO ()
putStrLn2 [] = putChar '\n'
putStrLn2 (c:cs) = do putChar c ; putStrLn2 cs

pampers :: IO ()
pampers = do
  co <- getChar
  when (co /= ' ') $ do
    putChar co
    pampers

pampers' :: IO ()
pampers' = do
  co <- getChar
  if co /= ' '
    then do
      putChar co
      pampers'
    else return ()  

new :: IO ()
new = do
  a <- getLine
  b <- getLine
  c <- getLine
  print [a,b,c]    

new2 :: IO ()
new2 = do
  rs <- sequence [getLine,getLine,getLine]  
  print rs

spittingBackCS :: IO a
spittingBackCS = forever $ do
  kek <- getLine
  when (kek /= "") $ putStrLn . map toUpper $ kek

--forM_ [1..10] (putStr . (++ " ") . show )
kolorki :: IO () -- PRZEĆWICZYĆ TĄ FUNKCJĘ!!!!!!!!!!!!!!!!!!!!!!!!11111111111111111111111111111111111
kolorki = do
  colors <- forM [1..4] (\x -> do
    putStrLn $ "What color do you associate with the number " ++ show x ++ "?"
    getLine)
  putStrLn "The colors you associate with numbers 1, 2, 3 and 4 are:"
  --mapM_ putStrLn colors
  forM_ colors putStrLn 
--forM [1..10] return  

krotsze :: IO a
krotsze = forever $ do
  c <- getLine
  when ((length c) < 10) $ putStrLn c

-- przecwiczyc ponizsza funkcje
krotsze2 :: IO ()     
krotsze2 = do
  contents <- getContents
  --putStrLn $ "Wow"++ contents
  putStr $ shortLinesOnly contents
  
krotsze3 = interact $ unlines . filter ((<10) . length) . lines

shortLinesOnly :: String -> String
shortLinesOnly input =
  let allLines = lines input
      shortLines = filter (\line -> length line < 10) allLines
      result = unlines shortLines
  in result

isPalindrom :: IO ()
isPalindrom = interact $ unlines . map (\s -> if trulyPalindrom s then "palindrom" else "not palindrom") . lines 
  where trulyPalindrom s = s == reverse s

-- Przećwiczyć poniższą funkcję.
kaka = do
  handle <- openFile "girlfriend.txt" ReadMode
  contents <- hGetContents handle
  putStrLn contents
  hClose handle

-- Przećwiczyć poniższą funkcję.
sraka = do
  withFile "girlfriend.txt" ReadMode (\handle -> do
    contents <- hGetContents handle
    putStrLn contents)

--sequence [getLine,getLine,getLine]
--sequence $ map print [1,2,3]  

pepa = do
  damnSon <- mapM (\number -> do
    putStrLn $ "What color do you associate with the number " ++ show number ++ "?"
    getLine
    ) [1..4]
  putStrLn "The colors you associate with numbers 1, 2, 3 and 4 are:"
  mapM putStrLn damnSon

putStr3 :: String -> IO ()
putStr3 [] = return ()
putStr3 (x:xs) = do 
  putChar x
  putStr3 xs

putStrLn3 :: String -> IO ()
putStrLn3 [] = putChar '\n'
putStrLn3 (x:xs) = do 
  putChar x
  putStrLn3 xs

withFile' ::   FilePath -> IOMode -> (Handle -> IO a) -> IO a
withFile' path mode f = do
  handle <- openFile path mode
  result <- f handle
  hClose handle
  return result

gfToCaps = do
  contents <- readFile "girlfriend.txt"  
  writeFile "girlfriendCAPS.txt" (map toUpper contents)

newTask = do
  task <- getLine
  appendFile "todo.txt" (task ++ "\n")

threeCoins :: StdGen -> (Bool, Bool, Bool)  
threeCoins gen = 
  let
     (firstCoin,  newGen)   = random gen
     (secondCoin, newGen')  = random newGen
     (thirdCoin,  newGen'') = random newGen'
  in (firstCoin, secondCoin, thirdCoin)

randoms' :: (RandomGen g, Random a) => g -> [a]
randoms' gen =
  let (val, g') = random gen in val : randoms' g'

finiteRandomness :: (RandomGen g, Random a) => g -> Int -> ([a],g)
finiteRandomness gen 0 = ([],gen)
finiteRandomness gen n =
  let (val, g') = random gen
      (vs, g'') = finiteRandomness g' (n-1)
  in  (val:vs,g'')

finiteRandomness' :: (RandomGen g, Random a) => g -> Int -> [a]
finiteRandomness' gen 0 = []
finiteRandomness' gen n =
  let (val, g') = random gen in val:finiteRandomness' g' (n-1) 

diceThrows :: (Random a, Num a) => [a]
diceThrows = randomRs (1,6) (mkStdGen 0)

roznicaKolejnych :: (Num a) => [a] -> [a]
roznicaKolejnych [] = []
roznicaKolejnych [x] = []
roznicaKolejnych (x:xs:[]) = [xs-x]
roznicaKolejnych (x:xs:xss:[]) = [xs-x,xss-xs]
roznicaKolejnych (x:xs) = head xs - x : roznicaKolejnych xs

