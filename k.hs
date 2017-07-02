import Data.Char
len :: (Integral a) => [t] -> a
len []     = 0
len (_:xs) = 1 + len xs 

--(.) :: (b -> c) -> (a -> b) -> a -> c
--f . g = \x -> f (g x)

potroj :: [t] -> [t]
potroj [] = []
potroj (x:xs) = x:x:x:potroj xs

--slowa =  do {putStr "Podaj slowa:\n"; s <- getLine;putStr $ show $ length . words $ s;putChar '\n'}
--slowa = putStr "Podaj slowa:\n" >> (\s -> getLine >>= return $ putStr $ show $ length . words $ s) >> putChar '\n'
--wordsCount =  do {putStr "Write a line and press [enter]:\n"; s <- getLine;putStr $ show $ length . words $ s;putChar '\n'}


usun :: String -> String
usun [] = []
usun (x:xs)
  | isSpace x = usun xs
  | otherwise = x:usun xs

usun' :: String -> String
usun' = filter (isAlphaNum)

wordsCount :: IO ()
wordsCount = putStr "Write a line and press [enter]:\n" >> getLine >>= putStr . show . length . words >> putChar '\n'

mapuj :: (a->b)->[a]->[b]
mapuj f = foldr (\x acc -> f x : acc) []

sumka :: String -> String
sumka = show . sum . map digitToInt
--sum . map digitToInt $ "22"
sumaCyfr :: IO ()
sumaCyfr = putStrLn "Podaj liczbe:" >> getLine >>= putStrLn . show . sum . map digitToInt

--4. Monada dla Problem a = Ok a | Error String (15 pkt)
instance Monad Problem where
  return x = Ok x
  Error x >>= _ = Error x
  Ok x >>= f = f x
--5. Implementacja Monady dla typu CosTam a = Ok a | Error {mess:: m, info:: i} (chyba coś takieg)
instance Monad CosTam where
  return = Ok
  (>>=) (Error m i) _ = Error m i
  (>>=) (Ok a) f = f a

--data CalculationContext = Result a | Error {errorId::Int,errorMessage::"String"} napisać do tego Monade
instance Monad CalculationContext where
  return a = Result a
  Error id msg >>= _ =  Error id msg
  Result a >>= f = f a

-- 6. Mamy Tree a = Node a [Tree a] - funkcja która zwróci nam listę z  wierzchołkami, oraz funkcja, która z listy wygeneruje nam Drzewo,
-- którego każdy node ma 3 wierzchołki (czyli Node x [tree_a, tree_b, tree_c]) <- w tym drzewie nie ma znaczenia kolejność wierzchołków

treeToList Empty = []
treeToList (Node x children) = [x]++(concatMap treeToList children)

--
wypiszDrzewo (Node x xs) = print x >> mapM_ wypiszDrzewo xs
drzewoWListe (Node x xs) = x:(concat $ map drzewoWListe xs)
listaWDrzewo (x:xs) = 
  let dzielNaTrzyListy = foldr (\a [x,y,z] -> [a:z,x,y]) [[],[],[]]
  in Node x (map listaWDrzewo $ filter (/=[]) $ dzielNaTrzyListy xs)

potroj2 :: [t]  -> [t]
potroj2 = map (replicate 3)

ileSlow = putStrLn "Wpisz tekst i nacisnij [enter]" >> getLine >>= putStrLn . show . length . words

