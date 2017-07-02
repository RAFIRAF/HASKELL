import Geometry
import qualified Data.Set as Set
import Data.List
import Data.Char
import qualified Data.Map as Map

numUniques :: (Eq a, Num b) => [a] -> b
numUniques = fromIntegral . length . nub

numUniques2 :: (Eq a, Num b) => [a] -> b
numUniques2 = \xs -> fromIntegral . length . nub $ xs

search :: Eq a => [a] -> [a] -> Bool  
search needle haystack =
  let nlen = length needle
  in foldl (\acc x -> if take nlen x == needle then True else acc) False (tails haystack)

ceasar :: Int -> String -> String
ceasar n s = 
  let succChar n c = chr $ ord c + n
  in  map (succChar n) s

unceasar :: Int -> String -> String
unceasar n s = ceasar (-n) s

phoneBook = [("betty","555-2938")
            ,("betty","342-2492")
            ,("bonnie","452-2928")
            ,("patsy","493-2928")
            ,("patsy","943-2929")
            ,("patsy","827-9162")
            ,("lucille","205-2928")
            ,("wendy","939-8282")
            ,("penny","853-2492")
            ,("penny","555-2111")]


--findkey :: Eq k => k -> [(k,v)] -> Maybe v
findkey k = find (\(kk,vv)->kk==k)

findkey' :: Eq k => k -> [(k,v)] -> v
findkey' k l = snd . head . filter (\(kk,vv)->kk==k) $ l

findKey :: Eq k => k -> [(k,v)] -> Maybe v
findKey k  = foldr (\(kk,v) acc -> if kk == k then Just v else acc) Nothing

fajndKi :: Eq k => k -> [(k,v)] -> Maybe v
fajndKi ki [] = Nothing
fajndKi ki ((k,w):xs) = if k == ki
  then Just w
  else fajndKi ki xs

fromList' :: Ord k => [(k,v)] -> Map.Map k v
fromList' [] = Map.empty
fromList' ((k,v):xs) = Map.insert k v (fromList' xs)

fromListy :: Ord k => [(k,v)] -> Map.Map k v
fromListy = foldl (\acc (k,v) -> Map.insert k v acc) Map.empty

phoneBookToMap :: Ord k => [(k,String)] -> Map.Map k String
phoneBookToMap = Map.fromListWith (\nr1 nr2 -> nr1++", "++nr2)

phoneBookToMap' :: Ord k => [(k,String)] -> Map.Map k [String]
phoneBookToMap' = Map.fromListWith (++) . map (\(k,v) -> (k,[v]))

text1 = "I just had an anime dream. Anime... Reality... Are they so different?"
text2 = "The old man left his garbage can out and now his trash is all over my lawn!" 

lengths :: (Ord a, Num b) => [a] -> b
lengths  = fromIntegral . Set.size . Set.fromList

sorts :: (Ord a) => [a] -> [a]
sorts = Set.toList . Set.fromList





