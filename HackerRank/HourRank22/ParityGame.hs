import Control.Applicative
import Control.Monad
import System.IO


main :: IO ()
main = do
    n_temp <- getLine
    let n = read n_temp :: Int
    a_temp <- getLine
    let a = map read $ words a_temp :: [Int]
    print $ smallestSizeSubsequence a
    

getMultipleLines :: Int -> IO [String]

getMultipleLines n
    | n <= 0 = return []
    | otherwise = do          
        x <- getLine         
        xs <- getMultipleLines (n-1)    
        let ret = (x:xs)    
        return ret       
        
smallestSizeSubsequence :: [Int] -> Int
smallestSizeSubsequence xs
  | even $ numberOfOdds xs = 0
  | odd (numberOfOdds xs) && (length xs == 1) = -1
  | otherwise = 1

numberOfOdds :: [Int] -> Int
numberOfOdds xs = foldl (\acc x -> if odd x then (acc + 1) else acc) 0 xs
