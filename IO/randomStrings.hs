import System.Random

--module Main where

main = do
  gen <- getStdGen
  let
    (first20, rest) = splitAt 20 $ randomRs ('0','z') gen
    (sec20, _) = splitAt 20 $ rest
  putStrLn first20
  putStrLn sec20
  g0 <- getStdGen
  g1 <- newStdGen
  g2 <- newStdGen
  putStrLn $ take 16 $ randomRs ('0','z') g0
  putStrLn $ take 16 $ randomRs ('0','z') g0
  putStrLn $ take 8 $ randomRs ('0','z') g1
  putStrLn $ take 8 $ randomRs ('0','z') g2