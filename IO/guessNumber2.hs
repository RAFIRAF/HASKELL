import System.Random
import Control.Monad(when)

main = do
   g <- getStdGen
   guess g

guess :: StdGen -> IO ()
guess gen = do
   putStrLn "What number from 1 to 10 am I thinking of?"
   numberString <- getLine
   --let number = (fst . head $ reads numberString) :: Int
   let number = reads numberString :: [(Int,String)]
   when (number /= []) $ do
     let (cpuNumber, g') = randomR (1,10) gen :: (Int, StdGen)
         number' = fst . head $ number
     if number' == cpuNumber
       then putStrLn "Wow! You've guessed it!"
       else putStrLn $ "Bad luck mate! It was " ++ show cpuNumber
     guess g'