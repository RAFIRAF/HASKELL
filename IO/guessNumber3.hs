import System.Random
import Control.Monad

main = do
	putStr "What number am I thinking of? "
	numberString <- getLine
	g <- newStdGen
	let (cpuNumber, _) = randomR (1,10) g :: (Int, StdGen)
	when (not . null $ numberString) $ do
		if (read numberString) == cpuNumber
			then putStrLn "Bravo! You've guessed it!"
			else putStrLn $ "Bad luck! I was thinking of " ++ show cpuNumber
		main