import System.Random

main = do
	putStrLn "Which number from 1 to 10 am I thinking of?"
	numberString <- getLine
	g <- newStdGen
	let cpuNumber = ((fst $ randomR (1,10) g)::Int)
	if cpuNumber == (read numberString )
		then do putStrLn "Wow, you've guessed it!"; main
		else do putStrLn ("Wrong! It was " ++ show cpuNumber); main