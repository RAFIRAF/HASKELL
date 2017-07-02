import System.Random

main = do
	g <- getStdGen
	putStrLn $ take 8 $ randomRs ('0','z') g