import System.Environment

main = do
	args <- getArgs
	name <- getProgName
	putStrLn "The program's name is:"
	putStrLn name
	putStrLn "The arguments are:"
	mapM putStrLn args