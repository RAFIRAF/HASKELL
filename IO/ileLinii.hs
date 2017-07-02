import Control.Monad(when)
import System.Directory
import System.Environment

main = do
	(plik:_) <- getArgs
	policzLinie plik

policzLinie :: FilePath -> IO ()	
policzLinie file = do
	fileExists <- doesFileExist file
	if fileExists then do
		contents <- readFile file
		putStrLn . show . length . lines $ contents
		else putStrLn "File does not exist!"