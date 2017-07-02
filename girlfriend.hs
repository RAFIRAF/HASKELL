import System.IO
main = do
	handle <- openFile "girlfriend.txt" ReadMode
	contens <- hGetContents handle
	putStrLn contens
	hClose handle