import System.IO

main = do
  gen <- getStdGen
  let (randNumber, _) = randomR (1,length $ lines contents) gen :: (Int, StdGen)
  handle <- openFile "przemyslenia 2016-05-10.txt" ReadMode

  contents <- hGetContents handle
  let firstLine = head $ lines contents
  putStrLn firstLine
  putStrLn $ show randNumber
  hClose handle