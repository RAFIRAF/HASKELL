import System.IO
import Data.List
import System.Directory

main = do
  handle <- openFile "todo.txt" ReadMode
  (tempName, tempHandle) <- openTempFile "." "temp"
  contents <- hGetContents handle
  let tasks = lines contents
      numberedTasks = zipWith (\n line -> show n ++ " - " ++ line) [0..] tasks
  putStrLn "These are your todo tasks:"
  putStrLn $ unlines numberedTasks
  putStrLn "Which one do you wish to delete?"
  numberString <- getLine
  let number = read numberString
      newTasks = delete (tasks !! number) tasks
  hPutStr tempHandle $ unlines newTasks
  hClose handle
  hClose tempHandle
  removeFile "todo.txt"
  renameFile tempName "todo.txt"