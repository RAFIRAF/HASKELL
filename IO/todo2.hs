import Data.List
import System.IO
import System.Directory
import System.Environment

type Arguments = [String]
type Action = Arguments -> IO ()
type Command = String

dispatch :: [(Command, Action)]
dispatch =  [ ("add", add)
            , ("remove", remove)
            , ("view", view)
            , ("bump", bump)
            ]

main = do
  (command:args) <- getArgs
  let (Just action) = lookup command dispatch in (if (lookup command dispatch) == Nothing then do errorExit else (action args))

errorExit :: IO ()  
errorExit = putStrLn "ERROR: ARGUMENTS"

add :: Action
add [filePath, task] = do
  appendFile filePath $ task ++ "\n"

remove :: Action
remove [filePath, taskNumber] = do
  handle <- openFile filePath ReadMode
  (tempName, tempHandle) <- openTempFile "." "temp"
  contents <- hGetContents handle
  let tasks = lines contents
  let number = read taskNumber
      newTasks = delete (tasks !! number) tasks
  hPutStr tempHandle $ unlines newTasks
  hClose handle
  hClose tempHandle
  removeFile filePath
  renameFile tempName filePath

view :: Action
view [filePath] = do
  contents <- readFile filePath
  let tasks = lines contents
      numberedTasks = zipWith (\n line -> show n ++ " - " ++ line) [0..] tasks
  putStrLn $ unlines numberedTasks

bump :: Action
bump [filePath, taskNumber] = do
  handle <- openFile filePath ReadMode
  (tempName, tempHandle) <- openTempFile "." "temp"
  contents <- hGetContents handle
  let tasks = lines contents
      number = read taskNumber
      newTasks = delete (tasks !! number) tasks
      newerTasks = (tasks !! number) : newTasks
  hPutStr tempHandle $ unlines newerTasks
  hClose handle
  hClose tempHandle
  removeFile filePath
  renameFile tempName filePath


