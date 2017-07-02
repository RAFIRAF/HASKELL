import System.IO.Error
--import System.IO
import System.Environment
--import Control.Exception

main = toTry `catchIOError` handler

toTry :: IO ()
toTry = do
    (fileName:_) <- getArgs
    contents <- readFile fileName
    putStrLn $ show $ length $ lines  contents

handler :: IOError -> IO () 
handler e
    | isDoesNotExistError e = case ioeGetFileName e of
       Just fileName -> putStrLn $ "The file \"" ++ fileName ++ "\" does not exissssssssssssssssst!!!!!11111111"
       Nothing       -> putStrLn "The file does not exist at unknown location!"
    | otherwise =  ioError e