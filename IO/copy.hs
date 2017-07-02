import System.Environment
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString as S

main = do
  (copyFrom:copyTo:_) <- getArgs
  copy copyFrom copyTo
  
copy :: FilePath -> FilePath -> IO ()
copy copyFrom copyTo = do  
  contents <- B.readFile copyFrom
  B.writeFile copyTo contents
--  (tempName, tempHandle) <- openTempFile "." "temp"
--  B.hPutStr tempHandle contents 
--  hClose tempHandle
--  renameFile tempName copyTo
