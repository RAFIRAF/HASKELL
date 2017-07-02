import System.Random  
import Control.Monad.State    

randomSt :: (RandomGen g, Random a) => State g a  
randomSt = state random 