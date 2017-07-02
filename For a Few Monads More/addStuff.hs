--import Control.Monad.Instances    
addStuff :: Int -> Int  
addStuff = do      
  a <- (*2)      
  b <- (+10)      
  return (a+b) 