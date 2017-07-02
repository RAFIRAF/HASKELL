import Control.Monad.State    

type Stack = [Int]    

--pop :: Stack -> (Int,Stack)  
--pop (x:xs) = (x,xs)    

--push :: Int -> Stack -> ((),Stack)  
--push a xs = ((),a:xs)

pop :: State Stack Int  
pop = state $ \(x:xs) -> (x,xs)    

push :: Int -> State Stack ()  
push a = state $ \xs -> ((),a:xs)

stackManip :: State Stack Int  
stackManip = do      
  push 3      
  pop      
  pop

stackStuff :: State Stack ()  
stackStuff = do      
  a <- pop      
  if a == 5          
    then push 5          
  else do              
    push 3              
    push 8


moreStack :: State Stack ()  
moreStack = do      
  a <- stackManip      
  if a == 100          
    then stackStuff          
    else return ()  