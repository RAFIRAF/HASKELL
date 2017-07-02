import Control.Monad

powerset :: [a] -> [[a]]
powerset = filterM (\x -> [True,False])