--2 3 4 + -
--2 7 -

--"2 3 4 + - 2 +"
--"2 3 + 2 -" [2,3] <- + 
--"5 2 -" [5,2] <- -
--"3" [3]


rpn :: (RealFloat a, Read a) => String -> a
rpn = head . foldl calc [] . words


--2 3 5 7 - + *
--[] 2 = 2:[] = [2]
--[7,5,3,2]
calc :: (RealFloat a, Read a) => [a] -> String -> [a]
calc (x:y:xs) "+" = x+y:xs
calc (x:y:xs)"-" = y-x:xs 
calc (x:y:xs)"*" = x*y:xs
calc (x:y:xs)"/" = y/x:xs
calc       xs x  = read x:xs


 
