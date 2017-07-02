--2 3 +
--words "10 4 3 + 2 * -"
--["10","4","3","+","2","*","-"]

solveRPN :: String -> Int
solveRPN s = head $ foldl (\acc x -> rpn x acc) [] (words s)

type Sign = String
rpn :: Sign -> [Int] -> [Int]
rpn "*" (x:y:xs) = (x*y):xs
rpn "+" (x:y:xs) = x+y:xs
rpn "-" (x:y:xs) = y-x:xs
rpn x xs =  read x:xsd