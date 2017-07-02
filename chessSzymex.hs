data Figure = E | K | Q | R | B | P
charToFig :: Char -> Fig
charToFig 'w' = W
charToFig 'W' = WD
charToFig 'b' = B
charToFig 'B' = BD
charToFig '.' = E

figToChar :: Fig -> Char
figToChar W  = 'w'
figToChar WD = 'W'
figToChar B  = 'b'
figToChar BD = 'B'
figToChar E  = '.'

initialBoardStr = ".b.b.b.b\n\
                  \b.b.b.b.\n\
                  \.b.b.b.b\n\
                  \........\n\
                  \........\n\
                  \w.w.w.w.\n\
                  \.w.w.w.w\n\
                  \w.w.w.w."
szachownica = initBoard initialBoardStr

--nieodporne na znak nowej linii
fromStr2 :: String -> [Fig]
fromStr2 str = map charToFig str

fromStr :: String -> [Fig]
fromStr str = map charToFig $ concat . lines $ str

toStr2 :: [Fig] -> String
toStr2 l = intersperse ' ' $ map figToChar l

toStr :: [Fig] -> String
toStr l = map figToChar l

--toStr2 l = foldl1 (\acc x -> if isSpace x then x:acc else acc)intersperse ' ' $ map figToChar l

initBoard :: String -> Board
initBoard str = map fromStr (lines str) 

--boardToStr :: Board -> [String]
--boardToStr b = map toStr2 [x | x <- b]

boardToStr :: Board -> [String]
boardToStr b = map toStr2 b

addRowNumber num line = (show num) ++ " " ++ line
addRowNumbers boardstr = zipWith addRowNumber [0..7] boardstr
addColNumbers boardstr = ["  0 1 2 3 4 5 6 7  "] ++ boardstr

showBoard :: Board -> String
showBoard board = unlines . addColNumbers . addRowNumbers $ boardToStr board

showBoard2 :: Board -> IO()
showBoard2   b = putStrLn $ unlines (map (\x->fst x ++ snd x) $ zip [show x ++ 
  "  "|x<-[1..8]] $ map toStr2  szachownica)++"   1 2 3 4 5 6 7 8"