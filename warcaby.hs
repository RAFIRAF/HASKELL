import Data.List
import Data.Char
import Data.Tree hiding (Tree )
data Tree a b = Branch b (Tree a b) (Tree a b) 
              | Leaf a deriving (Eq,Ord,Show)
data BTree a = BNil | BNode a (BTree a) (BTree a) deriving Show
data Move = SM Board Pos Pos | KM | MKM
data Fig   = W | B | WD | BD | E deriving (Show, Eq)
type Board = [[Fig]]
type Pos   = (Int, Int)

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

getFig :: Board -> Pos -> Fig
getFig board (row, col) = board !! row !! col

replaceWithFig :: Fig -> [Fig] -> Int -> [Fig]
replaceWithFig fig (h:t) 0 = fig : t
replaceWithFig fig (h:t) col = h : replaceWithFig fig t (col - 1)

setFig :: Fig -> Board -> Pos -> Board
setFig fig (h:t) (0, col) = replaceWithFig fig h col : t
setFig fig (h:t) (row, col) = h : setFig fig t ((row - 1), col)

countWhiteFigs :: Board -> Int
countWhiteFigs [] = 0
countWhiteFigs (h:t) = (countWhiteFigs t) + (length (filter (\f -> f == W || f == WD) h))

countBlackFigs :: Board -> Int
countBlackFigs [] = 0
countBlackFigs (h:t) = (countBlackFigs t) + (length (filter (\f -> f == B || f == BD) h))

getRow :: Pos -> Int
getRow p = fst p

getCol :: Pos -> Int
getCol p = snd p

isEmpty :: Board -> Pos -> Bool
isEmpty b p = (getFig b p) == E

isWhite :: Board -> Pos -> Bool
isWhite b p = (getFig b p) == W || (getFig b p) == WD

isBlack :: Board -> Pos -> Bool
isBlack b p = (getFig b p) == B || (getFig b p) == BD

isValidPos :: Pos -> Bool
isValidPos (row, col) = (row >= 0) && (row <= 7) && (col >= 0) && (col <= 7)

countPos :: Pos -> Pos -> Pos
countPos (row, col) (neighborRow, neighborCol) = 
  ((countIndex row neighborRow), (countIndex col neighborCol))
  where
    countIndex index neighborIndex = 2 * neighborIndex - index

capturedPos :: Pos -> Pos -> Pos
capturedPos (r1, c1) (r2, c2) = (quot (r1 + r2) 2, quot (c1 + c2) 2)

--setFig :: Fig -> Board -> Pos -> Board
--setFig fig (h:t) (0, col) = replaceWithFig fig h col : t
--setFig fig (h:t) (row, col) = h : setFig fig t ((row - 1), col)
makeCaptureMove :: Board -> Pos -> Pos -> Board
makeCaptureMove b from to
  | isWhite b from = (setFig E (setFig W (setFig E b from) to) (capturedPos from to))
  | isBlack b from = (setFig E (setFig B (setFig E b from) to) (capturedPos from to))

--makeSimpleMove :: Move -> Board
--makeSimpleMove (SM b from to)
--makeSimpleMove b from to
makeSimpleMove :: Board -> Pos -> Pos -> Board
makeSimpleMove b from to
  | isWhite b from = setFig W (setFig E b from) to
  | isBlack b from = setFig B (setFig E b from) to

--n is a list of nieghbors 
--getCaptureMoves :: Board -> Pos -> [Pos] -> [Pos]
--getCaptureMoves b p n = [pos | pos <- map (countPos p) n, isValidPos pos, isEmpty b pos]
getCaptureMoves :: ((Board, Pos), [Pos]) -> [((Board, Pos), [Pos])]
getCaptureMoves ((_, _), []) = []
getCaptureMoves ((b, p), n) = 
  x ++ concat (map getCaptureMoves x)
  where
    x = [((makeCaptureMove b p pos, pos), (getNeighbors (makeCaptureMove b p pos) pos)) | pos <- map (countPos p) n, isValidPos pos, isEmpty b pos]

getNeighbors :: Board -> Pos -> [Pos]
getNeighbors b (row, col)
  | isWhite b (row, col) = [(x, y) | x <- [(row - 1), (row + 1)], y <- [(col - 1), (col + 1)], isValidPos (x, y), isBlack b (x, y)]
  | isBlack b (row, col) = [(x, y) | x <- [(row - 1), (row + 1)], y <- [(col - 1), (col + 1)], isValidPos (x, y), isWhite b (x, y)]


getComplexMoves :: Board -> Pos -> [(Board, Pos)]
getComplexMoves b p = map fst (getCaptureMoves ((b, p), (getNeighbors b p)))

getSimpleMoves :: Board -> Pos -> [(Board, Pos)]
getSimpleMoves b (row, col) = [(makeSimpleMove b (row, col) (x, y), (x, y)) | x <- [(row - 1)], y <- [(col - 1), (col + 1)], isValidPos (x, y), isEmpty b (x, y)]

getMoves :: Board -> Pos -> [(Board, Pos)]
getMoves b p
  | isEmpty b p = []
  | otherwise = getSimpleMoves b p ++ getComplexMoves b p

b = initBoard ".b.b.b.b\n\
        \b.b.b.b.\n\
        \...b.b.b\n\
        \.b......\n\
        \........\n\
        \wb..w.w.\n\
        \ww.w.w.w\n\
        \w.w.w.w."

x = showBoard b

list = getMoves b (6,0)
move = showBoard $ fst $ last list
-- minmax algorytm do optymalnego ruchu
-- lines Data.Tree
-- odciecie drzewa
--minmax to ma byc minmax a nie minmax dla konkretnego rozwiazania (tak jest najlepiej)
-- funkcja otwarta, sieci petriego
-- gęsty graf, multigraf
-- neoforj 	
-- moves f board = (genkill f b, genMoves f b)
--test quickcheck automatycznie generuje testy
--IORef do zmiennych
-- import Data.Tree -- drzewo dokłdnie takie jakie potrzebne jest w tej implementacji
-- funkcja oceny planszy next time    

--zip [(x,y)|x<-[1..8],y<-[1..8]] $ fromStr initialBoardStr 
--zip [(x,y)|x<-[1..8],y<-[1..8]] $ concat b

--addRowNumber' :: Int -> String -> String
--addRowNumber' num line = (show num) ++ " " ++ line
--addRowNumbers' :: [Fig] 
--addRowNumbers' board = zipWith addRowNumber [1..8] board


{-
  Abstrakcje gier planszowych
  Generalne:
  - plansza
    - pola (rozmieszczenie obok siebie, kolumny, rzedy itd)
  - figury
    - rodzaje
    - reguly ruchu
    - reguly bicia
    - reguly zamiany w inne figury
    - rozmieszczenie poczatkowe

  Warcabowe:
  -plansza 8x8
  - figury:
    - rozmieszczenie poczatkowe
    - pionki albo damki
    - reguly ruchu dla pionkow i reguly ruchu dla damek
    - regula bicia dla pionkow i regula bicia dla damek
    - regula zamiany pionkow w damki
  
-}
 

--infiniteTree :: BTree Int
--infiniteTree = go 1 where 
  --go n = BNode (go (2*n)) n (go (2*n+1))

toDataTree (Leaf a) = Node a []
toDataTree (Branch b cs ds) = Node b [toDataTree cs, toDataTree ds]

d = Branch "1" (Branch "11" (Leaf "111") (Leaf "112")) 
               (Branch "12" (Leaf "121") (Leaf "122"))

e = toDataTree d
f = putStrLn $ drawTree e