import Data.List
import Data.Char



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

fromStr :: String -> [Fig]
fromStr = map charToFig

toStr :: [Fig] -> String
toStr = intersperse ' ' . map figToChar

initBoard :: String -> Board
initBoard  = map fromStr . lines

szachownica = initBoard initialBoardStr

boardToStr :: Board -> [String]
boardToStr b = map toStr b

showBoard :: Board -> String
showBoard board = unlines . addColNumbers . addRowNumbers $ boardToStr board
  where
    addRowNumber num line = (show num) ++ " " ++ line
    addRowNumbers board = zipWith addRowNumber [0..7] board
    addColNumbers board = ["  0 1 2 3 4 5 6 7  "] ++ board

getFig :: Board -> Pos -> Fig
getFig b (row, column) = b !! row !! column

replaceWithFig :: Fig -> [Fig] -> Int -> [Fig]
replaceWithFig f (_:t) 0 = f : t
replaceWithFig f (h:t) col = h:replaceWithFig f t (col-1)

setFig :: Fig -> Board -> Pos -> Board
setFig fig (headBoard:tailBoard) (0, column) = replaceWithFig fig headBoard column : tailBoard
setFig fig (headBoard:tailBoard) (row, column) = headBoard : setFig fig tailBoard ((row-1), column)

--countWhiteFigs :: Board -> Int
--countWhiteFigs [] = 0
--countWhiteFigs (h:t) = (countWhiteFigs t) + (length (filter (\f -> f == W || f == WD) h))

boardToNumberOfWhiteFigures :: Board -> Int
boardToNumberOfWhiteFigures []    = 0
boardToNumberOfWhiteFigures (h:t) = let
                                      isWhiteFig   fig  = fig == W || fig == WD  :: Bool
                                      areWhiteFigs figs = filter isWhiteFig figs :: [Fig]
                                    in 
                                      length (areWhiteFigs h) + boardToNumberOfWhiteFigures t

--boardToNumberOfBlackFigures :: Board -> Int
--boardToNumberOfBlackFigures []    = 0
--boardToNumberOfBlackFigures (h:t) = let
--                                      isBlackFig   fig  = fig == B || fig == BD  :: Bool
--                                      areBlackFigs figs = filter isBlackFig figs :: [Fig]
--                                    in 
--                                      length (areBlackFigs h) + boardToNumberOfBlackFigures t   

boardToNumberOfBlackFigures :: Board -> Int
boardToNumberOfBlackFigures b = foldr (\_ acc -> 1 + acc) 0 (foldr (\x acc -> if (isBlackFig x) then x:acc else acc) [] [uhu | uhu<- [ble | ble<-b]]) 
isBlackFig :: Fig -> Bool
isBlackFig f = f `elem` [B,BD]
areBlackFigs :: [Fig] -> [Fig]
areBlackFigs  = foldr (\x acc -> if (isBlackFig x) then x:acc else acc) []-- filter isBlackFig figs :: [Fig]
dlugosc :: [t]->Int
dlugosc = foldr (\_ acc -> 1 +acc) 0

--blabla :: [Int] -> [Int]
--blabla = foldr (\x acc -> x:acc) []

getRow :: Pos -> Int
getRow = fst

getCol :: Pos -> Int
getCol = snd

isEmpty :: Board -> Pos -> Bool
isEmpty b p = getFig b p == E

isWhite :: Board -> Pos -> Bool
isWhite b p = getFig b p `elem` [W,WD]
isBlack :: Board -> Pos -> Bool
isBlack b p = getFig b p `elem` [B,BD]

isValidPos :: Pos -> Bool
isValidPos (row,col) = let validPositions = [0..7] in (row `elem` validPositions && col `elem` validPositions)

countPos :: Pos -> Pos -> Pos
countPos (r1,c1) (r2,c2) = (abs (r2-r1)+r2, abs (c2-c1)+c2)

countPos2 :: Pos -> Pos -> Pos
countPos2 (row, col) (neighborRow, neighborCol) = 
  ((countIndex row neighborRow), (countIndex col neighborCol))
  where
    countIndex index neighborIndex = 2 * neighborIndex - index

capturedPos :: Pos -> Pos -> Pos
capturedPos prevPos@(r1,c1) newPos@(r2,c2) = ((r1+r2) `quot` 2,(c1+c2) `quot` 2)
--setFig :: Fig -> Board -> Pos -> Board
--setFig fig (headBoard:tailBoard) (0, column) = replaceWithFig fig headBoard column : tailBoard
--setFig fig (headBoard:tailBoard) (row, column) = headBoard : setFig fig tailBoard ((row-1), column)
makeCaptureMove :: Board -> Pos -> Pos -> Board
makeCaptureMove b curp newp 
  | isWhite b from = (setFig E (setFig W (setFig E b from) to) (capturedPos from to))
  | isBlack b from = 