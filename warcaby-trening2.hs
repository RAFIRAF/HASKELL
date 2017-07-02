import Data.List
import Data.Tree

data Fig = E | WK | BK| W | B deriving (Show, Eq)
type Pos = (Int, Int)
type Neigbor = Pos
type Neigbors = [Neigbor]
type Move = (Board, Pos)
type Moves = [Move]
type Board = [[Fig]]

initialBoardStr = ".b.b.b.b\nb.b.b.b.\n.b.b.b.b\n........\n........\nw.w.w.w.\n.w.w.w.w\nw.w.w.w."

charToFig :: Char -> Fig
charToFig '.' = E
charToFig 'b' = B
charToFig 'B' = BK
charToFig 'w' = W
charToFig 'W' = WK

figToChar :: Fig -> Char
figToChar E = '.'
figToChar B = 'b'
figToChar BK = 'B'
figToChar W = 'w'
figToChar WK = 'W'

fromStr :: String -> [Fig]
fromStr = map charToFig

toStr :: [Fig] -> String
toStr = intersperse ' ' . map figToChar

initBoard :: String -> Board
initBoard = map fromStr . lines

boardToStr :: Board -> [String]
boardToStr = map toStr

addRowNumber :: Show a => a -> String -> String
addRowNumber num line = (show num) ++ " " ++ line

-- !!!
addRowNumbers :: [String] -> [String]
addRowNumbers b = zipWith addRowNumber [0..7] b

addColNumbers :: [String] -> [String]
addColNumbers b = ["  0 1 2 3 4 5 6 7"]

showBoard :: Board -> String
showBoard b = unlines boardToStr

