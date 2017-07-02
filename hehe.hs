import Data.List -- (intersperse)

data Fig   = E | W | WD | B | BD deriving (Show, Read, Eq, Ord, Bounded, Enum)
type Pos   = (Row, Col)
type Row   = Int
type Col   = Int
type Board = [[Fig]]

initialBoardStr = ".b.b.b.b\nb.b.b.b.\n.b.b.b.b\n........\n........\nw.w.w.w.\n.w.w.w.w\nw.w.w.w."

charToFig :: Char -> Fig -- Converts Chars to Figures
charToFig c = case c of 
  '.' -> E
  'w' -> W
  'W' -> WD
  'b' -> B
  'B' -> BD

strToFigs :: String -> [Fig] -- Converts a String to list of Figures
strToFigs = map charToFig

initBoard :: String -> Board -- Converts a String to a Board
initBoard = map strToFigs . lines

showBoard :: Board -> String -- Shows the Board in a friendly format
showBoard = addColNumbers . unlines . addRowNumbers . map figsToStr where
  addColNumbers str = "  0 1 2 3 4 5 6 7\n" ++ str
  addRowNumbers rows = zipWith addRowNumber [0..7] rows where
    addRowNumber number row = show number ++ " " ++ row  

b = initBoard initialBoardStr

figToChar :: Fig -> Char -- Converts Figures to Chars
figToChar f = case f of 
  E  -> '.'
  W  -> 'w'
  WD -> 'W'
  B  -> 'b'
  BD -> 'B'  

--show' E = '.'  

figsToStr :: [Fig] -> String -- Converts a list of Figures to a String
figsToStr = intersperse ' ' . map figToChar

returnFig :: Pos -> Board -> Fig -- Returns a Figure at a given Position
returnFig (r,c) b = b !! r !! c

replaceRow :: [Fig] -> Fig -> Int -> [Fig] -- Replaces a Figure of a Row with a given Figure
replaceRow (f:figures) fig col = case col of
  0         -> fig:figures
  otherwise -> f: replaceRow figures fig (col-1) 

placeFig :: Fig -> Board -> Pos -> Board -- Places a Figure at a given Position on the Board
placeFig fig (r:rows) (row,col) = case row of
  0         -> replaceRow r fig col : rows
  otherwise -> r : placeFig fig rows ((row-1),col)

remFig :: Pos -> Board -> Board -- Removes (sets to E) a Figure at a given Position on the Board
remFig pos b = placeFig E b pos

isEmpty :: Pos -> Board -> Bool
isEmpty p b = returnFig p b == E

isWhite :: Pos -> Board -> Bool
isWhite p b = returnFig p b `elem` [W,WD]

isBlack :: Pos -> Board -> Bool
isBlack p b = returnFig p b `elem` [B,BD]

isPiece :: Pos -> Board -> Bool
isPiece p b = returnFig p b `elem` [B,W]

isKing :: Pos -> Board -> Bool
isKing p b = returnFig p b `elem` [BD,WD]

isValidPos :: Pos -> Bool
isValidPos (r,c) = all (`elem` [0..7]) [r,c]

isInvalidPos :: Pos -> Bool
isInvalidPos = (not.isValidPos)

isBlocked :: Pos -> Board -> Bool
isBlocked p@(r,c) b
  | isWhite p b && ((isEmpty (r-1,c+1) b) || (isEmpty (r-1,c-1) b)) = False
  | isBlack p b && ((isEmpty (r+1,c+1) b) || (isEmpty (r+1,c-1) b)) = False
  | otherwise = True

move :: Pos -> Pos -> Board -> Board
move p0@(r0,c0) p1 b
  | isInvalidPos p0 = error ""
  | isInvalidPos p1 = error ""
  | isEmpty p0 b = error ("Illegal move: There is no figure at the given Position: (" ++ show r0 ++ "," ++ show c0 ++ ").")
--move p0 p1 b = remFig p0 (placeFig (returnFig p0 b) b p1)

possibleMoves :: Pos -> Board -> [(Board, Pos)]
possibleMoves p b
  | isEmpty p b = []
  | isPiece p b  = pieceSimpleMoves b p ++ pieceComplexMoves  b p
  | isKing p b   = kingSimpleMoves b p ++ kingComplexMoves b p
  {-| isWhite p b = whitePieceMoves p b
  | isBlack p b = blackPieceMoves p b-}
  | otherwise = error "Figure is not Empty, nor White or Black"
  

whitePieceSimpleMoves :: Pos -> Board -> [Pos]
whitePieceSimpleMoves p@(r,c) b = filter isEmptyPos $ filter isValidPos [(r-1,c+1),(r-1,c-1)] where
  isEmptyPos p = isEmpty p b

blackPieceSimpleMoves :: Pos -> Board -> [Pos]
blackPieceSimpleMoves p@(r,c) b = filter isEmptyPos $ filter isValidPos [(r+1,c+1),(r+1,c-1)] where
  isEmptyPos p = isEmpty p b

blackNghbrs :: Pos -> Board -> [Pos]
blackNghbrs p@(r,c) b = 
  filter isBlackPos $ filter isValidPos [(r-1,c+1), (r-1,c-1), (r+1,c-1),(r+1,c+1)] where
    isBlackPos pos = isBlack pos b  

whiteNghbrs :: Pos -> Board -> [Pos]
whiteNghbrs p@(r,c) b = 
  filter isWhitePos $ filter isValidPos [(r-1,c+1), (r-1,c-1), (r+1,c-1),(r+1,c+1)] where
    isWhitePos pos = isWhite pos b  

whitePieceComplexMoves :: Pos -> Board -> [Pos]    
whitePieceComplexMoves p b = killMoves b p (blackNghbrs p b)

blackPieceComplexMoves :: Pos -> Board -> [Pos]    
blackPieceComplexMoves p b = killMoves b p (whiteNghbrs p b)

blackPieceMoves :: Pos -> Board -> [Pos]
blackPieceMoves p b = blackPieceSimpleMoves p b ++ blackPieceComplexMoves p b

whitePieceMoves :: Pos -> Board -> [Pos]
whitePieceMoves p b = whitePieceSimpleMoves p b ++ whitePieceComplexMoves p b

killMoves :: Board -> Pos -> [Pos] -> [Pos]
killMoves b p nghbrs = filter isValidPos $ filter isEmptyPos $ map (countPos p) nghbrs where
  isEmptyPos p = isEmpty p b

killMoves2 :: ((Board, Pos), [Pos]) -> [((Board, Pos), [Pos])]
killMoves2 ((_, _), []) = []
killMoves2 ((b, p), nghbrs) = [((killMove p pos b, pos),(gNghbrs pos (killMove p pos b))) | pos <- map (countPos p) nghbrs, isValidPos pos, isEmpty pos b]  

killMove :: Pos -> Pos -> Board -> Board
killMove p0 p1 b
  | isWhite p0 b = whitePieceKillMove p0 p1 b
  | isBlack p0 b = blackPieceKillMove p0 p1 b

pieceSimpleMoves :: Board -> Pos -> [(Board, Pos)]
pieceSimpleMoves b (row, col) = [(pieceSimpleMove (row, col) (x, y) b, (x, y)) | x <- [(row - 1)], y <- [(col - 1), (col + 1)], isValidPos (x, y), isEmpty (x, y) b]  

pieceSimpleMove :: Pos -> Pos -> Board -> Board
pieceSimpleMove p0 p1 b
  | isWhite p0 b = placeFig W (remFig p0 b) p1 
  | isBlack p0 b = placeFig B (remFig p0 b) p1

pieceComplexMoves :: Board -> Pos -> [(Board, Pos)]
pieceComplexMoves b p = map fst (killMoves2 ((b, p), (gNghbrs p b)))

kingSimpleMoves :: Board -> Pos -> [(Board, Pos)]
kingSimpleMoves b (row, col) = [(b, (x, y)) | x <- [0..7], y <- [0..7], row - col == x - y || row + col == x + y, isEmptyLine b (row, col) (x, y)]
kingComplexMoves :: Board -> Pos -> [(Board, Pos)]
kingComplexMoves b p = []

gNghbrs :: Pos -> Board -> [Pos]
gNghbrs p b
  | isWhite p b = blackNghbrs p b
  | isBlack p b = whiteNghbrs p b  

countRow :: Int -> Int -> Int
countRow row neighborRow = 2 * neighborRow - row

countCol :: Int -> Int -> Int
countCol col neighborCol = 2 * neighborCol - col

countPos :: Pos -> Pos -> Pos
countPos (row, col) (neighborRow, neighborCol) = ((countRow row neighborRow), (countCol col neighborCol)) 

numberOfWhites :: Board -> Int
numberOfWhites b = sum $ concat [[if figure `elem` [W,WD] then 1 else 0 | figure <- row] | row <- b]

numberOfBlacks :: Board -> Int
numberOfBlacks b = sum $ concat [[if figure `elem` [B,BD] then 1 else 0 | figure <- row] | row <- b]

killedPos :: Pos -> Pos -> Pos
killedPos (r1, c1) (r2, c2) = (div (r1 + r2) 2, div (c1 + c2) 2) 

whitePieceKillMove :: Pos -> Pos -> Board -> Board
whitePieceKillMove p0 p1 b = (remFig (killedPos p0 p1) (placeFig W (remFig p0 b) p1))

blackPieceKillMove :: Pos -> Pos -> Board -> Board
blackPieceKillMove p0 p1 b = (remFig (killedPos p0 p1) (placeFig B (remFig p0 b) p1))

isEmptyLine :: Board -> Pos -> Pos -> Bool
isEmptyLine b p1 p2 = all ((flip isEmpty) b) (createLine b p1 p2)

createLine :: Board -> Pos -> Pos -> [Pos]
createLine b (r1, c1) (r2, c2) = [(x, y) | x <- [(min r1 r2)..(max r1 r2)], y <- [(min c1 c2)..(max c1 c2)], x /= r1, y /= c1, r1 - c1 == x - y || r1 + c1 == x + y]