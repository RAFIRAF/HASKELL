data Position     = Position Column Row deriving (Show)
type Column       = Int
type Row          = Int
type Checkerboard = [[Position]]

move :: Position -> Position -> Maybe Checkerboard
move (Position Column Row) (Position Column Row)