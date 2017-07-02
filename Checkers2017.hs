type Col = Int
type Row = Int
data Position = Position { row :: Row,
                           col :: Col
                         } deriving (Show,Read)
data State = Dead | Alive deriving (Show, Read)
data Figure = Figure { state :: State
                     , pos   :: Position
                     } deriving (Show, Read)