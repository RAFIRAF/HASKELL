data Figura = Pionek Kolor | Damka Kolor deriving (Show)
data Kolor = Bialy | Czarny deriving (Show, Enum)
type Plansza = [[Pole]]
type Pole = (Kolumna, Wiersz)
type Kolumna = Int
type Wiersz = Int  