type Plansza = [Pole] 
data StanPola = Zajete Figura | Wolne deriving Show
data Figura = Pionek Kolor | Damka Kolor deriving Show
data Pole = Pole { wiersz    :: Int
                 , kolumna   :: Int
                 , stan      :: StanPola
                 } deriving Show
data Kolor = Bialy | Czarny deriving Show
