--data Wiersz = 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8
--data Kolumna = 'A' | 'B' | 'C' | 'D' | 'E' | 'F' | 'G' | 'H'
--data Pole = Kolumna | Wiersz
--data Szachownica = [[Pole]]

--pola = [(kolumna, wiersz) | kolumna <- ['A'..'H'], wiersz <- [1..8]]
--pionki = replicate 24 "Pionek"
--polaZPionkami = zip pola pionki
--dodajPionek pole
--data Stan = BialyPionek | CzarnyPionek | BialaDamka | CzarnaDamka | Pusty deriving Show
--data Wiersz = Wiersz Int deriving Show
--data Kolumna = Kolumna Char deriving Show
--data Pole = Pole (Kolumna, Wiersz, Stan) deriving Show
--move :: (Char,Int)->(Char,Int)->IO
 

-- co na WEJŚCIU co na WYJŚCIU -- ???

--zajecia: data Figura = F {kolor::Char,typ::Char} deriving Show
-- F 'b' 'p'

type Szachownica = [Pole]
type Pole        = (NumerPola, Stan)
data NumerPola   = A1 | B1 | C1 | D1 | E1 | F1 | G1 | H1 |
                   A2 | B2 | C2 | D2 | E2 | F2 | G2 | H2 |
                   A3 | B3 | C3 | D3 | E3 | F3 | G3 | H3 |
                   A4 | B4 | C4 | D4 | E4 | F4 | G4 | H4 |
                   A5 | B5 | C5 | D5 | E5 | F5 | G5 | H5 |
                   A6 | B6 | C6 | D6 | E6 | F6 | G6 | H6 |
                   A7 | B7 | C7 | D7 | E7 | F7 | G7 | H7 |
                   A8 | B8 | C8 | D8 | E8 | F8 | G8 | H8 deriving (Show, Enum)     
--type Wiersz      = Int
--type Kolumna     = Char
--type Numer       = (Kolumna, Wiersz)
data Stan        = Zajety | Pusty deriving Show
type Zajety      = Figura
data Figura      = Pionek | Damka deriving Show
data Pionek      = BialyPionek | CzarnyPionek
data Damka       = BialaDamka  | CzarnaDamka

--wczytajSzachownice :: String -> Szachownica
--wczytajSzachownice str = if length str < 64 
--  then error "Podano za malo danych do szachwonicy"
--  else str


--pokazSzachownice sz = putStrLn . 
