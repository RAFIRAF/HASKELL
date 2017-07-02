data Figura = Figura Rodzaj Kolor deriving Show
data Rodzaj = Pionek | Damka deriving Show
data Kolor  = Bialy  | Czarny deriving Show

type Kolumna   = Int
type Wiersz    = Int
type AdresPola = (Kolumna, Wiersz)


data StanPola  = Wolne | Zajete Figura deriving Show

data Pole = Pole AdresPola StanPola deriving Show
type Plansza = [[Pole]]         

--data StanPola = Wolne | Zajete
poleNastepne :: AdresPola -> AdresPola
poleNastepne pierwotnePole@(kolumna, wiersz)
  | kolumna == 7 = (0,wiersz+1)
  | otherwise = (kolumna+1,wiersz)
poleWDol :: AdresPola -> AdresPola
poleWDol pierwotnePole@(kolumna, wiersz) = (kolumna,wiersz-1)
poleWGore :: AdresPola -> AdresPola
poleWGore pierwotnePole@(kolumna, wiersz) = (kolumna,wiersz+1)
poleWPrawo :: AdresPola -> AdresPola
poleWPrawo pierwotnePole@(kolumna, wiersz) = (kolumna+1,wiersz)
poleWLewo :: AdresPola -> AdresPola
poleWLewo pierwotnePole@(kolumna, wiersz) = (kolumna-1,wiersz)

--poleUkosGoraLewo :: AdresPola -> AdresPola
--nastepnePole pierwotnePole@(kolumna, wiersz) = (kolumna-1,wiersz)
--poleUkosGoraPrawo :: AdresPola -> AdresPola
--nastepnePole pierwotnePole@(kolumna, wiersz) = (kolumna-1,wiersz)
--poleUkosDolPrawo :: AdresPola -> AdresPola
--nastepnePole pierwotnePole@(kolumna, wiersz) = (kolumna-1,wiersz)
--poleUkosDolLewo :: AdresPola -> AdresPola
--nastepnePole pierwotnePole@(kolumna, wiersz) = (kolumna-1,wiersz)

initialBoardStr = ".b.b.b.b\n\
                  \b.b.b.b.\n\
                  \.b.b.b.b\n\
                  \........\n\
                  \........\n\
                  \w.w.w.w.\n\
                  \.w.w.w.w\n\
                  \w.w.w.w."

charNaStanPola :: Char -> StanPola
charNaStanPola 'w' = (Zajete (Figura Pionek Bialy)) 
charNaStanPola 'W' = (Zajete (Figura Damka Bialy))
charNaStanPola 'b' = (Zajete (Figura Pionek Czarny))
charNaStanPola 'B' = (Zajete (Figura Damka Czarny))
charNaStanPola '.' = Wolne

stanPolaNaChar :: StanPola -> Char 
stanPolaNaChar (Zajete (Figura Pionek Bialy))  = 'w'
stanPolaNaChar (Zajete (Figura Damka Bialy))   = 'W'
stanPolaNaChar (Zajete (Figura Pionek Czarny)) = 'b'
stanPolaNaChar (Zajete (Figura Damka Czarny))  = 'B'
stanPolaNaChar Wolne                       = '.'

stringNAfigury :: String -> [Figura]
stringNAfigury strin = map charNaStanPola strin
