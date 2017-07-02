module Main where

import Data.List
import Data.Tree
import Data.Maybe
import Data.Ord
import FunkcjePomocnicze

data Figura  = PustePole | BialaDama | CzarnaDama | BialyPionek | CzarnyPionek deriving (Eq, Ord, Show, Read, Bounded, Enum)
data Gracz   = BialyGracz | CzarnyGracz deriving (Eq, Ord, Show, Read, Bounded, Enum)
type Plansza = [[Figura]]
type Pozycja = (Wiersz, Kolumna)
type Wiersz  = Int
type Kolumna = Int
type Sasiad = Pozycja
type Sasiedzi = [Sasiad]
type Ruch = (Plansza, Pozycja)
type Ruchy = [Ruch]
type Drzewo = Tree

---------------------------------------------------------------------------

charWFigure :: Char -> Figura -- Zwraca Figurę odpowiadającą danemu Charowi
charWFigure c = case c of
  '.' -> PustePole
  'W' -> BialaDama
  'B' -> CzarnaDama
  'w' -> BialyPionek
  'b' -> CzarnyPionek
   --x  -> error ("Dla wprowadzonego znaku: " ++ [x] ++ " nie istnieje odpowiadająca figura."

figuraWChar :: Figura -> Char -- Zwraca Char odpowiadający danej Figurze
figuraWChar fig = case fig of 
  PustePole    -> '.'
  BialaDama    -> 'W'
  CzarnaDama   -> 'B'
  BialyPionek  -> 'w'
  CzarnyPionek -> 'b'
  --x  -> error ("Dla wprowadzonego znaku: " ++ [x] ++ " nie istnieje odpowiadająca figura."

---------------------------------------------------------------------------

stringNaFigury :: String -> [Figura] -- Zwraca listę figur odpowiadającą danemu Stringowi
stringNaFigury = mapuj charWFigure

figuryNaString :: [Figura] -> String -- Zwraca Stringa odpowiadającego danej liście Figur
figuryNaString = intersperse ' ' . mapuj figuraWChar

---------------------------------------------------------------------------

stringNaPlansze :: String -> Plansza -- Zwraca Planszę odpowiadającą danemu Stringowi
stringNaPlansze = mapuj stringNaFigury . lines

planszaNaString :: Plansza -> [String] -- Zwraca listę Stringów odpowiadającą danej Planszy
planszaNaString = mapuj figuryNaString

---------------------------------------------------------------------------

pokazPlansze :: Plansza -> String -- Zwraca Stringa sformatowanego w sposób czytelny dla gracza
pokazPlansze =
  let 
    dodajNryWierszy  = zipWith dodajNrWiersza [0..7] where dodajNrWiersza nr wiersz = (show nr) ++ " " ++ wiersz
    dodajNryKolumn p = ["  0 1 2 3 4 5 6 7"] ++ p
  in
    unlines . dodajNryKolumn . dodajNryWierszy . mapuj figuryNaString

---------------------------------------------------------------------------

dajFig :: Plansza -> Pozycja -> Figura -- Zwraca Figurę z danej pozycji na danej Planszy
dajFig plansza (wiersz, kolumna) = plansza !! wiersz !! kolumna

zamienFigury :: Figura -> [Figura] -> Kolumna -> [Figura] -- Zamienia jedną Figurę za drugą w danej LIŚCIE Figur (czyli w danym wierszu)
zamienFigury nowaFigura wierszFigur@(pierwszaFigura:pozostaleFigury) kolumna = case kolumna of
  0 -> nowaFigura     : pozostaleFigury
  k -> pierwszaFigura : zamienFigury nowaFigura pozostaleFigury (k-1)

ustawFigure :: Figura -> Plansza -> Pozycja -> Plansza
ustawFigure nowaFigura plansza@(pierwszyWiersz:pozostaleWiersze) nowaPozycja@(wiersz, kolumna) = case wiersz of
  0 -> zamienFigury nowaFigura pierwszyWiersz kolumna : pozostaleWiersze
  w -> pierwszyWiersz : ustawFigure nowaFigura pozostaleWiersze ((wiersz-1), kolumna)

---------------------------------------------------------------------------

zliczBIALE :: Plansza -> Integer -- Zwraca liczbę BIAŁYCH Figur na danej Planszy
zliczBIALE plansza = sum $ concat [[if figura `elem` [BialyPionek,BialaDama] then 1 else 0 | figura <- wiersz] | wiersz <- plansza]

zliczCZARNE :: Plansza -> Integer -- Zwraca liczbę CZARNYCH Figur na danej Planszy
zliczCZARNE plansza = sum $ concat [[if figura `elem` [CzarnyPionek,CzarnaDama] then 1 else 0 | figura <- wiersz] | wiersz <- plansza]

---------------------------------------------------------------------------

pozycjeSkosnePuste :: Plansza -> Pozycja -> Pozycja -> Bool
pozycjeSkosnePuste plansza pozycja1 pozycja2 = all (czyPuste plansza) (pozycjeSkosne plansza pozycja1 pozycja2)

pozycjeSkosne :: Plansza -> Pozycja -> Pozycja -> [Pozycja]
pozycjeSkosne plansza (r1, c1) (r2, c2) = [(x, y) | x <- [(min r1 r2)..(max r1 r2)],
 y <- [(min c1 c2)..(max c1 c2)], x /= r1, y /= c1, r1 - c1 == x - y || r1 + c1 == x + y]

czyPuste :: Plansza -> Pozycja -> Bool -- Sprawdza, czy dana Pozycja na danej Planszy jest PUSTA
czyPuste plansza pozycja = (dajFig plansza pozycja) == PustePole

czyBiale :: Plansza -> Pozycja -> Bool -- Sprawdza, czy dana Figura na danej Planszy jest koloru BIAŁEGO
czyBiale plansza pozycja = figura `elem` [BialyPionek, BialaDama] where figura = dajFig plansza pozycja

czyCzarne :: Plansza -> Pozycja -> Bool -- Sprawdza, czy dana Figura na danej Planszy jest koloru CZARNEGO
czyCzarne plansza pozycja = figura `elem` [CzarnyPionek, CzarnaDama] where figura = dajFig plansza pozycja

czyPionek :: Plansza -> Pozycja -> Bool -- Sprawdza, czy dana Figura na danej Planszy jest PIONKIEM
czyPionek plansza pozycja = figura `elem` [CzarnyPionek, BialyPionek] where figura = dajFig plansza pozycja

czyDama :: Plansza -> Pozycja -> Bool -- Sprawdza, czy dana Figura na danej Planszy jest DAMĄ
czyDama plansza pozycja = figura `elem` [BialaDama, CzarnaDama] where figura = dajFig plansza pozycja

czyPrawidlowa :: Pozycja -> Bool -- Sprawdza, czy dana Pozycja na danej Planszy jest PRAWIDŁOWA
czyPrawidlowa pozycja@(wierszyk, kolumienka) = all (`elem` [0..7]) [wierszyk, kolumienka]

czyPrawidlowyRuch :: Plansza -> Pozycja -> Pozycja -> Bool
czyPrawidlowyRuch plansza p0 p1 = (czyPrawidlowa p1) && (elem p1 $ mapuj drugiZKrotki $ dajRuchy (plansza, p0))

obliczPozycje :: Pozycja -> Pozycja -> Pozycja
obliczPozycje (wiersz, kolumna) (sasiedniWiersz, sasiedniaKolumna) = 
  ((obliczIndeks wiersz sasiedniWiersz), (obliczIndeks kolumna sasiedniaKolumna))
  where
    obliczIndeks indeks sasiedniIndeks = sasiedniIndeks + signum (sasiedniIndeks - indeks)

ruchProsty :: Plansza -> Pozycja -> Pozycja -> Plansza
ruchProsty plansza p0 p1 = ustawFigure figura (ustawFigure PustePole plansza p0) p1 where figura = dajFig plansza p0

ruchPionekZbija :: Plansza -> Pozycja -> Pozycja -> Plansza
ruchPionekZbija plansza p0 p1 = 
  (ustawFigure PustePole (ustawFigure figura (ustawFigure PustePole plansza p0) p1) (zajetePole p0 p1))
  where
    figura = dajFig plansza p0
    zajetePole (wiersz1, kolumna1) (wiersz2, kolumna2) = (quot (wiersz1 + wiersz2) 2, quot (kolumna1 + kolumna2) 2)  

dajRuchyPionkaZbijajace :: (Ruch, Sasiedzi) -> [(Ruch, Sasiedzi)]
dajRuchyPionkaZbijajace (_, []) = []
dajRuchyPionkaZbijajace ((plansza, pozycja0), sasiedzi) = 
  aa ++ concatMap dajRuchyPionkaZbijajace aa
  where aa = [((ruchPionekZbija plansza pozycja0 pozycja, pozycja), (dajSasiadow (ruchPionekZbija plansza pozycja0 pozycja) pozycja)) | pozycja <- mapuj (obliczPozycje pozycja0) sasiedzi, czyPrawidlowa pozycja, czyPuste plansza pozycja]

ruchDamyZbijajacy :: Plansza -> Pozycja -> Pozycja -> Plansza
ruchDamyZbijajacy plansza p0 p1 = 
  (ustawFigure PustePole (ustawFigure figura (ustawFigure PustePole plansza p0) p1) zajetePole)
  where 
        figura = dajFig plansza p0
        zajetePole = head (filtruj (\p -> containsInTheLine p p0 p1) (dajSasiadow plansza p0))
        containsInTheLine (r, c) (fr, fc) (tr, tc) = r < (max fr tr) && r > (min fr tr) && c < (max fc tc) && c > (min fc tc)       

dajRuchyDamyZbijajce :: (Ruch, Sasiedzi) -> [(Ruch, Sasiedzi)]
dajRuchyDamyZbijajce (_, []) = []
dajRuchyDamyZbijajce ((plansza, pozycja0), sasiedzi) = 
  aa ++ concatMap dajRuchyDamyZbijajce aa
  where
        aa = [((ruchDamyZbijajacy plansza pozycja0 pozycja, pozycja), (dajSasiadow (ruchDamyZbijajacy plansza pozycja0 pozycja) pozycja)) | pozycja <- mapuj (obliczPozycje pozycja0) sasiedzi, czyPrawidlowa pozycja, czyPuste plansza pozycja]

dajSasiadow :: Plansza -> Pozycja -> Sasiedzi
dajSasiadow plansza (wiersz, kolumna)
  | czyPuste plansza (wiersz, kolumna) = []
  | czyPionek plansza (wiersz, kolumna) && czyBiale plansza (wiersz, kolumna) = [(x, y) | x <- [(wiersz - 1), (wiersz + 1)], y <- [(kolumna - 1), (kolumna + 1)],czyPrawidlowa (x, y), czyCzarne plansza (x, y)]
  | czyPionek plansza (wiersz, kolumna) && czyCzarne plansza (wiersz, kolumna) = [(x, y) | x <- [(wiersz - 1), (wiersz + 1)], y <- [(kolumna - 1), (kolumna + 1)],czyPrawidlowa (x, y), czyBiale plansza (x, y)]
  | czyDama plansza (wiersz, kolumna) && czyBiale plansza (wiersz, kolumna) = filtruj (czyCzarne plansza) [(x, y) | x <- [0..7], y <- [0..7],wiersz - kolumna == x - y || wiersz + kolumna == x + y, maTylkoJednegoCzarnegoPrzeciwnika (pozycjeSkosne plansza (wiersz, kolumna) (x, y))]
  | czyDama plansza (wiersz, kolumna) && czyCzarne plansza (wiersz, kolumna) = filtruj (czyBiale plansza) [(x, y) | x <- [0..7], y <- [0..7],wiersz - kolumna == x - y || wiersz + kolumna == x + y, maTylkoJednegoBialegoPrzeciwnika (pozycjeSkosne plansza (wiersz, kolumna) (x, y))]
  where
    maTylkoJednegoCzarnegoPrzeciwnika l = dlugosc (filtruj (czyCzarne plansza) l) == 1
    maTylkoJednegoBialegoPrzeciwnika l = dlugosc (filtruj (czyBiale  plansza) l) == 1    

dajRuchyPionkaZlozone :: Plansza -> Pozycja -> Ruchy
dajRuchyPionkaZlozone plansza pozycja = mapuj pierwszyZKrotki (dajRuchyPionkaZbijajace ((plansza, pozycja), (dajSasiadow plansza pozycja)))

dajRuchyPionkaProste :: Plansza -> Pozycja -> Ruchy
dajRuchyPionkaProste plansza (wiersz, kolumna) =
 [(ruchProsty plansza (wiersz, kolumna) (x, y), (x, y)) | x <- [if czyBiale plansza (wiersz, kolumna) then (wiersz - 1) else (wiersz + 1)], y <- [(kolumna - 1), (kolumna + 1)], czyPrawidlowa (x, y), czyPuste plansza (x, y)]

dajRuchyDamyZlozone :: Plansza -> Pozycja -> Ruchy
dajRuchyDamyZlozone plansza pozycja = mapuj pierwszyZKrotki (dajRuchyDamyZbijajce ((plansza, pozycja), (dajSasiadow plansza pozycja)))

dajRuchyDamyProste :: Plansza -> Pozycja -> Ruchy
dajRuchyDamyProste plansza (wiersz, kolumna) = [(ruchProsty plansza (wiersz, kolumna) (x, y), (x, y)) | x <- [0..7], y <- [0..7],wiersz - kolumna == x - y || wiersz + kolumna == x + y, pozycjeSkosnePuste plansza (wiersz, kolumna) (x, y)]

dajRuchy :: Ruch -> Ruchy
dajRuchy (plansza, pozycja)
  | czyPuste plansza pozycja = []
  | czyPionek plansza pozycja = zip (mapuj awansujFigure $ mapuj pierwszyZKrotki ruchyPionka) (mapuj drugiZKrotki ruchyPionka)
  | czyDama plansza pozycja = dajRuchyDamyProste plansza pozycja ++ dajRuchyDamyZlozone plansza pozycja
  where
    ruchyPionka = dajRuchyPionkaProste plansza pozycja ++ dajRuchyPionkaZlozone plansza pozycja

dajRuch :: Pozycja -> Ruchy -> Ruch
dajRuch pozycja ruch = ruch !! fromJust (elemIndex pozycja $ mapuj drugiZKrotki ruch)

menu plansza = do
  putStrLn "Wpisz numer i wcisnij [enter], aby wybrac:\n1. Gra Czlowiek - Czlowiek\n2. Gra Czlowiek - Komputer\n3. Zakoncz Program"
  wybor <- getLine
  case wybor of 
    "1" -> graCzlowiekKontraCzlowiek BialyGracz plansza
    "2" -> graCzlowiekKontraKomputer BialyGracz plansza
    "3" -> return ()
    otherwise -> menu plansza

graCzlowiekKontraCzlowiek BialyGracz plansza = do
  putStrLn "RUCH BIALYCH"
  putStrLn $ pokazPlansze plansza
  ruch <- wezRuch
  if (czyBiale plansza (pierwszyZKrotki ruch)) && (czyPrawidlowyRuch plansza (pierwszyZKrotki ruch) (drugiZKrotki ruch)) 
    then if (zliczBIALE (pierwszyZKrotki (dajRuch (drugiZKrotki ruch) (dajRuchy (plansza, (pierwszyZKrotki ruch)))))) == 0
      then putStrLn "WYGRALY BIALE"
      else graCzlowiekKontraCzlowiek CzarnyGracz (pierwszyZKrotki (dajRuch (drugiZKrotki ruch) (dajRuchy (plansza, (pierwszyZKrotki ruch)))))
    else putStrLn "ZLY RUCH!!!" >> graCzlowiekKontraCzlowiek BialyGracz plansza    

graCzlowiekKontraCzlowiek CzarnyGracz plansza = do
  putStrLn "RUCH CZARNYCH"
  putStrLn . pokazPlansze $ plansza
  ruch <- wezRuch
  if (czyCzarne plansza (pierwszyZKrotki ruch)) && (czyPrawidlowyRuch plansza (pierwszyZKrotki ruch) (drugiZKrotki ruch)) 
    then if (zliczBIALE (pierwszyZKrotki (dajRuch (drugiZKrotki ruch) (dajRuchy (plansza, (pierwszyZKrotki ruch)))))) == 0
      then putStrLn "CZARNE WYGRALY"
      else graCzlowiekKontraCzlowiek BialyGracz (pierwszyZKrotki (dajRuch (drugiZKrotki ruch) (dajRuchy (plansza, (pierwszyZKrotki ruch)))))
    else putStrLn "ZLY RUCH" >> graCzlowiekKontraCzlowiek CzarnyGracz plansza

graCzlowiekKontraKomputer BialyGracz plansza = do
  putStrLn "RUCH CZLOWIEKA:"
  putStr .  pokazPlansze $ plansza
  ruch <- wezRuch
  if (czyBiale plansza (pierwszyZKrotki ruch)) && (czyPrawidlowyRuch plansza (pierwszyZKrotki ruch) (drugiZKrotki ruch)) 
    then if (zliczBIALE (pierwszyZKrotki (dajRuch (drugiZKrotki ruch) (dajRuchy (plansza, (pierwszyZKrotki ruch)))))) == 0
      then putStrLn "CZLOWIEK WYGRAL"
      else graCzlowiekKontraKomputer CzarnyGracz (pierwszyZKrotki (dajRuch (drugiZKrotki ruch) (dajRuchy (plansza, (pierwszyZKrotki ruch)))))
    else putStrLn "ZLY RUCH!!!" >> graCzlowiekKontraKomputer BialyGracz plansza

graCzlowiekKontraKomputer CzarnyGracz plansza = do
  let ruchy = [dajRuchy (plansza, (wiersz, kolumna)) | wiersz <- [0..7], kolumna <- [0..7], czyCzarne plansza (wiersz, kolumna)]
  let bestBoard = minimumBy (comparing zliczBIALE) $ concatMap (mapuj pierwszyZKrotki) ruchy
  if zliczBIALE bestBoard == 0
    then putStrLn "KOMPUTER WYGRAL"
    else graCzlowiekKontraKomputer BialyGracz bestBoard

wezRuch = do
  putStrLn "POZYCJA FIGURY: "; p0 <- wezPozycje; putStrLn "POZYCJA DOCELOWA: "; p1 <- wezPozycje; return (p0, p1)

wezPozycje = do
  pozycja <- fmap read getLine :: IO Int
  return (quot pozycja 10, mod pozycja 10)

stringStartowy = ".b.b.b.b\nb.b.b.b.\n.b.b.b.b\n........\n........\nw.w.w.w.\n.w.w.w.w\nw.w.w.w."

planszaStartowa = stringNaPlansze stringStartowy

awansujFigure :: Plansza -> Plansza
awansujFigure plansza = 
  awansujCzarna (awansujBiala plansza)
  where 
    awansujBiala  plansza = [foldl (\acc x -> if (x == BialyPionek) then acc ++ [BialaDama] else acc ++ [x]) [] (head plansza)] ++ (tail plansza)
    awansujCzarna plansza = (init plansza) ++ [foldl (\acc x -> if (x == BialyPionek) then acc ++ [BialaDama] else acc ++ [x]) [] (last plansza)]

generujDrzewoGry :: Int -> Plansza -> Drzewo Ruchy
generujDrzewoGry n plansza
  | n == 0 = Node ruchy' []
  | otherwise = Node ruchy' $ mapuj (generujDrzewoGry (n - 1)) (mapuj pierwszyZKrotki ruchy')
  where
    ruchy' = concat [dajRuchy (plansza, (x, y)) | x <- [0..7], y <- [0..7], if (even n) then czyBiale plansza (x, y) else czyCzarne plansza (x, y)]

main = menu planszaStartowa

startGame = main