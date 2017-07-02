import qualified Data.Map as Map
data PlayerState = FirstTeam | Bench deriving (Show, Eq)
type Surname = String

type PlayerMap = Map.Map Int (PlayerState, Surname)

playersLookup :: Int -> PlayerMap -> Either String Surname
playersLookup playerNumber map =
  case Map.lookup playerNumber map of
    Nothing -> Left $ "No player wearing shirt number " ++ show playerNumber
    Just (state, surname) -> if state /= Bench
      then Right surname
      else Left $ "Player number " ++ show playerNumber ++ " is on the bench!"

matchPlayers :: PlayerMap
matchPlayers = Map.fromList
  [(1,(FirstTeam,"Fabianski"))
  ,(13,(Bench,"Stepinski"))
  ,(7,(FirstTeam,"Milik"))
  ,(9,(FirstTeam,"Lewandowski"))
  ,(21,(FirstTeam,"Kapustka"))
  ,(12,(Bench,"Boruc"))
  ]
