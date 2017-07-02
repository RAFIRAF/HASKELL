import qualified Data.Map as Map
type Code = String
data LockerState = Free | Taken deriving (Show, Eq)
type LockersMap = Map.Map Int (LockerState,Code)

lockerLookup :: Int -> LockersMap -> Either String Code
lockerLookup lockerNumber map =
  case Map.lookup lockerNumber map of
    Nothing -> Left "no such locker"
    Just (state,code) -> if state /= Taken
      then Right code
      else Left "locker taken"

lockers :: LockersMap
lockers = Map.fromList [(100,(Taken,"KKK ")),(101,(Free,"ABC"))]      