isBigGang :: Int -> (Bool, String)
isBigGang x = let n = 9 in (x > n, "Compared the gang size to " ++ show n ++ ".")

applyLog :: (a, String) -> (a -> (b, String)) -> (b, String)
applyLog (x,log) f = let (y, newLog) = f x in (y, log++newLog)