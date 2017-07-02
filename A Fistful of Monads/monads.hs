applyMaybe :: Maybe a -> (a -> Maybe b) -> Maybe b
applyMaybe Nothing _f = Nothing
applyMaybe (Just x) f = f x