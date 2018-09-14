module ExtendedPrelude where

maybeHead :: [a] -> Maybe a
maybeHead (x:_) = Just x
maybeHead [] = Nothing

readInt :: String -> Int
readInt str = read str :: Int
