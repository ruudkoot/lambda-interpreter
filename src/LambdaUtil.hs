module LambdaUtil where

splitOn :: (Eq a) => (a -> Bool) -> [a] -> ([a],[a])
splitOn p []     = ([],[])
splitOn p (a:as) | p a       = ([], as)
                 | otherwise = let (l, r) = splitOn p as in (a:l, r)

