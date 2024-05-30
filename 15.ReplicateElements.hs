
repli :: [a] -> Int -> [a]
repli [] _ = []
repli (x:xs) n  = (rep x n) ++ repli xs n
    where
        rep :: a -> Int -> [a] 
        rep _ 0 = []
        rep x n = x : rep x (n-1)
