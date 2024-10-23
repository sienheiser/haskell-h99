dropEvery :: [a] -> Int -> [a]
dropEvery xs n = dropEvery' xs n 1
    where
        dropEvery' :: [a] -> Int -> Int -> [a]
        dropEvery' [] _ _ = []
        dropEvery' (y:ys) n i | i `mod` n == 0 = dropEvery' ys n (i+1)
                              | otherwise = y : dropEvery' ys n (i+1)



