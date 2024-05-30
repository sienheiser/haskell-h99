slice :: [a] -> Int -> Int -> [a]
slice xs i k | (i < k) && (k < length xs) = drop (i-1) (take k xs)
             | otherwise = error "k out of bounds"

