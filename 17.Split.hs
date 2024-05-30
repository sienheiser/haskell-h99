split :: [a] -> Int -> ([a],[a])
split xs n | length xs > n = (take n xs, drop n xs)
           | otherwise = error "Out of bounds"
