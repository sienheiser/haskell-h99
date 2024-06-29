
comb :: Int -> [a] -> [[a]]
comb _ [] = []
comb 1 xs = [[x] | x <- xs]
comb n (x:xs) = map (x:) (comb (n-1) xs) ++ comb n xs

