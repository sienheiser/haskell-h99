combinations :: Int -> [a] -> [[a]]
combinations 1 xs = [[x] | x <- xs]
combinations n (x:xs) | length (x:xs) == n = [(x:xs)] 
                      | length (x:xs) < n = []  
                      | otherwise = map (x:) (combinations (n-1) xs) ++ combinations n xs

