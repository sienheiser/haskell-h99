
h1 :: a -> [a] -> [(a,a)]
h1 x ys = [(x,y) | y <- ys]

h2 :: [a] -> [(a,a)]
h2 [] = []
h2 (x:xs) = h1 x xs ++ h2 xs

