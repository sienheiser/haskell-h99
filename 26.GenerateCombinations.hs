
h1 :: a -> [a] -> [(a,a)]
h1 x ys = [(x,y) | y <- ys]

h2 :: [a] -> [(a,a)]
h2 [] = []
h2 (x:xs) = h1 x xs ++ h2 xs



g1 :: a -> a -> [a] -> [(a,a,a)]
g1 x y zs = [(x,y,z) | z <- zs]

g2 :: a -> [a] -> [(a,a,a)]
g2 x []  = []
g2 x (y:ys) = g1 x y ys ++ g2 x ys

g3 :: [a] -> [(a,a,a)]
g3 [] = []
g3 (x:xs) = g2 x xs ++ g3 xs

f1 :: a -> a -> a -> [a] -> [(a,a,a,a)]
f1 x y z ws = [(x,y,z,w) | w <- ws]

f2 :: a -> a -> [a] -> [(a,a,a,a)]
f2 x y [] = []
f2 x y (z:zs)  = f1 x y z zs ++ f2 x y zs

f3 :: a -> [a] -> [(a,a,a,a)]
f3 x [] = []
f3 x (y:ys) = f2 x y ys ++ f3 x ys

f4 :: [a] -> [(a,a,a,a)]
f4 [] = []
f4 (x:xs) = f3 x xs ++ f4 xs

w1 :: a -> [a] -> [[a]]
w1 x ys = [[x,y] | y <- ys]

w2 :: [a] -> [[a]]
w2 [] = []
w2 (x:xs) = w1 x xs ++ w2 xs


combinations :: Int -> [a] -> [[a]]
combinations _ [] = []
combinations 0 _ = [[]]
combinations n (x:xs) = 
