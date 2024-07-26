put :: Ord a => a -> [a] -> [a]
put _ [] = []
put n (x:xs) | n < x = x : put n xs
             | otherwise = x : n : xs

collapse :: [Int] -> [Int]
collapse [x,y] = [x+y]
collapse [x] = [x]
collapse (x:y:xs) = collapse (put (x+y) xs)

data Tree a = Empty | Leaf a | Node Int (Tree a) (Tree a) deriving (Show)

genTree :: [Int] -> Tree Int -> Tree Int
genTree [] t = t
genTree (x:y:xs) Empty = genTree xs (Node (x+y) (Leaf x) (Leaf y))
genTree (x:xs) (Node n t1 t2) = genTree xs (Node (x+n) (Leaf x) (Node n t1 t2))

genTreeV2 :: [(a,Int)] -> Tree a -> Tree a 
genTreeV2 [] t = t
genTreeV2 ((x,n):(y,m):xs) Empty = genTreeV2 xs (Node (n+m) (Leaf x) (Leaf y))
genTreeV2 ((x,n):xs) (Node m t1 t2) = genTreeV2 xs (Node (n+m) (Leaf x) (Node m t1 t2))

encode :: Tree a -> String
encode (Node n t1 t2) | 
