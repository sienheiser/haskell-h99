data Tree a = Empty | Leaf Int a | Node Int (Tree a) (Tree a) deriving (Show)

instance Eq (Tree a) where
    (Leaf n _) == (Leaf m _) = n == m 
    (Leaf n _) == (Node m _ _) = n == m
    (Node n _ _) == (Leaf m _) = n == m
    (Node n _ _) == (Node m _ _) = n == m 
    Empty == Empty = True
    _ == _ = False

instance Ord (Tree a) where
    (Leaf n _) `compare` (Leaf m _) = n `compare` m
    (Leaf n _) `compare` (Node m _ _)  = n `compare` m
    (Node n _ _) `compare` (Leaf m _) = n `compare` m
    (Node n _ _) `compare` (Node m _ _) = n `compare` m

addTree :: Tree a -> Tree a -> Tree a
addTree Empty t = t
addTree t Empty = t
addTree (Leaf n x) (Leaf m y) = Node (n + m) (Leaf n x) (Leaf m y)
addTree (Leaf n x) (Node m t1 t2) = Node (n + m) (Leaf n x) (Node m t1 t2)
addTree (Node n t1 t2) (Leaf m x) = Node (n + m) (Node n t1 t2) (Leaf m x)
addTree (Node n t1 t2) (Node m t3 t4) = Node (n + m) (Node n t1 t2) (Node m t3 t4)

makeList :: [(a,Int)] -> [Tree a]
makeList xs = [(Leaf n x) | (x,n) <- xs]

put :: Tree a -> [Tree a] -> [Tree a]
put t1 [] = [t1]
put t1 (t:ts) | t1 > t = t : put t1 ts
              | otherwise = t1 : t : ts


collapse :: [Tree a] -> [Tree a]
collapse [t1,t2] = [addTree t1 t2]
collapse [t] = [t]
collapse (t1:t2:ts) = collapse (put (addTree t1 t2) ts)

extractTree :: [Tree a] -> Tree a
extractTree [t1] = t1
extractTree _ = error "This function only expects lists with one tree in it"

encode :: String -> Tree a -> [(a,String)]
encode ns (Leaf _ x) = [(x,ns)]
encode ns (Node _ t1 t2) = encode (ns++"0") t1 ++ encode (ns++"1") t2

hencode :: Tree a -> [(a,String)]
hencode t1 = encode "" t1

huffman :: [(Char,Int)] -> [(Char,String)]
huffman = hencode . extractTree . collapse . makeList 

testCase :: [(Char,Int)]
testCase = [('e',5),('d',6),('c',6),('b',7),('a',15)]

l1 = Leaf 5 'e'
l2 = Leaf 6 'd'
testCase2 :: [(Char,Int)]
testCase2 =  [('c',6),('b',7),('a',15)]

testCase3 :: [(Char,Int)]
testCase3 = [('f',5),('e',9),('c',12),('b',13),('d',16),('a',45)]


