data Tre a = Empty | Leaf Int a | Node Int (Tre a) (Tre a) deriving (Show)

instance Eq (Tre a) where
    (Leaf n _) == (Leaf m _) = n == m 
    (Leaf n _) == (Node m _ _) = n == m
    (Node n _ _) == (Leaf m _) = n == m
    (Node n _ _) == (Node m _ _) = n == m 
    Empty == Empty = True
    _ == _ = False

instance Ord (Tre a) where
    (Leaf n _) `compare` (Leaf m _) = n `compare` m
    (Leaf n _) `compare` (Node m _ _)  = n `compare` m
    (Node n _ _) `compare` (Leaf m _) = n `compare` m
    (Node n _ _) `compare` (Node m _ _) = n `compare` m

addTree :: Tre a -> Tre a -> Tre a
addTree Empty t = t
addTree t Empty = t
addTree (Leaf n x) (Leaf m y) = Node (n + m) (Leaf n x) (Leaf m y)
addTree (Leaf n x) (Node m t1 t2) = Node (n + m) (Leaf n x) (Node m t1 t2)
addTree (Node n t1 t2) (Leaf m x) = Node (n + m) (Node n t1 t2) (Leaf m x)
addTree (Node n t1 t2) (Node m t3 t4) = Node (n + m) (Node n t1 t2) (Node m t3 t4)

t1 :: Tre Char
t1 = Node 1 (Leaf 2 'a') (Leaf 3 'b')

t2 :: Tre Char
t2 = Node 1 (Leaf 2 'a') (Leaf 3 'b')


makeList :: [(a,Int)] -> [Tre a]
makeList xs = [(Leaf n x) | (x,n) <- xs]

put :: Tre a -> [Tre a] -> [Tre a]
put t1 [] = [t1]
put t1 (t:ts) | t1 > t = t : put t1 ts
              | otherwise = t1 : t : ts



collapse :: [Tre a] -> [Tre a]
collapse [t1,t2] = [addTree t1 t2]
collapse [t] = [t]
collapse (t1:t2:ts) = collapse (put (addTree t1 t2) ts)

testCase :: [(Char,Int)]
testCase = [('e',5),('d',6),('c',6),('b',7),('a',15)]

l1 = Leaf 5 'e'
l2 = Leaf 6 'd'
testCase2 :: [(Char,Int)]
testCase2 =  [('c',6),('b',7),('a',15)]

