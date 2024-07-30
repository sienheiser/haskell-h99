data Tre a = Empty | Leaf Int a | Node Int (Tre a) (Tre a) deriving (Show)

instance Eq (Tre a) where
    (Leaf n _) == (Leaf m _) = n == m 
    (Leaf n _) == (Node m _ _) = n == m
    (Node n _ _) == (Node m _ _) = n == m 
    Empty == Empty = True
    _ == _ = False

instance Ord (Tre a) where
    (Leaf n _) `compare` (Leaf m _) = n `compare` m
    (Leaf n x) `compare` (Node m _ _)  = n `compare` m
    (Node n _ _) `compare` (Node m _ _) = n `compare` m


t1 :: Tre Char
t1 = Node 1 (Leaf 2 'a') (Leaf 3 'b')

t2 :: Tre Char
t2 = Node 1 (Leaf 2 'a') (Leaf 3 'b')


makeList :: [(a,Int)] -> [Tre a]
makeList xs = [(Leaf n x) | (x,n) <- xs]

put :: Tre a -> [Tre a] -> [Tre a]
put _ [] = []
put t1 (t:ts) | t1 < t = t : put t1 ts
              | otherwise = t : t1 : ts

collapse :: [Tre a] -> [Tre a]
collapse [t] = t
collapse (t1:t2:ts) = collapse (put )

