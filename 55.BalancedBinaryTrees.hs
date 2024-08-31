data Tree a = Nil | Leaf a | Branch a (Tree a) (Tree a)
  deriving (Show,Eq)

instance Eq (Tree a) where
    (Leaf n _) == (Leaf m _) = n == m 
    (Leaf n _) == (Node m _ _) = n == m
    (Node n _ _) == (Leaf m _) = n == m
    (Node n _ _) == (Node m _ _) = n == m 
    Empty == Empty = True
    _ == _ = False

rightTree :: Int -> Tree Char
rightTree 1 = Leaf 'x'
rightTree n = Branch 'x' (Nil) (rightTree (n-1))

leftTree :: Int -> Tree Char
leftTree 1 = Leaf 'x'
leftTree n = Branch 'x' (leftTree (n-1)) (Nil)


simpTree :: Int -> Tree Char
simpTree 1 = Leaf 'x'
simpTree n = Branch 'x' (simpTree (n-1)) (simpTree (n-1))

makeTree :: Int -> Bool -> Bool -> Tree Char
makeTree 0 _ _ = Nil
makeTree 1 _ _ = Leaf 'x'
makeTree 2 True False = Branch 'x' (Leaf 'x') (Nil)
makeTree 2 False True = Branch 'x' (Nil) (Leaf 'x')
makeTree n l r 
  | l = Branch 'x' (makeTree (n-1) l r) (Nil)
  | r = Branch 'x' (Nil) (makeTree (n-1) l r)
  | l && r = error "l and r cannot be True at the same time"



addTree :: Tree a -> Tree a -> Int -> Tree a
addTree (Nil) t _ = t
addTree t (Nil) _ = t
addTree (Leaf x) (Leaf x) _ = Branch x (Leaf x) (Leaf x)





