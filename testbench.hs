data Tree a = Nil | Leaf a | Branch a (Tree a) (Tree a)
  deriving (Show,Eq)


leftTree :: Tree Char -> Tree Char
leftTree Nil = Leaf 'x'
leftTree (Leaf 'x') = Branch 'x' (Leaf 'x') (Nil)
leftTree (Branch 'x' (Leaf 'x') (Nil)) = Branch 'x' (Leaf 'x') (Leaf 'x')
leftTree (Branch 'x' (Nil) (Leaf 'x')) = Branch 'x' (Leaf 'x') (Leaf 'x')
leftTree (Branch 'x' t1 t2) = Branch 'x' (leftTree t1) t2

rightTree :: Tree Char -> Tree Char
rightTree Nil = Leaf 'x'
rightTree (Leaf 'x') = Branch 'x' (Nil) (Leaf 'x')
rightTree (Branch 'x' (Leaf 'x') (Nil)) = Branch 'x' (Leaf 'x') (Leaf 'x')
rightTree (Branch 'x' (Nil) (Leaf 'x')) = Branch 'x' (Leaf 'x') (Leaf 'x')
rightTree (Branch 'x' t1 t2) = Branch 'x' t1 (rightTree t2)

t1 :: Tree Char
t1 = Branch 'x' (Branch 'x' (Leaf 'x') (Nil)) (Branch 'x' (Leaf 'x') (Nil))


trees :: Int -> Tree Char -> [Tree Char]
trees 0 t = [t]
trees n Nil = trees (n-1) (Leaf 'x')
trees n t = trees (n-1) (leftTree t) ++ trees (n-1) (rightTree t)
