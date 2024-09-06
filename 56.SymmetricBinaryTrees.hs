data Tree a = Empty | Branch a (Tree a) (Tree a) deriving (Show)

reflectTree :: Tree Char -> Tree Char
reflectTree Empty = Empty
reflectTree (Branch 'x' (Branch 'x' Empty Empty) Empty) = Branch 'x' Empty (Branch 'x' Empty Empty)
reflectTree (Branch 'x' Empty (Branch 'x' Empty Empty)) = Branch 'x' (Branch 'x' Empty Empty) Empty
reflectTree (Branch 'x' (Branch 'x' Empty Empty) (Branch 'x' Empty Empty)) = Branch 'x' (Branch 'x' Empty Empty) (Branch 'x' Empty Empty)
reflectTree (Branch 'x' t1 t2) = Branch 'x' (reflectTree t2) (reflectTree t1)


symmetric :: Tree Char -> Tree Char -> Bool
symmetric (Branch 'x' Empty Empty) (Branch 'x' Empty Empty) = True
symmetric (Branch 'x' (Branch 'x' Empty Empty) Empty) (Branch 'x' (Branch 'x' Empty Empty) Empty) = True
symmetric (Branch 'x' Empty (Branch 'x' Empty Empty)) (Branch 'x' Empty (Branch 'x' Empty Empty)) = True
symmetric (Branch 'x' (Branch 'x' Empty Empty) (Branch 'x' Empty Empty)) (Branch 'x' (Branch 'x' Empty Empty) (Branch 'x' Empty Empty)) = True
symmetric
 
