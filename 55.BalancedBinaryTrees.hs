data Tree a = Nil | Leaf a | Branch a (Tree a) (Tree a)
  deriving (Show,Eq)

cbalTree :: Int -> Tree Char
cbalTree 0 = Leaf 'x'
cbalTree n = Branch 'x' (cbalTree (n-1)) (cbalTree (n-1))


cbalTree :: Int -> Tree Char
cbalTree 1 = Nil
cbalTree n = [Branch 'x' (cBalTree (n-1)) 
