data Tree a = Empty | Branch a (Tree a) (Tree a)

leaf :: a -> Tree a 
leaf x = Branch x Empty Empty


test :: Tree Char 
test = Branch 'a' (leaf 'a') (Empty)
