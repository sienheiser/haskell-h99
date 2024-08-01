data Tree a = Empty | Branch a (Tree a) (Tree a)

leaf :: Tree a
leaf x = Branch x Empty Empty


