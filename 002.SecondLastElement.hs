myButLast :: [a] -> a
myButLast [] = error "Did not expect empty list"
myButLast [x,y] = x
myButLast (x:xs) = myButLast xs
