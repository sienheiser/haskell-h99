
myLast :: [a] -> a
myLast [] = error "Did not expect empty list"
myLast [x] = x
myLast (x:xs) = myLast xs

