elementAt :: [a] -> Int -> a
elementAt [] _ = error "Did not expect empty list"
elementAt (x:xs) 1 = x
elementAt (x:xs) n = elementAt xs (n-1)
