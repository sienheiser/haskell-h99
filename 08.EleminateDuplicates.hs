import Data.List (sort)

h :: Eq a => [a] -> [a]
h (x : []) = []
h (x:y:xs) | x/=y = y : h (y:xs)
           | otherwise = h (y:xs)

compr :: Eq a => [a] -> [a]
compr (x:xs) = x : h (x:xs)

compress :: (Eq a,Ord a) => [a] -> [a]
compress = compr 

