swapElements :: [a] -> [a] -> [a]
swapElements [] ys = ys
swapElements (x:xs) ys = swapElements xs (x:ys)

myReverse :: [a] -> [a]
myReverse xs = swapElements xs []

isPalindrome :: Eq a => [a] -> Bool
isPalindrome [] = True
isPalindrome xs | xs == myReverse xs = True
                | otherwise = False
