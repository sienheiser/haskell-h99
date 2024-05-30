data Mult a = Multiple Int a | Single a deriving(Show)

makeMult :: Int -> a -> (Mult a)
makeMult 0 x = error "Did not expect zero"
makeMult n x | n > 1 = Multiple n x
             | otherwise = Single x


encodeModified :: Eq a => [a] -> [Mult a]
encodeModified [] = []
encodeModified (x:xs) = 
    let
        (first,rest) = span (==x) xs
    in
        makeMult (length (x:first)) x : encodeModified (rest)

