data Mult a = Multiple Int a | Single a deriving(Show)

makeList :: Int -> a -> [a]
makeList 0 _ = []
makeList n x = x : makeList (n-1) x

decode :: (Mult a) -> [a]
decode (Single x) = [x]
decode (Multiple n x) = makeList n x

decodeModified :: [Mult a] -> [a]
decodeModified [] = []
decodeModified (x:xs) = decode x ++ decodeModified xs
