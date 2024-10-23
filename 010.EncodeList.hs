encode :: Eq a => [a] -> [(Int,a)]
encode [] = []
encode (x:xs) =
    let
        (first,rest) = span (==x) (xs)
    in
        (length (x:first),x) : encode (rest)
