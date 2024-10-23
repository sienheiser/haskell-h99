element :: Int -> Int -> String
element 1 0 = "0"
element 1 1 = "1"
element n m 
  | m >= 2^n = error "m cannot be greater than 2^n"
  | m < 2^(n-1) = "0" ++ element (n-1) m
  | otherwise = "1" ++ element (n-1) (2^n - m - 1)


gray :: Int -> [String]
gray 0 = []
gray n = [element n m | m <- [0 .. (2^n)-1]]

grayV2 :: Int -> [String]
grayV2 0 = [""]
grayV2 n = ['0' : x | x <- prev] ++ ['1' : x | x <- reverse prev]
  where
    prev = grayV2 (n-1)
