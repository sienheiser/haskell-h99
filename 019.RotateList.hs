
split ::  Int -> [a] -> ([a],[a])
split n xs = (take n xs, drop n xs)

split2 :: Int -> [a] -> ([a],[a])
split2 n xs = (drop n xs, take n xs)


