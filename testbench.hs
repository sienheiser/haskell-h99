t1 :: Char -> IO Char
t1 a = return a

t2 :: Char -> IO Char
t2 a = do
    let x = a 
        in return x 

t3 :: Char -> IO Char
t3 a = do {
    let {x = a} 
    in return x;
};

t4 :: Int -> String -> IO String
t4 0 xs = return []
t4 n xs | n < 0 = error "Given n is less than 0"
        | length xs < n = error "Given n is greater than length xs"
        | otherwise = return (xs !! n) : t4 (n-1) xs
