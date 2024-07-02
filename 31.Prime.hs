
divisible :: Int -> Int -> Bool
divisible n m | mod n m == 0 = True
              | otherwise = False

prime' :: Int -> Int -> Bool
prime' n 2 = divisible n 2
prime' n m = divisible n (m-1)  ||  prime' n (m-1)

prime n = not (prime' n n)


