quotient' :: Int -> Int -> Int -> (Int,Int)
quotient' a b counter 
    | b - a < 0 = (counter,b)
    | otherwise = quotient' a (b-a) (counter+1)

quotientRemainder :: Int -> Int -> (Int,Int)
quotientRemainder a b = quotient' a b 0

g :: Int -> Int -> Int
g a b 
    | snd qr == 0 = a
    | otherwise = g (snd qr) a
  where
    qr = quotientRemainder a b

myGCD :: Int -> Int -> Int
myGCD a b 
    | a < 0 = g (-a) b 
    | b < 0 = g a (-b)
    | b < a = g b a
    | otherwise = g a b

coprime :: Int -> Int -> Bool
coprime a b 
    | myGCD a b == 1 = True
    | otherwise = False

conv :: Bool -> Int
conv True = 1
conv False = 0
totient' :: Int -> Int -> Int
totient' n 1 = conv $ coprime n 1
totient' n m | m > n = error"second arguement cannot be greater than first" 
             | otherwise = conv (coprime n m) + totient' n (m-1)
    

totient :: Int -> Int
totient n = totient' n n
