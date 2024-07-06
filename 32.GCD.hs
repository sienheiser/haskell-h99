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
