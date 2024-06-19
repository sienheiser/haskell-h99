import System.Random

--Can return a list with repeated elements
rnd_select1 :: [a] -> Int -> IO [a]
rnd_select1 xs n = do
   gen <- newStdGen
   return $ take n [xs !! p| p <- randomRs (1,(length xs)-1) gen]


removeAt :: Int -> [a] -> [a]
removeAt n xs = take n xs ++ drop (n+1) xs

pick :: Int -> [a] -> (a,[a])
pick n xs = (xs !! n, removeAt n xs)

rnd_select2' :: Int -> [a] -> StdGen -> IO [a]
rnd_select2' 0 _ _ = return []
rnd_select2' n xs gen = do
   let (rn,newGen) = randomR (0 ,length xs -1) gen
   let (y,ys) = pick rn xs
   rest <- rnd_select2' (n-1) ys newGen
   return (y:rest)
--return a list with unique elements
rnd_select2 :: Int -> [a] -> IO [a]
rnd_select2 n xs | n > length xs = error "n is greater than length list"
                 | otherwise = do
                  gen <- newStdGen
                  rnd_select2' n xs gen
