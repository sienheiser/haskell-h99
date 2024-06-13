import System.Random

rnd_select1 :: [a] -> Int -> IO [a]
rnd_select1 xs n = do
   gen <- newStdGen
   return $ take n [xs !! p| p <- randomRs (1,(length xs)-1) gen]


