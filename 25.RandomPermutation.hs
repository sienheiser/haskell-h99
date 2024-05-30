import System.Random
import Control.Monad (replicateM)

-- rnd_permu :: [a] -> IO [a]
-- rnd_permu [] = return []
-- rnd_permu xs = do 
--     pos <- replicateM (length xs) $ getStdRandom $ randomR (0,(length xs)-1)
--     return [xs !! p | p <- pos]

rnd_select :: [a] -> Int -> IO [a]
rnd_select [] _ = return []
rnd_select xs n | n < 0 = error "Selection range less than zero"
                | otherwise = do pos <- replicateM n $ 
                                    getStdRandom $ randomR (0, (length xs)-1)
                                 return [xs!!p | p <- pos]

rnd_permu :: [a] -> IO [a]
rnd_permu xs = rnd_select xs (length xs)

