import System.Random
import Control.Monad (replicateM)

rnd_select :: [a] -> Int -> IO [a]
rnd_select [] _ = return []
rnd_select xs n | n < 0 = error "Selection range less than zero"
                | otherwise = do pos <- replicateM n $ 
                                    getStdRandom $ randomR (0, (length xs)-1)
                                 return [xs!!p | p <- pos]

diff_select :: Int -> Int -> IO [Int]
diff_select _ 0 = return []
diff_select n m | n < 0 = error "Selection range less than zero"
                | otherwise = do 
                    pos <- replicateM n $ getStdRandom $ randomR (0,m)
                    return [p | p <- pos]
