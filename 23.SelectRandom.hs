import System.Random
import Control.Monad (replicateM)

rnd_select :: [a] -> Int -> IO [a]
rnd_select [] _ = return []
rnd_select xs n | n < 0 = error "Selection range less than zero"
                | otherwise = do pos <- replicateM n $ 
                                    getStdRandom $ randomR (0, (length xs)-1)
                                 return [xs!!p | p <- pos]

-- rnd_selectV2 :: [a] -> Int -> IO [a]
-- rnd_selectV2 _ 0 = return []
-- rnd_selectV2 [] _ = error "No elements to select in given list"
-- rnd_selectV2 xs n | n < 0 = error "Selection range less than zero"
--                   | n > length xs = error "selection range greater than length of list"
--                   | otherwise = do 
--                         pos <- replicateM n $ getStdRandom $ randomR(0,(length xs)-1)
--                         let
                            

rnd_selectV3 :: [a] -> [a] -> Int -> IO [a]
rnd_selectV3 [] _ = return []
rnd_selectV3 _ [] = return []
rnd_selectV3 (i:is) xs = do 
      let remainder = (take i xs) ++ (drop (i+1) xs)
      in return (xs !! i) : rnd_selectV3 i remainder


