import Control.Monad (replicateM)

and',nand',or',nor',xor' :: Bool -> Bool -> Bool
not' :: Bool -> Bool

not' True = False
not' False = True

and' True True = True
and' _ _ = False

nand' a b = not' $ and' a b

or' False False = False
or' _ _ = True

nor' a b = not' $ or' a b

xor' True True = False
xor' False False = False
xor' _ _ = True
boolToStr :: Bool -> String
boolToStr b = show b

printTruthRow :: [Bool] -> [Bool] -> ([Bool] -> Bool) -> String
printTruthRow [] cs f     = boolToStr (f cs)
printTruthRow (b:bs) cs f = boolToStr b ++ " " ++ printTruthRow bs cs f

truthTableGen :: Int -> ([Bool] -> Bool) -> [String]
truthTableGen n f = [(printTruthRow bs bs f)| bs <- arg n]
  where
    arg n = replicateM n [True,False]
    

