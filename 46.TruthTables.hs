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
boolToStr True = show True
boolToStr False = show False
printTruthRow :: Bool -> Bool -> (Bool -> Bool -> Bool) -> String
printTruthRow a b f = boolToStr a ++ " " ++ boolToStr b ++ " " ++ boolToStr (f a b)

printTruthTable :: (Bool -> Bool -> Bool) -> [String]
printTruthTable f = [printTruthRow a b f  ++ "\n"| a <- boolean, b <- boolean]
    where
        boolean = [True,False]
