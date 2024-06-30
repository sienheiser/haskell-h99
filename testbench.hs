

f :: [a] -> [[[a]]]
f xs = [[[x]] | x <- xs]



combination :: Int -> [a] -> [([a],[a])]
combination 0 xs     = [([],xs)]
combination n []     = []
combination n (x:xs) = ds ++ ts 
  where
    ts = [ (x:ys,zs) | (ys,zs) <- combination (n-1) xs ]
    ds = [ (ys,x:zs) | (ys,zs) <- combination  n    xs ]

comb :: Int -> [a] -> [([a],[a])]
comb 0 xs     = [([],xs)]
comb n []     = []
comb n (x:xs) = ts ++ ds
  where
    ts = [ (x:ys,zs) | (ys,zs) <- comb (n-1) xs ]
    ds = [ (ys,x:zs) | (ys,zs) <- comb  n    xs ]
