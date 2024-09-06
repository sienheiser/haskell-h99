
combination :: Int -> [a] -> [([a],[a])]
combination 0 xs = [([],xs)]
combination _ [] = []
combination n (x:xs) = ls ++ rs
    where
        ls = [(x:ys,zs)|(ys,zs)<-combination (n-1) xs]
        rs = [(ys,x:zs)|(ys,zs)<-combination n xs]

group :: [Int] -> [a] -> [[[a]]]
group [] _ = [[]]
group (n:ns) xs = 
    [ g:gs | (g,rs) <- combination n xs
           , gs <- group ns rs]
