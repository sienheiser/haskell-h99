f :: [a] -> [([a],[a])]
f [x] = [([],[x])]
f (x:xs) = [(x:ys,zs)|(ys,zs) <- f xs]

g :: [a] -> [([a],[a])]
