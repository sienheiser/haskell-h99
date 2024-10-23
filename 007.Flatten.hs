data NestedList a = Elem a | List [NestedList a]

flatten :: NestedList a -> [a]
flatten (List []) = []
flatten (Elem a) = [a]
flatten (List [Elem a]) = flatten (Elem a)
flatten (List [(List xs)]) = flatten (List xs)
flatten (List (x:xs)) = flatten x ++ flatten (List xs)


--swap :: NestedList a -> [a] -> [a]
--swap (List []) ys = ys
--swap (Elem a) ys = a : ys
--swap (List [Elem a]) ys = swap (Elem a) ys
--swap (List ([List xs])) ys = swap (List xs) ys
--swap (List (x : xs)) ys = swap x ys
