powerSet :: [a] -> [[a]]
powerSet [] = [[]]
powerSet (x:xs) = [x : r | r <- xs'] ++ xs'
    where xs' = powerSet xs