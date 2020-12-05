positions :: (Eq t, Num t, Num a, Enum a) => t -> a -> [[a]]
positions 0 n = [[]]
positions k n = [x : xs | x <- [1..n], xs <- positions (k - 1) n]

noSameRow :: Eq a => [a] -> Bool
noSameRow [] = True
noSameRow (x:xs) = (not $ elem x xs) && noSameRow xs

noSameDig :: (Eq a, Num a, Enum a) => [a] -> Bool
noSameDig [] = True
noSameDig xs@(x:xs') = and [abs (i1 - i) /= abs (p1 - p) | (i, p) <- ip] && noSameDig xs'
    where
        (i1, p1):ip = zip [1..] xs

queen n = [xs | xs <- positions n n, noSameRow xs, noSameDig xs]

positions' 0 n = [[]]
positions' k n =
    [p : ps | ps <- positions' (k - 1) n, p <- [1..n], isSafe p ps]

isSafe p ps = not ((elem p ps) || (sameDig p ps))
    where
        sameDig p ps = any (\(dist, q) -> abs (p - q) == dist) $ zip [1..] ps