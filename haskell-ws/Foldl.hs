reverse' :: [a] -> [a]
reverse' = foldl (flip (:)) []

unwords' :: [String] -> String
unwords' [] = ""
unwords' ws = foldr1 (\w s -> w ++ ' ' : s) ws

maximum', minimum' :: Ord a => [a] -> a
maximum' = foldl1 max

minimum' = foldl1 min

gcds :: Integral a => [a] -> a
gcds = foldr1 gcd

lcms :: Integral a => [a] -> a
lcms = foldr1 lcm

interLeave :: [a] -> [a] -> [a]
interLeave xs [] = xs
interLeave [] ys = ys
interLeave (x:xs) (y:ys) = x : y : interLeave xs ys