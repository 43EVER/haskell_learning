isTwo :: Int -> Bool
isTwo n = if n == 2 then True else False

month :: Int -> Int
month n = case n of
    1 -> 31
    2 -> 28
    3 -> 30
    4 -> 30
    5 -> 31
    6 -> 31
    7 -> 31
    8 -> 31
    9 -> 31
    10 -> 31
    11 -> 31
    12 -> 31
    _ -> error "invalid month"

abs' :: (Num a, Ord a) => a -> a
abs' n
    | n > 0 = n
    | otherwise = -n

month' :: Int -> Int
month' 1 = 31
month' 2 = 28
month' 3 = 31
month' 4 = 30
month' 5 = 31
month' 6 = 30
month' 7 = 31
month' 8 = 31
month' 9 = 30
month' 10 = 31
month' 11 = 30
month' 12 = 31
month' _ = error "invalid month"

head' :: [x] -> x
head' [] = error "emtpy list"
head' (x:_) = x