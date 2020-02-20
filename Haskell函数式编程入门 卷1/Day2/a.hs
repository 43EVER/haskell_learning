{-# LANGUAGEBangPatterns #-}

factorial :: Integral a => a -> a
factorial n
        | n < 0 = error "invalid parameter, n is less than 0"
        | n == 0 = 1
        | otherwise = n * factorial (n - 1)

myGcd :: Integral a => a -> a -> a
myGcd x y
        | y == 0 = x
        | otherwise = myGcd y (mod x y)

power :: Integral a => a -> a -> a
power 0 0 = 1
power _ 0 = 1
power x n = x * power x (n - 1)

power' :: Integral a => a -> a -> a
power' 0 0 = 1
power' _ 0 = 1
power' x n
        | odd n = let p = power' x (div (n - 1) 2) in p * p * x
        | otherwise = let p = power' x (div n 2) in p * p

product' :: Num a => [a] -> a
product' [] = 1
product' (x:xs) = x * product' xs

snoc :: a -> [a] -> [a]
snoc x [] = [x]
snoc x (y:ys) = y : snoc x ys

last' :: [a] -> a
last' [] = error "invalid parameter, empty list"
last' [x] = x
last' (x:xs) = last' xs

take' :: (Integral a) => a -> [b] -> [b]
take' n _ | n <= 0 = []
take' _ [] = []
take' n (x:xs) = x : take' (n - 1) xs

myElem :: Eq a => a -> [a] -> Bool
myElem _ [] = False
myElem a (x:xs)
        | a == x = True
        | otherwise = myElem a xs

myDelete :: Eq a => a -> [a] -> [a]
myDelete _ [] = []
myDelete a (x:xs)
        | a == x = myDelete a xs
        | otherwise = x : (myDelete a xs)

total :: Num a => [a] -> a
total [] = 0
total (x:xs) = x + total xs

total' :: Num a => [a] -> a -> a
total' [] n = n
-- total' (x:xs) !n = total' xs (n+x)
total' (x:xs) n = total' xs $! (n+x)

even' 0 = True
even' n = odd (n-1)

odd' 0 = False
odd' n = even (n-1)

mc n 
    | n > 100 = n - 10
    | otherwise = mc $ mc $ (+) n 11 