import Data.Bits ( Bits(shift, (.&.)) )
import Data.List

factorial :: Integer -> Integer
factorial n | n < 0     = error "n is less than 0"
            | n == 0    = 1
            | otherwise = n * factorial (n - 1)


mygcd :: Int -> Int -> Int
mygcd x y   | y == 0    = x
            | otherwise = mygcd y (mod x y)


power :: Int -> Int -> Int
power 0 0 = 1
power _ 0 = 1
power x n = x * power x (n - 1)

power' :: Int -> Int -> Int
power' 0 0 = 1
power' _ 0 = 1
power' x n  | odd n     = let p = power' x ((n - 1) `div` 2) in x * p * p
            | otherwise = let p = power' x (n `div` 2) in p * p

product' :: Num p => [p] -> p
product' [] = 1
product' (x:xs) = x * product' xs

sonc :: a -> [a] -> [a]
sonc x [] = [x]
sonc x (y:ys) = y : sonc x ys

last' :: [a] -> a
last' [] = error "empty list"
last' [x] = x
last' (_:xs) = last' xs

take' n _ | n <= 0 = []
take' _ [] = []
take' n (x : xs) = x : take (n - 1) xs

elem' :: Eq a => a -> [a] -> Bool
elem' _ [] = False
elem' a (x:xs)  | a == x = True
                | otherwise = elem' a xs

delete' :: Eq a => a -> [a] -> [a]
delete' _ [] = []
delete' a (x:xs)    | a == x = delete' a xs
                    | otherwise = x : delete' a xs

drop' :: Int -> [a] -> [a]
drop' _ [] = []
drop' 0 xs = xs
drop' a (x:xs) = drop' (a - 1) xs

total' [] n = n
total' (x:xs) n = total' xs (n + x)

total xs = total' xs 0

itotal :: Num a => [a] -> a -> a
itotal [] n = n
itotal (x:xs) n = itotal xs $! (x + n)

mc :: (Ord t, Num t) => t -> t
mc n    | n > 100   = n - 10
        | otherwise = mc (mc (n + 11))


fibonacci :: (Num a, Eq a) => a -> a
fibonacci 0 = 0
fibonacci 1 = 1
fibonacci n = fibonacci (n - 1) + fibonacci (n - 2)

fibStep :: Num a => (a, a) -> (a, a)
fibStep (u, v) = (v, u + v)

fibPair :: (Eq a, Num a) => a -> (a, a)
fibPair 0 = (0, 1)
fibPair n = fibStep (fibPair (n - 1))

fastFib :: Eq b => Num b => b -> b
fastFib n = fst (fibPair n)

fibs :: (Enum b, Eq b, Num b) => b -> [b]
fibs n = map fastFib [1..n]

fibs' n = take n (map fst (iterate fibStep (0, 1)))

golden :: Fractional a => Int -> [a]
golden n = take n (map (\(x, y) -> x/y) (iterate fibStep (0, 1)))

combine :: [(a, a)] -> [(a, a, a)]
combine ((f1, f2):(f3, f4):fs) = (f1, f2, f4):combine ((f3, f4):fs)
combine _ = []

fibPairs :: Int -> [(Int, Int)]
fibPairs n = map fibPair [1..n]

difference :: Int -> [Int]
difference n = map (\(f1, f2, f3) -> f1*f3 - f2*f2) (combine $ fibPairs n)


romeNotation :: [String]
romeNotation = ["M", "CM", "D", "CD", "C", "XC", "L", "XL", "X", "IX", "V", "IV", "I"]

romeAmount :: [Int]
romeAmount = [1000, 900, 500, 400, 100, 90, 50, 40, 10, 9, 5, 4, 1]

pair :: [(Int, String)]
pair = zip romeAmount romeNotation

subtrahend :: Int -> (Int, String)
subtrahend n = head (dropWhile (\(a, _) -> a > n) pair)

convert :: Int -> String
convert 0 = ""
convert n = let (rome, m) = subtrahend n
            in m ++ convert (n - rome)

getFst :: String -> (Int, String)
getFst s = head $ dropWhile (\x -> not (snd x `isPrefixOf` s)) pair

convert' :: String -> Int
convert' "" = 0
convert' s = let p = getFst s in fst p + convert' (drop (length $ snd p) s)


convertToBinary :: Int -> String
convertToBinary 0 = "0"
convertToBinary 1 = "1"
convertToBinary n   | n  .&. 1 == 1 = convertToBinary (shift n (-1)) ++ "1"
                    | otherwise     = convertToBinary (shift n (-1)) ++ "0"

ones :: [Integer]
ones = 1:ones

nature :: [Integer]
nature = 0 : map (+1) nature

fibsList :: [Integer]
fibsList = 0:1:zipWith (+) fibsList (tail fibsList)

{-
nature = 0 : map (+1) nature
= 0 : map (+1) (0 : map (+1) nature)
= 0:1:map (+1) (map (+1) nature)
= 0:1:map (+1) (map (+1) (0:map (+1) (0:map (+1) nature)))
= 0:1:2:map (+1) (map (+1) (map (+1) (map (+1) nature)))
=...

fibsList = 0:1:zipWith (+) fibsList (tail fibsList)
= 0:1:zipWith (+) (0:1:zipWith (+) fibsList (tail fibsList)) (1:zipWith (+) fibsList (tail fibsList))
= 0:1:1:zipWith (+) 
        (
                1:zipWith (+) 
                (
                        0:1:zipWith (+) 
                                fibsList 
                                (tail fibsList)
                ) 
                (
                        1:zipWith (+) 
                                fibsList 
                                (tail fibsList)
                )
        ) 
        (
                zipWith (+) 
                (
                        0:1:zipWith (+) 
                                fibsList 
                                (tail fibsList)
                )
                (
                        1:zipWith (+) 
                                fibsList 
                                (tail fibsList)
                )
        )
-}

lazyShorter :: [a] -> [a] -> [a]
lazyShorter xs ys 
        | short xs ys = xs
        | otherwise = ys
        where
                short [] ys = True
                short xs [] = False
                short (x:xs) (y:ys) = short xs ys

merge :: Eq a => Ord a => [a] -> [a] -> [a]
merge [] ys = ys
merge xs [] = xs
merge (x:xs) (y:ys)
        | x < y = x:merge xs (y:ys)
        | x == y = x:merge xs ys
        | otherwise = y:merge (x:xs) ys

ham :: [Integer]
ham = 1:merge (map (*2) ham) (merge (map (*3) ham) (map (*5) ham))