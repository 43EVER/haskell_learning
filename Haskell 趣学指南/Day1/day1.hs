-- 函数
lucky :: (Integral a) => a -> String
lucky 7 = "LUCKY NUMBER SEVEN!"
lucky x = "Sorry, you're out of luck, pal!"

sayMe :: (Integral a) => a -> String
sayMe 1 = "One!"
sayMe 2 = "Two!"
sayMe 3 = "Three!"
sayMe 4 = "Four!"
sayMe 5 = "Five!"
sayMe x = "Not between 1 and 5!"

factorial :: (Integral a) => a -> a
factorial 0 = 1
factorial n = n * factorial (n - 1)

charMe :: Char -> String
charMe 'a' = "Albert"
charMe 'b' = "Broseph"
charMe 'c' = "Cecil"

addVectors :: (Num a) => (a, a) -> (a, a) -> (a, a)
addVectors a b =
    (fst a + fst b, snd a + snd b)

first :: (a, b, c) -> a
first (x, _, _) = x
second :: (a, b, c) -> b
second (_, x, _) = x
third :: (a, b, c) -> c
third (_, _, x) = x

head' :: [a] -> a
head' [] = error "Can't call head' for an empty list!"
head' (x:_) = x
tail' :: [a] -> [a]
tail' [] = error "Cant' call tail' for an empty list!"
tail' (_:x) = x

tell :: (Show a) => [a] -> String
tell [] = "The list is empty"
tell (x:[]) = "The list has one element" ++ show x
tell (x:y:[]) = "The list has two elements: " ++ show x ++ " and " ++ show y
tell (x:y:_) = "This list is long. The first two elements are: " ++ show x ++ " and " ++ show y

length' :: (Num b) => [a] -> b
length' [] = 0
length' (_:xs) = 1 + length' xs

sum' :: (Num a) => [a] -> a
sum' [] = 0
sum' (x:xs) = x + sum' xs

capital :: String -> String
capital "" = "Empty string"
capital all@(x:xs) = "The first letter of " ++ all ++ " is " ++ [x]


-- guard
bmiTell :: (RealFloat a) => a  -> a -> String
bmiTell weight height
    | bmi <= skinny = "You're underweight"
    | bmi <= normal = "You're supposedly normal"
    | bmi <= fat = "You're fat"
    | otherwise = "You're a whale"
    where   bmi = weight / height ^ 2
            skinny = 18.5
            normal = 25.0
            fat = 30.0

max' :: (Ord a) => a -> a -> a
max' a b
    | a > b = a
    | otherwise = b

myComapre :: (Ord a) => a -> a -> Ordering
a `myComapre` b
    | a > b     = GT
    | a == b    = EQ
    | otherwise = LT


-- where 语法
initials :: String -> String -> String
initials firstname lastname = [f] ++ ". " ++ [l] ++ "."
    where   (f:_) = firstname
            (l:_) = lastname

calcBmis :: (RealFloat a) => [(a, a)] -> [a]
calcBmis xs = [bmi w h | (w, h) <- xs]
    where bmi weight height = weight / height ^ 2

-- let 表达式
cylinder :: (RealFloat a) => a -> a -> a
cylinder r h =
    let sideArea = 2 * pi * r * h
        topArea = pi * r ^ 2
    in sideArea + 2 * topArea

calcBmis' :: (RealFloat a) => [(a, a)] -> [a]
calcBmis' xxs =
    [bmi | (w, h) <- xs, let bmi = w / h ^ 2, bmi >= 25.0 && bmi <= 30.0]
    where xs = xxs

multiLetBindings = (let a = 100; b = 100; c = 100 in a * b * c)


-- case 表达式
myHead :: [a] -> a
myHead xs = case xs of
    []      -> error "No head for empty list"
    (x:_)   -> x
myTail :: [a] -> [a]
myTail xs = case xs of
    []      -> error "No tail for empty list"
    (_:x)   -> x

describList :: [a] -> String
describList xs = "This list is " ++ case xs of
    [] -> "empty"
    [x] -> "a singleton list"
    xs -> "a longer list"

describList' :: [a] -> String
describList' xs = "This list is " ++ what xs
    where   what [] = "empty"
            what [x] = "a singleton list"
            what xs = "a longer list"


-- 递归

maximum' :: (Ord a) => [a] -> a
maximum' [] = error "maximum of empty list"
maximum' [x] = x
maximum' (x:xs)
    | x > maxTail = x
    | otherwise = maxTail
    where maxTail = maximum' xs

myMaximum :: (Ord a) => [a] -> a
myMaximum [] = error "myMaximum of empty list"
myMaximum [x] = x
myMaximum (x:xs) = max x (myMaximum xs)

replicate' :: (Num i, Ord i) => i -> a -> [a]
replicate' n x
    | n <= 0    = []
    | otherwise = x:(replicate' (n - 1) x)

take' :: (Num i, Ord i) => i -> [a] -> [a]
take' n _
    | n <= 0    = []
take' _ [] = []
take' n (x:xs) = x : take' (n - 1) xs

reverse' :: [a] -> [a]
reverse' [] = []
reverse' (x:xs) = (reverse' xs) ++ [x]

repeat' :: a -> [a]
repeat' x = x: repeat' x

zip' :: [a] -> [b] -> [(a, b)]
zip' _ [] = []
zip' [] _ = []
zip' (x:xs) (y:ys) = (x, y):zip' xs ys

elem' :: (Eq a) => a -> [a] -> Bool
elem' a [] = False
elem' a (x:xs)
    | a == x    = True
    | otherwise = elem' a xs

quickSort :: (Ord a) => [a] -> [a]
quickSort [] = []
quickSort (x:xs) =
    let smallerSorted   = quickSort [a | a <- xs, a <= x]
        biggerSorted    = quickSort [a | a <- xs, a > x]
    in smallerSorted ++ [x] ++ biggerSorted

fibonacci :: (Integral a) => a -> a
fibonacci 0 = 1
fibonacci 1 = 1
fibonacci x = fibonacci (x - 1) + fibonacci(x - 2)


-- 高阶函数
multThree :: (Num a) => a -> a -> a -> a
multThree x y z = x * y * z

compareWithHundred :: (Num a, Ord a) => a -> Ordering
compareWithHundred = compare 100

divideByTen :: (Floating a) => a -> a
divideByTen = (/20)

isUpperAlphanum :: Char -> Bool
isUpperAlphanum = (`elem` ['A'..'Z'])

myMinusFour :: (Num a) => a -> a
myMinusFour = (subtract 4)

applyTwice :: (a -> a) -> a -> a
applyTwice f x = f (f x)

zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _ [] _ = []
zipWith' _ _ [] = []
zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys

flip' :: (a -> b -> c) -> b -> a -> c
flip' f = g
    where g x y = f y x
flip'' :: (a -> b -> c) -> b -> a -> c
flip'' f x y = f y x

quickSort' :: (Ord a, Eq a) => [a] -> [a]
quickSort' [] = []
quickSort' (x:xs) =
    let smallerSorted = quickSort (filter (<=x) xs)
        biggerSorted = quickSort (filter (>x) xs)
    in smallerSorted ++ [x] ++ biggerSorted

getTriangles :: (Integral a) => [a] -> [a] -> [a] -> [(a, a, a)]
getTriangles xs ys zs =
    [(x, y, z) | x <- xs, y <- ys, z <- zs, x^2 + y^2 == z^2]

largestDivisible :: (Integral a) => [a]
largestDivisible = filter p (take 100000 [100000, 99999..])
    where p x = x `mod` 3829 == 0

chain :: (Integral a) => a -> [a]
chain 1 = [1]
chain n
    | even n    = n:chain (n `div` 2)
    | odd n     = n:chain (n * 3 + 1)
numLongChains :: Int
numLongChains = length (filter isLong (map chain [1..100]))
    where isLong xs = length xs > 15

numLongChains' :: Int
numLongChains' = length (filter (\xs -> length xs > 15) (map chain [1..100]))

flip''' :: (a -> b -> c) -> b -> a -> c
flip''' f = \x y -> f y x

sum'' :: (Num a) => [a] -> a
sum'' xs = foldl (\acc x -> acc + x) 0 xs

sum''' :: (Num a) => [a] -> a
sum''' = foldl (+) 0

elem'' :: (Eq a) => a -> [a] -> Bool
elem'' x xs = foldl (\acc y -> if y == x then True else acc) False xs

map' :: (a -> b) -> [a] -> [b]
map' f xs = foldr (\x acc -> f x : acc) [] xs

maximum'' :: (Ord a) => [a] -> a
maximum'' = foldl1 (\acc x -> if x > acc then x else acc)

reverse'' :: [a] -> [a]
reverse'' = foldl (\acc x -> x : acc) []

product' :: (Num a) => [a] -> a
product' = foldl1 (*)

filter' :: (a -> Bool) -> [a] -> [a]
filter' p = foldr (\x acc -> if p x then x:acc else acc) []

head'' :: [a] -> a
head'' = foldr1 (\x _ -> x)

last'' :: [a] -> a
last'' = foldl1 (\_ x -> x)

sqrtSums :: Int
sqrtSums = length (takeWhile (<1000) (scanl1 (+) (map sqrt [1..]))) + 1
