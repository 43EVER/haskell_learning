main = putStrLn "Hello, World!"

add :: (Int, Int) -> Int
add (x, y) = x + y

add' :: Int -> Int -> Int
add' x y = x + y

type MYRGB = (Int, Int, Int)
type MYPicture = [[MYRGB]]

type ID         = Int
type BookName   = String
type Book = (ID, BookName)

fun1 :: Show a => Num a => Ord a => Show b => a -> b -> String
fun1 a b = "fuck"

fun2 :: (a -> a) -> a -> a
fun2 = \a -> \b -> a b

f' :: Num a => a -> a -> a
f' = \x -> \y -> 4*x + 5*y + 1

s :: Double -> Double -> Double -> Double
s a b c = let p = (a + b + c) / 2
          in sqrt (p * (p-a) * (p-b) * (p-c))

a = let f x = x + 1 in f 5

s' :: Double -> Double -> Double -> Double
s' a b c = sqrt (p * (p-a) * (p-b) * (p-c))
                where
                    p = (a + b + c) / 2 + fuck
                        where fuck = 0

isTwo :: Int -> Bool
isTwo n = if n == 2 then True else False

month :: Int -> Int
month n = case n of
    1 -> 31
    2 -> 28
    3 -> 31
    4 -> 30
    5 -> 31
    6 -> 30
    7 -> 31
    8 -> 31
    9 -> 30
    10 -> 31
    11 -> 30
    12 -> 31
    _ -> error "invalid month"

abs' :: (Num a, Ord a) => a -> a
abs' n
    | n > 0 = n
    | otherwise = -n

month' :: Int -> Int
month' 1 = 31
month' a
    | a == 2 = 28
    | a == 3 = 21
month' a = case a of
    4 -> 30
    5 -> 31
month' _ = error "invalid month"

head' :: [a] -> a
head' [] = error "empty list"
head' (x:_) = x

infixr 5 <->, <+>
(<->), (<+>) :: Int -> Int -> Int
(<->) x y = x - y
(<+>) x y = x + y

infixl 5 ++++
(++++) a b = a + b

infix 5 +-
(+-) a b c = a + b - c