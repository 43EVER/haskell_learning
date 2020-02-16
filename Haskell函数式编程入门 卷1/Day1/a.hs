import Data.List (genericLength)
import Prelude


month :: Int -> Int
month = undefined

avg :: (Fractional a) => [a] -> a
avg xs = sum xs / (fromIntegral $ length xs)

avg' :: (Fractional a) => [a] -> a
avg' xs = sum xs / genericLength xs

last' :: [a] -> a
last' [] = error "empty list"
last' (x:[]) = x
last' (_:xs) = last' xs

type Weekday = Int
type Year = Int
type Month = Int
type Day = Int

week' :: Year -> Day -> Weekday
week' y d = let y1 = y - 1 in
            (y1 + (y1 `div` 4) - (y1 `div` 100) + (y1 `div` 400) + d) `mod` 7

isLeapYear :: Int -> Bool
isLeapYear y = (mod y 4 == 0) && (mod y 100 /= 0) || (mod y 400 == 0)

monthDays :: Year -> Month -> Int
monthDays y m
            | m == 2 = if not $ isLeapYear y then 28 else 29
            | elem m [1, 3, 5, 7, 8, 10, 12] = 31
            | elem m [4, 6, 9, 11] = 30
            | otherwise = error "invalid month"

accDays :: Year -> Month -> Day -> Int
accDays y m d
            | d > monthDays y m = error "invalid days"
            | otherwise = (sum $ take (m - 1) (map (monthDays y) [1..12])) + d

week :: Year -> Month -> Day -> Int
week y m d = week' y (accDays y m d)

reverseWords :: String -> String
reverseWords str = unwords $ reverse $ words str