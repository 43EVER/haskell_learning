import Test as T hiding (f2)
import Boolean
import Data.List (genericLength)

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