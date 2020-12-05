{-# LANGUAGE ParallelListComp, TransformListComp #-}
import GHC.Exts ( groupWith, sortWith, the )
map' :: (t -> a) -> [t] -> [a]
map' f xs = [f x | x <- xs]

filter' :: (a -> Bool) -> [a] -> [a]
filter' f xs = [x | x <- xs, f x]

multipleCondition :: [Integer]
multipleCondition = [x | x <- [0..], even x, x > 10]

series :: Int -> [Double]
series n = [1 / (2 * (fromIntegral k) + 1) * (-1) ^ k | k <- [0..n]]

pi' :: Double
pi' = 4 * (sum $ series 200000)

series1 :: Int -> [Double]
series1 n = 1/2:[1 / (2 * fromIntegral k + 1) * (-1) ^ k * (1/2 ^ (2 * fromIntegral k + 1)) | k <- [1..(n-1)]]

series2 :: Int -> [Double]
series2 n = 1/3:[1 / (2 * fromIntegral k + 1) * (-1) ^ k * (1/3 ^ (2 * fromIntegral k + 1)) | k <- [1..(n-1)]]

fastpi :: Double
fastpi = 4 * sum (series1 2000) + 4 * sum (series2 2000)

table :: [([Char], [Char], Integer)]
table =
    [
        ("Hangzhou", "MP4", 243),
        ("Hangzhou", "CD", 925),
        ("Beijing", "MP4", 157),
        ("Beijing", "CD", 536),
        ("Shanghai", "MP4", 784),
        ("Shanghai", "CD", 766)
    ]

analysis :: [([Char], Integer)]
analysis = [(the product, sum cost) |
                (city, product, cost) <- table,
                then group by product using groupWith,
                then sortWith by (sum cost)]