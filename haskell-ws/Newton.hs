squareroot :: Int -> Double -> Double
squareroot 0 x = x
squareroot n x = (x' + x / x') / 2
    where x' = squareroot (n - 1) x

fix :: (t -> t -> Bool) -> (t -> t) -> t -> t
fix c f x   | c x x' = x
            | otherwise = fix c f x'
            where x' = f x

newton :: Fractional a => a -> a -> a
newton c t = (c / t + t) / 2.0

mysqrt :: Double -> Double
mysqrt c = fix (\a b -> a - b < 0.000001) (newton c) c