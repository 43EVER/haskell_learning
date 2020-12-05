f :: Num a => a -> a
f x = 4 * x + 1

area r = pi * r ^ 2

f2 :: Num a => a -> a -> a
f2 x y = 4*x + 5*y + 1

f2' :: Num a => a -> a -> a
f2' = \x -> \y -> 4*x + 5*y + 1

s :: Double -> Double -> Double -> Double
s a b c = let p = (a + b + c) / 2
        in sqrt (p * (p-a) * (p-b) * (p-c))

s' :: Double -> Double -> Double -> Double
s' a b c = sqrt (p * (p-a) * (p-b) * (p-c))
            where
                p = (a + b + c) / 2