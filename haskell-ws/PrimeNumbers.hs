factors :: Integral a => a -> [a]
factors n = smallFactors ++ reverse (map (div n) smallFactors)
    where smallFactors = [x | x <- takeWhile (\x -> x * x <= n) [1..], mod n x == 0]

isPrime :: Integral a => a -> Bool
isPrime n = factors n == [1, n]

isPrime' :: Integral a => a -> Bool
isPrime' 2 = True
isPrime' p 
    | even p = False
    | otherwise = p > 1 && all (\n -> p `mod` n /= 0) (takeWhile (\n -> n * n <= p) [3,5..])

nextPrime :: Integer -> Integer
nextPrime a | odd a = if isPrime a then a else nextPrime (a + 2)
            | otherwise = nextPrime (a + 1)

sieve :: Integral a => [a] -> [a]
sieve (p:xs) = p : sieve [x | x <- xs, x `mod` p /= 0]

primes' :: Integral a => [a]
primes' = sieve [2..]

primeFactorList :: Integral a => a -> [a]
primeFactorList a = [x | x <- factors a, isPrime' x]

maxFactorNum :: Integral a => a -> a -> a
maxFactorNum x fac
    | mod x fac /= 0    = 0
    | otherwise         = 1 + maxFactorNum (div x fac) fac

primeFactors :: Integral a => a -> [(a, a)]
primeFactors a = zip (map (maxFactorNum a) factors) factors
    where factors = primeFactorList a

