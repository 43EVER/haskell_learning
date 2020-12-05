map' :: (a -> b) -> [a] -> [b]
map' op [] = []
map' op (x:xs) = (op x) : map' op xs

apply :: (a -> a) -> Int -> a -> a
apply f 1 x = f x
apply f n x = apply f (n - 1) (f x)

apply' :: (a -> a) -> Int -> a -> a
apply' f 1 x = f x
apply' f n x = apply (f . f) (div n 2) (if even n then x else f x)
