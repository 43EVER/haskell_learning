foldr' :: (a -> b -> b) -> b -> [a] -> b
foldr' f v [] = v
foldr' f v (x:xs) = f x (foldr' f v xs)

sum xs = foldr (+) 0 xs
and xs = foldr (&&) True xs

(+++) :: [a] -> [a] -> [a]
(+++) = foldr (:)

insert :: Ord a => a -> [a] -> [a]
insert x [] = [x]
insert x (y:ys)
    | x < y = x : y : ys
    | otherwise = y : insert x ys

isort :: Ord a => [a] -> [a]
isort xs = foldr insert [] xs

skip :: Eq a => a -> [a] -> [a]
skip x [] = [x]
skip x (y:ys)
    | x == y = (y:ys)
    | otherwise = x : y : ys

compress :: Eq a => [a] -> [a]
compress = foldr skip []

snoc :: a -> [a] -> [a]
snoc x = foldr (:) [x]

reverse' :: [a] -> [a]
reverse' = foldr snoc []

concat' :: [[a]] -> [a]
concat' = foldr (++) []

map' :: (a -> b) -> [a] -> [b]
map' f = foldr (\l ls -> f l : ls) []