insert :: Ord a => a -> [a] -> [a]
insert x [] = [x]
insert x (y:ys) | x < y = x:y:ys
                | otherwise = y : insert x ys

insertionSort :: Ord a => [a] -> [a]
insertionSort [] = []
insertionSort (x:xs) = insert x (insertionSort xs)

swaps :: Ord a => [a] -> [a]
swaps [] = []
swaps [x] = [x]
swaps (x1:x2:xs)
    | x1 > x2 = x2 : swaps (x1 : xs)
    | otherwise = x1 : swaps (x2 : xs)

fix :: Eq a => (a -> a) -> a -> a
fix f x | x == x' = x
        | otherwise = fix f $! x'
        where x' = f x

bubbleSort :: Ord a => [a] -> [a]
bubbleSort = fix swaps

bubbleSort' :: Ord a => [a] -> [a]
bubbleSort' xs  | xs' == xs = xs
                | otherwise = bubbleSort' xs'
                where xs' = swaps xs

bubbleSort'' :: Ord a => [a] -> [a]
bubbleSort'' [] = []
bubbleSort'' xs = bubbleSort'' initialElements ++ [lastElement]
    where   swappedxs       = swaps xs
            initialElements = init swappedxs
            lastElement     = last swappedxs

delete :: Eq a => a -> [a] -> [a]
delete _ [] = []
delete x (l:ls) | x == l = ls
                | otherwise = l:delete x ls

selectionSort :: Ord a => [a] -> [a]
selectionSort []    = []
selectionSort xs    = mini : selectionSort xs'
    where   mini    = minimum xs
            xs'     = delete mini xs

quickSort :: Ord a => [a] -> [a]
quickSort [] = []
quickSort (x:xs) = quickSort left ++ [x] ++ quickSort right
    where   left = filter (<x) xs
            right = filter (>=x) xs

filterSplit :: (a -> Bool) -> [a] -> ([a], [a])
filterSplit _ [] = ([], [])
filterSplit f (x:xs)    | f x = (x:l, r)
                        | otherwise = (l, x:r)
    where (l, r) = filterSplit f xs

quickSort' :: Ord a => [a] -> [a]
quickSort' [] = []
quickSort' [x] = [x]
quickSort' (x:xs) = quickSort' l ++ [x] ++ quickSort' r
    where (l, r) = filterSplit (<x) xs

merge :: Ord a => [a] -> [a] -> [a]
merge xs [] = xs
merge [] ys = ys
merge (x:xs) (y:ys) | x < y     = x : merge xs (y:ys)
                    | otherwise = y : merge (x:xs) ys

msort :: Ord a => [a] -> [a]
msort [] = []
msort [x] = [x]
msort xs = merge (msort x1) (msort x2)
    where
        (x1, x2) = splitAt l xs
        l        = length xs `div` 2

fix' :: (a -> a) -> a
fix' f = f (fix' f)

factorial' :: Int -> Int
factorial' = fix' (\f n -> if n == 0 then 1 else n * f (n - 1))


{-
    (\f n -> if n == 0 then 1 else n * f (n - 1)) 的 type 是 (Int -> Int) -> Int -> Int
    传进 fix', 
-}