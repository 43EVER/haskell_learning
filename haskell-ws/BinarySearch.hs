search :: (Ord a) => a -> [a] -> Bool
search a [] = False
search a xs     | m < a = search a right
                | m > a = search a left
                | otherwise = True
                where (left, m:right) = splitAt (length xs `div` 2) xs

search' :: (Ord a) => a -> [a] -> [a]
search' a [] = []
search' a xs    | m < a = search' a right
                | m > a = search' a left
                | otherwise = [a] ++ search' a left ++ search' a right
                where (left, m:right) = splitAt (length xs `div` 2) xs