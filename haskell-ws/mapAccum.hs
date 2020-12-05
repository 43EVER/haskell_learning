mapAccumR :: (acc -> x -> (acc, y)) -> acc -> [x] -> (acc, [y])
mapAccumR _ s [] = (s, [])
mapAccumR f s (x:xs) = (s'', y : ys)
    where
        (s'', y) = f s' x
        (s', ys) = mapAccumR f s xs

mapAccumL :: (acc -> x -> (acc, y)) -> acc -> [x] -> (acc, [y])
mapAccumL _ s [] = (s, [])
mapAccumL f s (x:xs) = (s'', y:ys)
    where
        (s'', y) = f s' x
        (s', ys) = mapAccumL f  xs