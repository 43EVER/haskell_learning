-- $
dollarTest1 :: (Num a, Floating a) => [a]
dollarTest1 = map ($ 3) [(4+), (/4), (+4), (*4), (^2), sqrt]


-- 函数组合
combinTesst1 :: Integer
combinTesst1 = negate . (*3) $ 5

myAbs :: [Integer] -> [Integer]
myAbs xs = map (negate . abs) xs

-- replicate 100 (product (map (*3) (zipWith max [1,2,3,4,5] [4,5,6,7,8])))

-- replicate 100 $ product . map (*3) $ zipWith max [1, 2, 3, 4, 5] [4, 5, 6, 7, 8]

oddSquareSum = sum . takeWhile (<10000) . filter odd . map (^2) $ [1..]

addThree :: (Integral a) => a -> a -> a -> a
addThree x y z = x + y + z