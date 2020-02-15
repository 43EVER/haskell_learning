doubleMe x = x + x
doubleUs x y = doubleMe x + doubleMe y
doubleSmallNumber x = 
    if x > 100
        then x
    else 
        doubleMe x
doubleSmallNumber' x = 
    doubleSmallNumber x + 1

-- list operation
-- head tail
-- init last
-- length null elem

-- set comprehansion
setCanDivide3Or2 = [x * 2 | x <- [1..10], (x `mod` 3) == 0]
boomBangs xs =
    [if x < 10 then "BOOM!" else "BANG!" | x <- xs, odd x]
length' xs = sum [1 | _ <- xs]
        
removeNonUpperCase :: [Char] -> [Char]
removeNonUpperCase st =
    [ c | c <- st, c `elem` ['A'..'Z']]


-- Tuple
-- fst snd
isTriangleLegal (x, y, z) =
    x + y > z && x + z > y && y + z > x && x^2 + y^2 == z^2



-- 类型和类型类
addThree :: Int -> Int -> Int -> Int
addThree x y z = x + y + z

factorial :: Integer -> Integer
factorial n = product [1..n]

circumference :: Float -> Float
circumference r = 2 * pi * r

circumference' :: Double -> Double
circumference' r = 2 * pi * r