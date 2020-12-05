nand, nor :: Bool -> Bool -> Bool
nand True True = False
nand _ _ = True


nor False False = True
nor _ _ = False


not1, not2 :: Bool -> Bool
not1 b = nand b b
not2 b = nor b b


and1, and2 :: Bool -> Bool -> Bool
and1 a b = not1 $ nand a b
and2 a b = nor (not2 a) (not2 b)


or1, or2 :: Bool -> Bool -> Bool
or1 a b = nand (not a) (not b)

or2 a b = not2 $ nor a b