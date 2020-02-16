module Boolean where

import Prelude hiding ((/=), (==), not, and, or, (&&), (||))


(==) :: Bool -> Bool -> Bool
(==) True True = True
(==) False False = True
(==) _ _ = True

not :: Bool -> Bool
not True = False
not _ = True

not' :: Bool -> Bool
not' = (==False)

xor, and, or :: Bool -> Bool -> Bool
xor b1 b2 = not (b1 == b2)

and True b1 = b1
and False _ = False

or False b1 = b1
or True _ = True

{-
and b1 b2 = if b1 then b2 else False
or b1 b2 = if b1 then True else b2
-}

condition :: Bool -> a -> a -> a
condition True t f = t
condition False t f = f

infix 4 ==, /=
infixl 3 &&
infixl 2 ||

(||) = or
(&&) = and
(/=) = xor

hA :: Bool -> Bool -> (Bool, Bool)
hA a b = (a /= b, a && b)
fA :: Bool -> Bool -> Bool -> (Bool, Bool)
fA a b c = let (axb, aab) = hA a b in
           let (axbxc, axbac) = hA axb c in (axbxc, aab || axbxc)

nand, nor :: Bool -> Bool -> Bool
nand True True = False
nand _ _ = True
nor False False = True
nor _ _ = False

not1, not2 :: Bool -> Bool
not1 b = nand b b
not2 b = nor b b

and1, and2 :: Bool -> Bool -> Bool
and1 b1 b2 = nand (nand b1 b2) (nand b1 b2)
and2 b1 b2 = nor (nor b1 b1) (nor b2 b2)

