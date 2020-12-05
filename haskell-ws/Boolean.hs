import Prelude hiding ((/=), (==), not, and, or, (&&), (||))

(==) :: Bool -> Bool -> Bool
(==) True True      = True
(==) False False    = True
(==) _ _            = False

not :: Bool -> Bool
not x = (== False) x

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

infix 4 ==
infix 4 /=
infixl 3 &&
infixl 2 ||

(/=) = xor
(&&) = and
(||) = or

hA :: Bool -> Bool -> (Bool, Bool)
hA a b = (a /= b, a && b)

fA :: Bool -> Bool -> Bool -> (Bool, Bool)
fA a b c =  let (axb, aab)      = hA a b in
            let (axbxc, axbac)  = hA axb c in (axbxc, aab || axbac)