import Data.List (minimumBy, transpose, elemIndices)
import Data.Ord (comparing)

type Distance   = Double
type Name       = String
type Direction  = String
type Weight     = (Distance, Direction)

zipD :: [Name] -> [[String]]
zipD ns = [[start ++ "->" ++ des | des <- ns] |  start <- ns]

zipW :: [[Distance]] -> [Name] -> [[Weight]]
zipW ds ns = [zip d n | (d, n) <- zip ds (zipD ns)]

tuplePlus :: Weight -> Weight -> Weight
tuplePlus (d1, n1) (d2, n2) = (d1 + d2, n1 ++ destination)
    where (from, destination) = break (=='-') n2

type RouteMap = [[Weight]]

step :: RouteMap -> RouteMap -> RouteMap
step a b =
    [ [minimumBy (comparing fst) $ zipWith tuplePlus ar bc | bc <- transpose b]
    | ar <- a
    ]

iteration :: Int -> (a -> a) -> a -> a
iteration 0 f x = x
iteration n f x = iteration (n - 1) f (f x)

steps :: Int -> RouteMap -> RouteMap
steps n route = iteration n (step route) route

fix f x =
    if dss == dss' then x else fix f x'
    where
        x' = f x
        dss = [fst $ unzip ds | ds <- x']
        dss' = [fst $ unzip ds | ds <- x]

path :: [[Distance]] -> [Name] -> RouteMap
path dis ns = fix (step route) route
    where
        route = zipW dis ns

infinity :: Fractional a => a
infinity = 1 / 0

i = infinity

graph :: [[Distance]]
graph =
    [
        [0, 6, i, i, i],
        [6, 0, 3, i, i],
        [2, 3, 0, 1, 5],
        [i, i, 1, 0, 4],
        [7, 8, 5, 4, 0]
    ]

names = map (:[]) ['A'..'E']

shortestPath :: [[Distance]] -> [String] -> String -> String -> Weight
shortestPath graph ns fr ds = (graph' !! fr') !! ds'
    where
        graph' = path graph ns
        fr' = head $ elemIndices fr ns
        ds' = head $ elemIndices ds ns

mystep :: RouteMap -> RouteMap -> RouteMap
mystep a b =
    [
        [minimumBy (comparing fst) $ zipWith tuplePlus ar bc | bc <- transpose b ]
        | ar <- a
    ]

steps' :: Int -> RouteMap -> RouteMap -> RouteMap
steps' 1 a b = step a b
steps' n a b
    | even n = steps' (div n 2) a (mystep b b)
    | otherwise = steps' (div n 2) (mystep a b) (step b a)

path' :: Int -> [[Distance]] -> [Name] -> RouteMap
path' n dis ns = steps' n route route
    where
        route = zipW dis ns