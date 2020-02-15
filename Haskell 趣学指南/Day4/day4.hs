import Data.Char
import Data.Function
import Data.List
import qualified Data.Map as Map
import My.Sphere as Sphere

encode :: Int -> [Char] -> [Char]
encode a = map (chr . (+ a) . ord)

phoneBook =  
    [("betty","555-2938"),  
    ("betty","342-2492"), 
    ("bonnie","452-2928"), 
    ("patsy","493-2928"), 
    ("patsy","943-2929"), 
    ("patsy","827-9162"), 
    ("lucille","205-2928"),  
    ("wendy","939-8282"),  
    ("penny","853-2492"), 
    ("penny","555-2111")]

findKey :: (Eq k) => k -> [(k, v)] -> v
findKey key xs = snd . head . filter (\ (k, v) -> key == k) $ xs

findKey' :: (Eq k) => k -> [(k, v)] -> Maybe v
findKey' key [] = Nothing
findKey' key ((k, v) : xs) =
    if key == k then
        Just v
    else
        findKey' key xs

findKey'' :: (Eq k) => k -> [(k, v)] -> Maybe v
findKey'' key = foldr 
    (\(k, v) acc -> if key == k then Just v else acc) Nothing

fromList' :: (Ord k) => [(k, v)] -> Map.Map k v
fromList' = foldr
    (\(k, v) acc -> Map.insert k v acc) Map.empty

phoneBookToMap :: (Ord k) => [(k, String)] -> Map.Map k String
phoneBookToMap = Map.fromListWith (\number1 number2 -> number1 ++ ", " ++ number2)

phoneBookToMap' :: (Ord k) => [(k, a)] -> Map.Map k [a]
phoneBookToMap' xs = Map.fromListWith (++) $ map (\(k, v) -> (k, [v])) xs