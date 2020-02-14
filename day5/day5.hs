import qualified Data.Map as Map


data Car = Car String String Int deriving (Show)

tellCar :: Car -> String
tellCar (Car c m y) = "This " ++ c ++ " " ++ m ++ " was made in " ++ show y

data Car' a b c = Car' { company :: a
                       , model :: b
                       , year :: c 
                       } deriving (Show)

tellCar' :: (Show a) => Car' String String a -> String
tellCar' (Car' {company = c, model = m, year = y}) = "This " ++ c ++ " " ++ m ++ " was made in " ++ show y



data Vector a = Vector a a a deriving (Show)

vecPlus :: (Num t) => Vector t -> Vector t -> Vector t
(Vector i j k) `vecPlus` (Vector l m n) = Vector (i + l) (j + m) (k + n)

vecMult :: (Num t) => Vector t -> t -> Vector t
(Vector i j k) `vecMult` m = Vector (i * m) (j * m) (k * m)

vecScalarMult :: (Num t) => Vector t -> Vector t -> t
(Vector i j k) `vecScalarMult` (Vector l m n) = i * l + j * m + k * n


data Person = Person { firstName :: String
                     , lastName :: String
                     , age :: Int
                     } deriving (Show, Eq, Read)


data Day = Monday | Tuesday | Wednesday | Thursday | Friday | Saturday | Sunday
            deriving (Eq, Ord, Show, Read, Bounded, Enum)

type PhoneNumber = String
type Name = String
type PhoneBook = [(Name, PhoneNumber)]

phonebook :: PhoneBook
phonebook = 
    [("betty","555-2938")      
    ,("bonnie","452-2928")      
    ,("patsy","493-2928")      
    ,("lucille","205-2928")      
    ,("wendy","939-8282")      
    ,("penny","853-2492")      
    ]

inPhoneBook :: Name -> PhoneNumber -> PhoneBook -> Bool
inPhoneBook name pnumber pbook = (name, pnumber) `elem` pbook

type AssocList k v = [(k,v)]



data LockerState = Taken | Free deriving (Show, Eq)
type Code = String
type LockerMap = Map.Map Int (LockerState, Code)

lockerLookup :: Int -> LockerMap -> Either String Code
lockerLookup lockerNumber map =
    case Map.lookup lockerNumber map of
        Nothing -> Left $ "Locker number " ++ show lockerNumber ++ " doesn't exist"
        Just (state, code) -> if state /= Taken
                                then Right code
                                else Left $ "Locker " ++ show lockerNumber ++ " is already taken!"

lockerMap :: LockerMap
lockerMap = Map.fromList
                    [
                        (1, (Free, "liuyang")),
                        (2, (Taken, "ly")),
                        (3, (Free, "dengkaiming")),
                        (4, (Free, "dkm"))
                    ]