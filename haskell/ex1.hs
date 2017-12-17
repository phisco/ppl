module A170311 where
myReverse :: [a] -> [a]
myReverse[]=[]
myReverse (x:xs) = myReverse xs ++ [x]

myRange :: Int -> Int -> [Int]

myRange a b = if a>b
    then error "L>H"
    else if a<b
        then [a] ++ (myRange (a + 1) b)
        else [a]

myRange' :: Int -> Int -> [Int]
myRange' a b  -- guards
    | a>b  = error "error"
    | a==b = [a]
    | otherwise = a : myRange' (a + 1) b

myFoldL :: (a -> a -> a)-> a-> [a] -> a
myFoldL f x [] = x
myFoldL f x (y : ys) = myFoldL f (f x y) ys

zip' :: [a] -> [b] -> [(a,b)]
zip' _ [] = []
zip' [] _ = []
zip' (x : xs) (y : ys) = (x,y) : (zip' xs ys)

-- TODO: myFoldR

listComprehension=[x*2 | x <- [1 .. 10]]

factorial :: Integer -> Integer -- Int would overflow
factorial n = product [1 .. n]

first5 = take 5 [1, 2 ..]

--type sum

data TrafficLight = Red | Yellow | Green -- deriving (Eq) -- cool but...

instance Eq TrafficLight where
    --_ == _ = False --ordered pattern matching selectivity
    Red == Red = True
    Yellow == Yellow = True
    Green == Green = True
    _ == _ = False --ordered pattern matching 

instance Ord TrafficLight where
    -- compare x x = EQ
    compare Red _ = LT -- do not work for EQ
    compare Green _ = GT
    compare Yellow Red = GT
    compare Yellow Green = LT

-- product type

data Point = Point Float Float deriving (Show)

getX (Point x _ ) = x
getY (Point _ y ) = y

--accessors

data Person = Person{
    firstName :: String,
    age :: Int,
    email :: String
} --  deriving (Show)

instance Show Person where
    show (Person x y z) = "Name: " ++ x ++ ", Age: " ++ (show y) ++ ", Email: " ++ z


