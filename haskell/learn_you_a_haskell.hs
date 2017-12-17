import Data.List.Split (splitOn)
import qualified Data.Map as Map
import System.IO
import System.Environment

len :: [a] -> Integer
len []=0
-- suggested len xs = foldr (\x -> (+) 1) 0 xs
len (x:xs) = 1+ len xs

checkRedefined f f' x = f x == f' x

doubleSmallNumber x = if x>100 then x else x*2

what []="empty"
what (c:_)
    | c `elem` ['A' .. 'Z']= "upper"
    | c `elem` ['a' .. 'z'] = "lower"
    | otherwise = "wtf"

triangles = [ (a,b,c) | c <- [1..10], b <- [1..c], a <- [1..b], 
                        a^2 + b^2 == c^2, a+b+c == 24] 


data Point a = Point a a
pointx (Point x _) = x
pointy (Point _ y) = y

data Point' a = Point' { x, y :: a}

data Tree a = Leaf a | Branch (Tree a) (Tree a)

fringe :: Tree a -> [a]
fringe (Leaf x) = [x]
fringe (Branch left right) = fringe left ++ fringe right

quicksort [] = []
quicksort (x:xs) = quicksort [y|y<-xs, y<=x]
                    ++ [x] ++
                    quicksort [y|y<-xs, y>x]

sign x 
    | x>0 = 1
    | x==0 = 0
    | x<0 = -1

take' m ys = case (m, ys) of
    (0, _) -> []
    (_, []) -> []
    (n,x:xs) -> x : take' (n-1) xs

data Rat = Rat !Integer !Integer deriving Eq

simplify (Rat x y) = let g = gcd x y
                    in Rat (x `div` g) (y `div` g)

makeRat x y = simplify (Rat x y)

instance Num Rat where
    (Rat x y) + (Rat x' y') = makeRat (x*y'+x'*y) (y*y')
    (Rat x y) - (Rat x' y') = makeRat (x*y'-x'*y) (y*y')
    (Rat x y) * (Rat x' y') = makeRat (x*x') (y*y')
    abs (Rat x y) = makeRat (abs x) (abs y)
    signum (Rat x y) = makeRat (signum x * signum y) 1
    fromInteger x = makeRat x 1

(Rat x y) // (Rat x' y') = Rat x y * Rat y' x'

--instance Fractional Rat where
--  (Rat x y) / (Rat x' y') = (Rat x y) * (Rat y' x')

instance Ord Rat where
    (Rat x y) <= (Rat x' y') = x*y'<=x'*y

instance Show Rat where
    show (Rat x y) = show x ++ "/" ++ show y

getListAndSort= do{
    putStrLn "tell me a list";
    list <- getLine;
    putStrLn ("received "++list);
    let strippedList = [x | x<-splitOn " " list, x/= ""];
        numOfList = map show (quicksort (map (read::String->Int) strippedList)) in 
    putStrLn (foldl ((++).(++"  ")) "" numOfList );
}

readfile = do{
    args<- getArgs;
    handle <- openFile (head args) ReadMode;
    contents <- hGetContents handle;
    putStr contents;
    hClose handle;
}
main= readfile

bmiTell :: (RealFloat a) => a -> a -> String
bmiTell weight height
    | bmi <= 18.5 = "underweight"
    | bmi <= 25.0 = "normal"
    | bmi <= 30.0 = "fat"
    | otherwise = "whale!!"
    where bmi = weight / height^2

calcBmis :: (RealFloat a) => [(a,a)] -> [a]
calcBmis xs = [bmi w h| (w,h)<- xs]
    where bmi w h = w / h^2
--with let instead of where
calcBmis' :: (RealFloat a) => [(a,a)] -> [a]
calcBmis' xs = [bmi | (w,h) <- xs, let bmi = w / h^2]
-- calcBmis' xs = [bmi w h | (w,h) <- xs, let bmi w h = w / h^2] -- also right

describeList :: [a] -> String
describeList xs = case xs of    [] -> "empty"
                                [x] -> "singleton"
                                xs -> "longer"

maximum' :: (Ord a) => [a] -> a
maximum' [] = error "max of empty list"
maximum' [x] = x
maximum' (x:xs) = max x (maximum' xs)

replicate' :: (Ord b, Num b) => b -> a -> [a]
replicate' n x
    | n <= 0 = []
    | otherwise = x:replicate' (n-1) x

repeat' :: a -> [a]
repeat' x = x:repeat' x

take'' :: (Num n, Ord n) => n -> [a] -> [a]
take'' n x
    | n <= 0 = []
take'' _ [] = []
take'' n (x:xs) = x:take'' (n-1) xs

zip' :: [a] -> [b] -> [(a,b)]
zip' (x:xs) (y:ys) = (x,y): zip' xs ys
zip' _ [] = [] 
zip' [] _ = []

zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _ _ [] = []
zipWith' _ [] _ = []
zipWith' f (x:xs) (y:ys) = f x y:zipWith' f xs ys

flip' :: (a->b->c) -> (b->a->c)
-- flip' f = g 
   --  where g y x = f x y
flip' f x y = f y x

map' :: (a->b) -> [a] -> [b]
map' _ []=[]
map' f (x:xs) = f x:map f xs

filter' :: (a -> Bool) -> [a] -> [a]
filter' _ [] = []
filter' p (x:xs)
    | p x = x:filter' p xs
    | otherwise = filter' p xs

largeDivisible :: (Integral a) => a -> a
largeDivisible x = head $ filter (\p -> p `mod` x == 0) [100000,99999..]

takeWhile' :: (a -> Bool) -> [a] -> [a]
takeWhile' _ [] = []
takeWhile' p (x:xs)
    | p x = x:takeWhile' p xs
    | otherwise = []

chain :: (Integral a) => a -> [a]
chain 1 = [1]
chain n
    | even n = n:chain (n `div` 2)
    | odd n = n:chain (n*3 + 1)

-- !! = (list !! num) is the getter of  num from list

flip'' :: (a->b->c) -> (b->a->c)
flip'' f = \x y -> f y x

elem' :: (Eq x) => x -> [x] -> Bool
elem' x = foldl (\acc y -> ((y==x) ||  acc) ) False
-- not working for infinite lists, need recursion for it 

map'':: (a->b) -> [a] -> [b]
map'' f = foldr (\x acc -> f x:acc) [] -- best solution with foldr
-- map'' f = foldl (\acc x -> f x:acc) [] -- reverse order but infinite lists
-- map'' f = foldl (\acc x -> acc ++[f x]) [] -- reverse order and does not
-- work with infinite lists
-- : less expensive than ++

-- foldl func acc foldable -- func ~ (\acc x -> ...)
-- foldr func acc foldable -- func ~ (\x acc -> ...)
-- foldr => work on infinite lists!!!!

-- foldr1 foldl1 last element is the accumulator => error if empty list passed
--

head' :: [a] -> a
-- head' = foldr1 (\x _ -> x)
head' = foldr1 const
last' :: [a] -> a
last' = foldl1 seq

-- takewhile allow filtering on monotonic infinite lists
-- takewhile predicate list
--

findKey' :: (Eq k) => k -> [(k,v)] -> Maybe v
findKey' key = foldr (\(k,v) acc -> if k==key then Just v else acc) Nothing

-- record syntax gives accessors directly
data Person = Person { firstName :: String
, lastName :: String
, age :: Int
, height :: Float
, phoneNumber :: String
, flavor :: String
} deriving (Show, Read, Eq)
-- also allows to build with named arguments, disregarding the order of the
-- parameters
-- Read allows to "read" from a string and convert to the class


data LockerState = Taken | Free deriving (Show, Eq)
type Code = String
type LockerMap = Map.Map Int (LockerState,Code)

-- can fail because locker already taken or because locker not exists
lockerLookup :: Int -> LockerMap -> Either String Code
lockerLookup lockerNumber map = case Map.lookup lockerNumber map of 
    Nothing -> Left $ "Locker number " ++ show lockerNumber ++ " doesn't exist!"
    Just (state, code) -> if state /= Taken then Right code
                            else Left $ "Locker " ++ show lockerNumber ++ " is already taken"

data Tree' a = EmptyTree | Node a (Tree' a) (Tree' a) deriving (Show, Read, Eq)
singleton :: a -> Tree' a
singleton x = Node x EmptyTree EmptyTree
treeInsert :: (Ord a) => a -> Tree' a -> Tree' a
treeInsert x EmptyTree = singleton x
treeInsert x n@(Node a l r) 
    | x==a = n
    | x<a = Node a (treeInsert x l) r
    | x>a = Node a l (treeInsert x r)

treeElem :: (Ord a) => a -> Tree' a -> Bool
treeElem x EmptyTree = False
treeElem x (Node a left right)
    | x==a = True
    | x < a = treeElem x left
    | x> a = treeElem x right

data TrafficLight = Red | Yellow | Green
instance Eq TrafficLight where
    Red == Red = True
    Green = Green = True
    Yelow = Yellow = True
    _ == _ = False
instance Show TrafficLight where
    show Red = "red"
    show Yellow = "yellow"
    show Green = "green"

-- Bool a la javascript
class YesNo a where
    yesno :: a -> Bool
instance YesNo Int where
    yesno 0 = False
    yesno _ = True
instance YesNo [a] where
    yesno [] = False
    yesno _ = True
instance YesNo Bool where
    yesno  = id
instance YesNo (Maybe a) where
    yesno (Just _) = True
    yesno Nothing = False
instance Functor Tree' where
    fmap f EmptyTree = EmptyTree
    fmap f (Node x leftsub rightsub) = Node (f x) (fmap f leftsub) (fmap f rightsub)

