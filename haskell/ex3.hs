-- infinite tree
data Itree a = Node a (Itree a) (Itree a)

-- show a
instance Show a => Show (Itree a) where
    show (Node v l r) = "Node (... " ++ show v ++ " ...) "


-- show can be derived when type system is enough to infere it

costItree :: a -> Itree a

costItree v = Node v (costItree v) (costItree v)

--list2Itree, which takes an infinite list L and returns an Itree T in which every path
--from the root downwards contains the 
--same sequence of values of those in L (of course, all values in L but the
--first are duplicated in T).

list2Itree :: [a] -> Itree a

list2Itree (x:xs) = Node x (list2Itree xs) (list2Itree xs)

costList2Itree x = list2Itree [x,x ..]

-- fmap :: Functor f => (a -> b) -> f a -> f b
-- itfmap :: Functor f => (a->b) -> f a -> f b
-- not working because can infer it

itfmap f (Node v l r) = Node (f v) (itfmap f l) (itfmap f r)

instance Functor Itree where
    fmap = itfmap

data Tree a = Branch a (Tree a) (Tree a) | Leaf a deriving Show

takeLevelsn :: Int -> Itree a -> Tree a
takeLevelsn 0 (Node v _ _) = Leaf v
takeLevelsn n (Node v l r) = Branch v (takeLevelsn (n-1) l) (takeLevelsn (n-1) r)

applyAtLevel :: (a -> a) -> (Int -> Bool) -> Itree a -> Itree a
applyAtLevel f p t = applyAtLevel' f p t 0 where
    applyAtLevel' f p (Node v l r) lvl = Node (if p lvl then f v else v) 
                                        (applyAtLevel' f p l (lvl+1)) 
                                        (applyAtLevel' f p r (lvl+1))

data Color = Blue | Yellow deriving Show
data Ttree a = Tleaf a Color | Tbranch a Color (Ttree a) (Ttree a) (Ttree a) deriving Show

ttreemap :: (a -> b) -> Ttree a -> Ttree b
ttreemap f (Tleaf v c) = Tleaf (f v) c
ttreemap f (Tbranch v c l m r) = Tbranch (f v) c (ttreemap f l) (ttreemap f m) (ttreemap f r)

ttreefoldr :: (a -> b -> b) -> b -> Ttree a -> b
ttreefoldr f b (Tleaf a _) = f a b
ttreefoldr f b (Tbranch  v _ l m r) =
    let v1 = ttreefoldr f b l
        v2 = ttreefoldr f v1 m
        v3 = ttreefoldr f v2 r
    in f v v3
