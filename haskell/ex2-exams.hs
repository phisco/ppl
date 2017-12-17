--20170705
--tcompose
data Tree a = Nil | Leaf a | Branch (Tree a)(Tree a) deriving (Show, Eq)
tcompose :: (a -> b -> c ) -> Tree a -> Tree b -> Tree c

tmap :: (a -> b) -> Tree a -> Tree b
tmap _ Nil = Nil
tmap f (Leaf a) = Leaf (f a)
tmap f (Branch l r) = Branch (tmap f l) (tmap f r)

tcompose _ Nil _ = Nil
tcompose f (Leaf a) t2 = tmap (f a) t2
tcompose f (Branch l r) t2 = Branch (tcompose f l t2) (tcompose f r t2)


fringe :: Tree a -> [a]
fringe t = fringe' t [] where
    fringe' Nil xs = xs
    fringe' (Leaf a) xs = a:xs
    fringe' (Branch l r) xs = (fringe l) ++ (fringe r) ++ xs

revtree :: Tree a -> Tree a
revtree t = t1 where
    (t1, _) = revtree' t (reverse (fringe t)) where
        revtree' Nil xs = (Nil, xs)
        revtree' (Leaf _) (x:xs) = (Leaf x , xs)
        revtree' (Branch l r) xs = let (l', xs') = revtree' l xs ;
                                        (r', xs'') = revtree' r xs'
                                    in (Branch l' r', xs'')

data Color = Blue | Yellow deriving (Eq, Show)
data Ttree a = Tleaf a Color | TBranch a Color (Ttree a) (Ttree a) (Ttree Color)

yellowSubTrees :: Ttree a -> [Ttree a]
yellowSubTrees t = yellowSubTrees' t [] where
    yellowSubTrees' (Tleaf _ Blue) xs = xs
    yellowSubTrees' x@(Tleaf _ Yellow) xs = x:xs
    yellowSubTrees' x@(TBranch _ color l m r) xs
                    | (noBlues x) = x:xs
                    | otherwise = (yellowSubTrees m) ++ (yellowSubTrees m)
