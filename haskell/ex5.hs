module ESE20171212 where

import Control.Applicative
import Control.Monad 
-- misses the monad state
newType State st a = State {runStateM :: st -> (st, a)} -- ??
data Tree a = Leaf a | Branch (Tree a) (Tree a)

instance Show a => Show (Tree a) where
    show (Leaf a) = show a
    show (Branch l r) = "< " ++ show l ++ "  |  " ++ show r ++ " >"

numberTree :: Tree a -> Tree (a, Int)
numberTree t = fst (numberTree' t 0) where
                numberTree' (Leaf a) st = (Leaf (a, st+1), st+1)
                numberTree' (Branch l r) st = 
                    let (l', st') = numberTree' l st
                        (r', st'') = numberTree' r st'
                    in (Branch l' r', st'')

put :: s -> State s ()
put new = State (\_ -> ((),new))

get :: State s s
get = State (\s -> (s,s) )

runStateM :: State state a -> state -> a
runStateM (State f) st = fst (f st)

mapTreeM :: (Monad m) => (t -> m a) -> Tree t -> m (Tree a)
mapTreeM f (Leaf a) = do
                        b <- f a
                        return (Leaf b)

mapTreeM f (Branch l r) = do
    l' <- mapTreeM f l
    r' <- mapTreeM f r
    return (Branch l' r')

numberTreeM :: Tree a -> State Int (Tree (a, Int))
numberTreeM = mapTreeM number
    where number value = do
                            cur <- get
                            put (cur+1)
                            return (value, cur)
-- get >>= (\cur -> put (cur+1) >> (\_ -> returnState)

type Log = [String]
newType Logger a = Logger {run :: (a,Log) }
instance (Show a) => Show (Logger a) where
    show (Logger a) = show a

instance (Eq a) => Eq (Logger a) where
    show (Logger a) = show a
instance (Eq a) => Eq (Logger a) where
    Logger (x,y) /= Logger (a,b) = (x/=a) || (y/=b)
instance Functor Logger where
    fmap f log =
        let (a,ls) = run log
            a' = f a
            in Logger (b, ls)


llap lf la = 
    let (f, lsf) = run lf
        ln = fmap f la
        (b, l) = run ln
    in Logger (b, l++lsf)

instance Applicative Logger where
    pure a = Logger (a, [])
    (<*>) = llap

instance Monad Logger where
    return = pure
    m >>= f = 
        let 
            (a,l) = run m
            m'  = f a
            (b, l') = run m'
        in Logger (b , l++l')

logPlus1 :: (Num a) => a -> Logger a
logPlus1 a = Logger (a+1, ["+1"])
MultiplyBy2 :: (Num a) => a -> Logger a
main logger = do
    v <- logger
    p1 <- logPlus1 v
    m2 <- MultiplyBy2 p1
    return m2
        
