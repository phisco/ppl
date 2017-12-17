--import Control.Monad.State
import Control.Monad

newtype State st a = State {runState :: st -> (a, st)}
-- new type that takes 2 arguments
--
-- instance Functor (State s) where
--     fmap f (State g) = State $ (\st ->
--     let (a, st') = g st
--     in (f a, st') )

instance Functor (State s) where
    fmap f st = State $ (\(a,st')-> (f a, st')) . runState st
                                                --(\st -> (a,st))
                        -- (\st -> (f a, st))

instance Applicative (State s) where
    pure a = State $ (\st -> (a, st))
    (State ff) <*> (State fa) = State $ (\s -> 
                let (f', s') = ff s
                    (a, s'') = fa s'
                    in (ff a, s'') )

-- MONADIC CLUB
-- 1 left identity : a >>= f === f a
-- 2 right identity : a >>= return === a
-- 4 associativity (a >>= f) >>= g === m >>= (\x f x >>= g)

instance Monad (State s) where
    return = pure
    (State a) >>= f = State $ \s ->
        let (a', s') = a s
            (State b) = f a'
            (b', s'') = b s'
        in (b', s'')

getState :: State state state
getState = State (\state -> (state, state))

putState :: state -> State state ()
putState new = State (\_ -> (new, ()))

mapListM :: (t -> State st a) -> [t] -> State st [a]
-- mapListM f (x:xs) = f x >= (\x1 -> mapListM f xs >>= (\xs1 -> return (x:xs)))
mapListM f [] = do return []
mapListM f (x:xs) = do
    x1 <- f x -- x1 is a state
    xs1 <- mapListM f xs
    return (x1:xs1)

numList :: Num st => [st] -> State st [(st,st)]
numList list = mapListM helper list where
    helper x = do cur <- getState
                    putState (x+cur)
                    return (x, x+cur)

