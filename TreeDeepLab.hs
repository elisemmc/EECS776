{-# LANGUAGE GADTs, KindSignatures #-}

import Control.Monad (ap)

data State :: * -> * -> * where
    Add :: s -> State s a
    Return :: a -> State s a
    Bind :: State s a -> ( a -> State s b ) -> State s b
    --deriving( Show )

data Tree :: * where
    Leaf :: Int -> Tree
    Node :: Tree -> Tree -> Tree
    deriving (Show,Eq,Ord)

-- Write the same function, using the state monad
instance Monad (State s) where
    return = Return
    (>>=) = Bind

-- Templates
instance Applicative (State s) where
    pure = return
    (<*>) = ap

instance Functor (State s) where
    fmap f m = pure f <*> m

add :: Int -> State Int ()
add = Add

runState :: State s a -> s -> (a,s)
runState (Return a) s = (a,s)
runState (Add a) s = ((),a+s)

summationM :: Tree -> State Int ()
summationM (Leaf a)= do
    add a
summationM (Node a b) = do
    summationM a
    summationM b

summation :: Tree -> Int
summation a = case runState (summationM a) 0 of (_,s) -> s