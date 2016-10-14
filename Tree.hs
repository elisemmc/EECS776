{-# LANGUAGE GADTs, KindSignatures #-}

import Control.Monad (ap)

data Tree :: * where
    Leaf :: Int -> Tree
    Node :: Tree -> Tree -> Tree
    deriving (Show,Eq,Ord)

-- Write a function adds up all the leaves of the tree, using regular recursion (no monads)
summation :: Tree -> Int
summation (Leaf a) = a
summation (Node a b) = summation a + summation b

-- Write the same function, using the state monad
newtype State s a = State (s -> (a,s))
instance Monad (State s) where
    return x = State $ \s -> (x,s)  
    (State h) >>= f = State $ \s -> let (a, newState) = h s  
                                        (State g) = f a
                                        (r, outState) = g newState 
                                    in  (r, outState)

-- Templates
instance Applicative (State s) where
    pure = return
    (<*>) = ap

instance Functor (State s) where
    fmap f m = pure f <*> m

add :: Int -> State Int ()
add a = State $ \b -> ((), a + b)

runState :: State s a -> s -> (a,s)
runState (State s) a = s a

summationM :: Tree -> State Int ()
summationM (Leaf a)= do
    add a
summationM (Node a b) = do
    summationM a
    summationM b

summation' :: Tree -> Int
summation' a = case runState (summationM a) 0 of (_,s) -> s

-- get = State $ \s -> (s,s)
-- put newState = State $ \s -> ((), newState)

a = (Node (Leaf 4) (Leaf 3))
b = Leaf 4
c = (Node (Node (Leaf 9) (Leaf 7)) (Leaf 3))

--type Stack = [Int]

--pop :: State Stack Int  
--pop = State $ \(x:xs) -> (x,xs)  
  
--push :: Int -> State Stack ()  
--push a = State $ \xs -> ((),a:xs)  

--stackManip :: State Stack Int  
--stackManip = do  
  --  push 3  
  --  a <- pop  
  --  pop  

-- Websites Used
-- http://stackoverflow.com/questions/29340070/how-should-i-define-a-binary-tree-in-haskell