{-# LANGUAGE GADTs, KindSignatures #-}

import Control.Monad (ap)

-- The top level function ...

numTree :: Eq a => Tree a -> Tree Integer

-- ... and the function which does all the work:

numberTree :: Eq a => Tree a -> State a (Tree Integer)

-- Its structure mirrors exactly the structure of the earlier program to
-- sum the tree.

numberTree Nil = return Nothing

numberTree (Node x t1 t2)
  = do num <- numberNode x
       nt1 <- numberTree t1
       nt2 <- numberTree t2
       return (Node num nt1 nt2)

-- The work of the algorithm is done node by node, hence the function

numberNode :: Eq a => a -> State a Integer

numberNode x = State (nNode x)

--  
-- Looking up a value in the table; will side-effect the table if the value
-- is not present.

nNode :: Eq a => a -> (Table a -> (Table a , Integer))
nNode x table
  | elem x table        = (table      , lookup x table)
  | otherwise           = (table++[x] , integerLength table)
    where
      integerLength = toInteger.length
  
-- Looking up a value in the table when known to be present

lookup :: Eq a => a -> Table a -> Integer

lookup x tab = 
    locate 0 tab
           where
             locate n (y:ys) = 
                 if x==y then n else locate (n+1) ys

-- Extracting a value froma state monad.

runST :: State a b -> b
runST (State st) = snd (st [])

-- The top-level function defined eventually.

numTree = runST . numberTree

-- Example tree

egTree :: Tree String
 
egTree = Node "Moon"
               (Node "Ahmet" Nothing Nothing)
               (Node "Dweezil"  
                        (Node "Ahmet" Nothing Nothing) 
                        (Node "Moon" Nothing Nothing))