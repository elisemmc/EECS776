{-# LANGUAGE KindSignatures, GADTs #-}

--fold :: (a -> a -> a) -> a -> [a] -> a
--fold f z [] = z
--fold f z [x] = x
--fold f z xs = fold f z front `f` fold f z back
--    where half_len = length xs `div` 2
--          front    = take half_len xs
--          back     = drop half_len xs

--merge :: [Int] -> [Int] -> [Int]
--merge (x:xs) (y:ys)
--    | x <= y    = x : merge xs (y:ys)
--    | otherwise = y : merge (x:xs) ys
--merge xs [] = xs
--merge [] ys = ys

--f1 = fold merge [] . map (\ x -> [x])

--main = print (f1 [10,9,8,7,4,2,1])

data Fails a = Failure

instance Monad Fails where
  return a = Failure
  (>>=) Failure k = Failure
--  fail msg = Nothing

instance Applicative Fails where
  pure = return
  (<*>) mf ma = mf >>= \ f -> ma >>= \ a -> return (f a)

instance Functor Fails where
  fmap f Failure = Failure

runFails :: Fails a -> a
runFails (Fails 0) = 0 

myFmap :: Monad f => (a->b) -> f a -> f b
myFmap f m = m >>= \ a -> return (f a)