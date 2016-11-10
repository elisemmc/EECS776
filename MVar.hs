{-# LANGUAGE GADTs, KindSignatures #-}

import Data.Char
import System.Random
import Control.Concurrent
import Control.Monad

main :: IO ()
main = do
    m1 <- newEmptyMVar
    m2 <- newEmptyMVar
    forks m1 m2
    --forever $ threadDelay(1*1000*1000)

forks m1 m2 = do    
    t1 <- forkIO ( forever $ do 
        num <- generateNum
        putMVar m1 num
        )

    t2 <- forkIO ( forever $ do
        num <- takeMVar m1
        putMVar m2 ( num, "Factorial", factorial num )
        )

    t3 <- forkIO ( forever $ do
        num <- takeMVar m1
        putMVar m2 ( num, "Fibonacci", fibonacci num )
        )

    t4 <- forkIO ( forever $ do
        num <- takeMVar m2
        threadDelay ( 1*1000*1000 )
        printMVar num
        )
    putStrLn "Threads generated"

generateNum :: IO Integer
generateNum = getStdRandom (randomR (1,25))

factorial :: Integer -> Integer
factorial 1 = 1
factorial a = a * ( factorial ( a-1 ) )

fibonacci :: Integer -> Integer
fibonacci 1 = 1
fibonacci 2 = 1
fibonacci a = fibonacci (a-1) + fibonacci (a-2)

printMVar :: ( Integer, String, Integer ) -> IO ()
printMVar (a, b, c) = do
    putStrLn ( b ++ " of " ++ (show a) ++ " is " ++ (show c) )