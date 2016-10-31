{-# LANGUAGE GADTs, KindSignatures #-}

import Data.List
import Test.QuickCheck

sortInt :: [Int] -> [Int]
sortInt [] = []
sortInt xs = sort xs

inOrder :: [Int] -> Bool
inOrder []  = True
inOrder [x] = True
inOrder xs  = ( ( head xs ) <= ( head $ tail xs ) ) && ( inOrder $ tail xs )

main :: IO ()
main = do
    quickCheck prop_staticlength
    quickCheck prop_sortrevlist
    quickCheck prop_minfront
    quickCheck prop_maxback
    quickCheck prop_inorder

prop_staticlength :: [Int] -> Bool
prop_staticlength xs = length xs == length (sortInt xs)

prop_sortrevlist :: [Int] -> Bool
prop_sortrevlist xs = sortInt xs == sortInt (reverse xs)

prop_minfront :: [Int] -> Bool
prop_minfront [] = True
prop_minfront xs = minimum xs == head (sortInt xs)

prop_maxback :: [Int] -> Bool
prop_maxback [] = True
prop_maxback xs = maximum xs == last (sortInt xs)

prop_inorder :: [Int] -> Bool
prop_inorder xs = inOrder ( sortInt xs )

{- The first check (prop_staticlength) double checks that sorting doesn't change the list length.
   The second check (prop_sortrevlist) checks that sorting a list and sorting the reverse of a list will give the same output.
   The third check (prop_minfront) makes sure that the minimum element in the list always ends up in the first position of the sorted list.
   The fourth check (prop_maxback) makes sure that the maximum valued element in the list is always at the end of the sorted list.
   The last check (prop_inorder) runs a function that recursively checks that earlier values are always less than or equal to later values in the list.
   Alltogether, I think these are a fairly comprehensive set of checks that we've managed to sort the list into ascending order.-}


