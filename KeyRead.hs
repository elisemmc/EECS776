{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Data.List (nub)
import           Data.Monoid ((<>))
import qualified Data.Text as Text
-- import           Debug.Trace
import           Graphics.Blank

data State = State
        { keys :: [Int]    -- key *codes* for pressed keys
        , step :: Int
        }
     deriving Show

main :: IO ()
main = blankCanvas 3000 { events = ["keyup","keydown"] } $ \ context -> loop context (State [] 0)

loop :: DeviceContext -> State -> IO ()
loop context state = do
        send context $ do
                clearRect (0,0,width context,height context)
                lineWidth 1
                strokeStyle "red"
                font "30pt Calibri"
                fillText("Keys currently pressed: " <> Text.pack (show (keys state)),10,50)
                fillText("Counter: " <> Text.pack (show (step state)),10,150)

        control context state

control :: DeviceContext -> State -> IO ()
control context state = do
    event <- wait context
    print event
    let down_keys = case (eType event,eWhich event) of
                     ("keydown",Just c) -> [c]
                     _ -> []
    let up_keys = case (eType event,eWhich event) of
                     ("keyup",Just c) -> [c]
                     _ -> []
    let current_keys = [ k | k <- nub (keys state ++ down_keys), not (k `elem` up_keys) ]
    let state' = state { step = step state + 1, keys = current_keys }
    loop context state'