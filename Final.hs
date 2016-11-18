{-# LANGUAGE OverloadedStrings #-}
module Main where

--http://localhost:3000/
import Control.Concurrent
import Data.List (nub)
--import Data.Monoid ((<>))
--import qualified Data.Text as Text
-- import           Debug.Trace
import Graphics.Blank

type Rock = (Shift, Double)

data Shift = L | M | R

data State = State
    { rocks :: [Rock]
    , count :: Int
    , ship  :: Shift
    }
--     deriving Show

main :: IO ()
main = blankCanvas 3000 { events = ["keyup","keydown"] } $ \ context -> loop context (State [(M,70), (R, 70), (L,280)] 0 M)

loop :: DeviceContext -> State -> IO ()
loop context state = do
    send context $ do
        clearCanvas
        drawLanes context

        let mid = width context / 2
        drawShip ( shiftPosition mid (ship state) , height context - 50)

        sequence_ $ map (drawRock mid) ( rocks state )

    threadDelay (50 * 1000)

    let newRocks = filter (filterRock context) (rocks state)
    let state' = state { count = count state + 1, rocks = map moveRock newRocks}

    loop context state'
    --control context state

--control :: DeviceContext -> State -> IO ()
--control context state = do
----    event <- wait context
----    print event
--    let down_keys = case (eType event,eWhich event) of
--                     ("keydown",Just c) -> [c]
--                     _ -> []
--    let up_keys = case (eType event,eWhich event) of
--                     ("keyup",Just c) -> [c]
--                     _ -> []
--    let current_keys = [ k | k <- nub (keys state ++ down_keys), not (k `elem` up_keys) ]

--    let state' = state { count = count state + 1, rocks = map moveRock (rocks state)}
--    loop context state'
                

drawLanes :: DeviceContext -> Canvas ()
drawLanes context = do
    let mid = ( width context ) / 2

    moveTo( mid+130, height context - 50 )
    lineTo( mid+130, 50 )

    moveTo( mid-130, height context - 50 )
    lineTo( mid-130, 50 )

    lineWidth 20
    globalAlpha 1
    strokeStyle "black"

    stroke()

    moveTo( mid+40, height context - 50 )
    lineTo( mid+40, 50 )

    moveTo( mid-40, height context - 50 )
    lineTo( mid-40, 50 )

    lineWidth 10
    globalAlpha 0.3

    stroke()

drawRock :: Double -> Rock -> Canvas ()
drawRock mid (x,y) = do
    globalAlpha 1
    lineWidth 10
    fillStyle "red"

    beginPath()
    arc(shiftPosition mid x, y, 25, 0, pi*2, False)
    closePath()

    fill()
    stroke()

drawShip :: (Double, Double) -> Canvas ()
drawShip (x,y) = do
    lineWidth 2
    globalAlpha 1
    strokeStyle "black"

    moveTo(x-10,y)
    lineTo(x+10,y)
    lineTo(x+5,y-5)
    lineTo(x-5,y-5)
    lineTo(x-10,y)

    fillStyle "black"
    fill()

    beginPath()

    moveTo(x-5,y-5)
    lineTo(x-10,y-10)
    lineTo(x-10,y-40)
    lineTo(x,y-60)
    lineTo(x+10,y-40)
    lineTo(x+10,y-10)
    lineTo(x+5,y-5)

    moveTo(x+10,y-40)
    lineTo(x+30,y-10)
    lineTo(x+10,y-10)

    moveTo(x-10,y-40)
    lineTo(x-30,y-10)
    lineTo(x-10,y-10)

    closePath()
    
    fillStyle "cyan"
    fill()
    stroke()

    beginPath()
    arc(x, y-40, 6, 0, pi*2, False)
    closePath()

    fillStyle "black"
    globalAlpha 1
    stroke()
    globalAlpha 0.5
    fill()

filterRock :: DeviceContext -> Rock -> Bool
filterRock context (_,y) = if (y < (height context - 50) ) then True else False

moveRock :: Rock -> Rock
moveRock (x,y) = (x,y+10)

shiftPosition :: Double -> Shift -> Double
shiftPosition mid L = mid - 85
shiftPosition mid M = mid
shiftPosition mid R = mid + 85

