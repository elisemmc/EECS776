{-# LANGUAGE OverloadedStrings #-}
module Main where

--http://localhost:3000/
import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad
import Data.List
import Data.Monoid ((<>))
import qualified Data.Text as Text
-- import           Debug.Trace
import Graphics.Blank
import System.Random

type Rock = (Shift, Double)

data Shift = L | M | R
    deriving Eq

data State = State
    { count :: Int
    , rocks :: [Rock]
    , ship  :: Shift
    , speed :: Int
    }
--     deriving Show

main :: IO ()
main = blankCanvas 3000 { events = ["keyup","keydown"] } $ \ context -> initialize context (State 0 [] M 50)

initialize :: DeviceContext -> State -> IO ()
initialize context state = do
    keysV <- newTVarIO []

    forkIO $ forever $ do
        event <- wait context
        case (eType event,eWhich event) of
            ("keydown",Just c) -> atomically $ do
                modifyTVar keysV ( nub . insert c )
            ("keyup",Just c) -> atomically $ do
                modifyTVar keysV ( delete c )
            _ -> return ()

    loop context state keysV

loop :: Foldable t => DeviceContext -> State -> TVar (t Int) -> IO ()
loop context state keysV = do
    send context $ do
        clearCanvas
        drawLanes context

        let mid = width context / 2
        drawShip ( shiftPosition mid (ship state) , height context - 50)

        sequence_ $ map (drawRock mid) ( rocks state )

        lineWidth 1
        fillStyle "black"
        font "30pt Calibri"
        beginPath()
        fillText("Score: " <> Text.pack (show (count state)), width context / 2 + 150,80)

    threadDelay ( (speed state) * 1000 )
    control context state keysV

control :: Foldable t => DeviceContext -> State -> TVar (t Int) -> IO ()
control context state keysV = do
    rand <- getStdRandom (randomR (0,5))

    let isPressed :: Int -> IO Bool
        isPressed n = do
            v <- readTVarIO keysV
            return $ n `elem` v

    let spacing = 30

    let newRocks = if ( ( (count state) `mod` spacing ) == 0) 
        then (rockFormation rand) 
        else []

    let count' = (count state) + 1

    let rocks' = ( map moveRock ( filter (filterRock context) (rocks state) ) ) ++ newRocks

    let speed' = if ( ( ( (count state) `mod` 100 ) == 0 ) && (speed state) >= 5 )
        then (speed state) - 1
        else (speed state)

    right <- isPressed 39
    left <- isPressed 37
    up <- isPressed 40
    down <- isPressed 38
    let middle = up || down

    let ship' = nextPos (ship state) left middle right

    let alive = ( length ( filter ( checkHit (height context - 100) (ship state) ) (rocks state) ) == 0 )

    let state' = state { count = count', rocks = rocks', speed = speed', ship = ship' }

    if alive
        then loop context state' keysV
        else dead context state' keysV

dead :: Foldable t => DeviceContext -> State -> TVar (t Int) -> IO ()
dead context state keysV = do
    send context $ do 
        youDied context
    threadDelay (500 * 1000)
    v <- readTVarIO keysV

    if ( length v == 0 )
        then dead context state keysV
        else loop context (State 0 [] M 50) keysV

youDied :: DeviceContext -> Canvas ()
youDied context = do
    lineWidth 1
    fillStyle "red"

    beginPath()
    font "50pt Ariel"
    fillText("YOU DIED", width context / 2 - 160, height context / 2 - 150)

    beginPath()
    font "20pt Calibri"
    fillText("Press any key to play again", width context / 2 - 144, height context / 2 - 100)

checkHit :: Double -> Shift -> Rock -> Bool
checkHit limit s (r, y) = if (y > limit) then (s == r) else False 

drawLanes :: DeviceContext -> Canvas ()
drawLanes context = do
    let mid = ( width context ) / 2

    beginPath()
    
    moveTo( mid+130, height context - 50 )
    lineTo( mid+130, 50 )

    moveTo( mid-130, height context - 50 )
    lineTo( mid-130, 50 )

    lineWidth 20
    globalAlpha 1
    strokeStyle "black"

    stroke()

    beginPath()

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
    strokeStyle "maroon"
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

    beginPath()

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
filterRock context (_,y) = (y < (height context - 50) )

moveRock :: Rock -> Rock
moveRock (x,y) = (x,y+10)

nextPos :: Shift -> Bool -> Bool -> Bool -> Shift
nextPos p False False False = p
nextPos L True False False = L
nextPos M True False False = L
nextPos M False False True = R
nextPos R False False True = R
nextPos _ _ _ _ = M

rockFormation :: Int -> [Rock]
rockFormation 0 = [(L,70)]
rockFormation 1 = [(M,70)]
rockFormation 2 = [(R,70)]
rockFormation 3 = [(L,70),(M,70)]
rockFormation 4 = [(L,70),(R,70)]
rockFormation 5 = [(R,70),(M,70)]

shiftPosition :: Double -> Shift -> Double
shiftPosition mid L = mid - 85
shiftPosition mid M = mid
shiftPosition mid R = mid + 85

