import Data.Char
import System.Random

generateNum :: IO Int
generateNum = getStdRandom (randomR (1,10))

playAgain :: IO ()
playAgain = do
    putStrLn "Play Again? [Y/N]"
    answer <- getLine
    if ( (answer == "Y") || (answer == "y") )
        then do
            putStrLn "Starting New Game"
            main
        else do
            putStrLn " ... Exiting ... "

guessNum :: Int -> IO ()
guessNum num = do
    putStrLn "Please guess a number"
    guess <- getLine
    if ( (read guess :: Int) == num )
        then do
            print "Correct"
            playAgain
        else
            if ( (read guess) < num )
                then do
                    putStrLn "Higher"
                    guessNum num
                else do
                    putStrLn "Lower"
                    guessNum num

main :: IO ()
main = do
    num <- generateNum
    guessNum num
    
