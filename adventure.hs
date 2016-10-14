data Dir = N | S
    deriving (Show, Read, Eq, Ord)

data Room = Room
    { desc :: String
    , dead :: Bool
    , dirs :: [(Dir,Room)]
    }

instance Show Room where
    show (Room loc _ w) = "You are at: " ++ loc ++ " and you can go " ++ show (map fst w)

worldMap :: [Room]
worldMap = [ drawBridge, castle ]

drawBridge = Room { desc = "draw bridge", death = False, dirs = [(N,castle)] }
castle = Room { desc = "castle", death = True, dirs = [(S, drawBridge)] }

data Cmd = GO Dir
    deriving (Show)

data State = InRoom Room | Dead

instance Show State where
    show (InRoom room) = show room
    show (Dead)        = "you are dead"

main :: IO()
main = display (InRoom drawBridge)

display :: State -> IO ()
display st = do
    putStrLn (show st)
    adventure st

adventure :: State -> IO ()
adventure st = do
    str <- getLine
    case parse str of
        Nothing -> do
            putStrLn "I'm sorry, I do not understand"
            adventure st
        Just cmd -> case eval cmd st of
            Left msg -> do
                putStrLn ("Eval failure : " ++ msg)
                adventure st
            Right st' -> do
                display st'
    return ()

eval :: Cmd -> State -> Either String State
eval (GO d) (InRoom (Room _ dirs)) = case lookup d dirs of
    Nothing      -> Left "I'm sorry!"
    Just newRoom -> Right newRoom

parse :: String -> Maybe Cmd
parse "N" = return (GO N)
parse "S" = return (GO S)
parse _   = fail "!"