isDot :: Char -> Bool
isDot '.' = True
isDot _   = False

f :: Maybe Char -> Maybe Bool
f (Just c) = if(isDot c)then(Just True)else(Just False)
f Nothing  = Nothing 

g :: Monad m => m Char -> m Bool
g m = fmap isDot m
--g m = pure isDot <*> m
--g m = m >>= \a->return(isDot a)
--note: all of the above do the same thing

h :: Monad m => [m a] -> m [a]
h [] = return []
h [a] = a >>= \x -> return [x]
h (m:ms) = m >>= \a -> h ms >>= \as -> return (a:as)
--parenthesized h (m:ms) = m >>= (\a -> h ms >>= (\as -> return (a:as)))
