{- 1. Define the WhyNot monad -}
data WhyNot a = Nah | Sure a
    deriving Show

instance Monad WhyNot where
    Sure x >>= k = k x
    Nah    >>= _ = Nah
    return x = Sure x
    fail _ = Nah
{- 
    The type of (>>=) is Monad m => m a -> (a -> m b) -> m b
    safeRoot is a function that takes a double and returns the WhyNot monad with a Double parameter.
-}

safeRoot :: Double -> WhyNot Double
safeRoot x =
    if x >= 0 then
        return (sqrt x)
    else
        fail "Boo!"

test :: Double -> WhyNot Double
test x = do
    y <- safeRoot x
    z <- safeRoot (y - 4)
    w <- safeRoot z
    return w
    
{- 2. Define a monad instance of Trace (no need to override fail).
    The idea is to create a trace of execution by sprinkling your code with calls to put. The result of executing this code should look something like this:
    
    ["fact 3","fact 2","fact 1","fact 0"]
    6
-}
newtype Trace a = Trace ([String], a)

instance Monad Trace where
    return x = Trace ([], x)
    (Trace (lst, x)) >>= k =
        let Trace (lst', y) = k x
        in Trace (lst ++ lst', y)

put :: Show a => String -> a -> Trace ()
put msg v = Trace ([msg ++ " " ++ show v], ())

fact :: Integer -> Trace Integer
fact n = do
    put "fact" n
    if n == 0
        then return 1
        else do
            m <- fact (n -1)
            return (n * m)      

main = let Trace (lst, m) = fact 3
       in do
            print $ test 9
            print $ test 400
            print lst
            print m