data Term = Con Int | Div Term Term
    deriving (Show)
    
data M a = Raise Exception | Return a
    deriving (Show)
type Exception = String

unit :: a -> M a
unit a = Return a
{- The call unit a simply returns the value a -}

bind :: M a -> (a -> M b) -> M b
bind m k = case m of
             Raise e -> Raise e
             Return a -> k a
{- The call bind m k examines the result of the computation m: if it 
is an exception, it is re-raised, otherwise the function k is applied 
to the value returned. Just as bind in the identity monad is function 
application, bind in the exception monad may be considered as a form 
of strict function application. -}

raise :: Exception -> M a
raise e = Raise e
{- The call raise e raises the exception e -}

eval :: Term -> M Int
eval (Con a) = unit a
eval (Div t u) = bind (eval t) (\a ->
    bind (eval u) (\b ->
        if b == 0
          then raise "divide by zero"
          else unit(quot a b)
    ))
{- We just check if b is zero before evaluating the result. In the 
case that it is zero, the exception is raised. -}
        
answer, errorTerm :: Term
answer = (Div (Div (Con 1972) (Con 2)) (Con 23))
errorTerm = (Div (Con 1) (Con 0))

main = do
    putStrLn (show answer)
    putStrLn (show (eval answer))
    putStrLn (show errorTerm)
    putStrLn (show (eval errorTerm))