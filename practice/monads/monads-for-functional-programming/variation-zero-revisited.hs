{- 2.6 Variation zero, revisited: The basic evaluator -}
data Term = Con Int | Div Term Term
    deriving (Show)
{- In general, a function of type a -> b is replaced by a function of 
type a -> M b. This can be read as a function that accepts an argument
of type a and returns a result of type b, with a possible additional
effect captured by M. -}

type M a = a

{- We need a way to turn a value into the computation that returns 
that value and does nothing else. -}
unit :: a -> M a
unit a = a

{- We need a way to apply a function of type a -> M b to a 
computation of type M a. It is convenient to write these in an order 
where the argument comes before the function. -}
bind :: M a -> (a -> M b) -> M b
bind a k = k a

{- A monad is a triple (M, unit, *), consisting of a type constructor M
and two operations of the given polymorphic types. It is further 
required that the operations satisfy the monad laws...-}

{- m*\a.n where m and n are expressions, and a is a variable. The form \a.n is a lambda expression, with the scope of the bound variable a being the expression n. The above can be read as follows: perform computation m, bind a to the resulting value, and then perform computation n. -}

eval :: Term -> M Int
eval (Con a) = unit a
eval (Div t u) = bind (eval t) (\a ->
    bind (eval u) (\b ->
        unit(quot a b)))
        
answer, errorTerm :: Term
answer = (Div (Div (Con 1972) (Con 2)) (Con 23))
errorTerm = (Div (Con 1) (Con 0))

main = do
    putStrLn (show answer)
    putStrLn (show (eval answer))
    putStrLn (show errorTerm)
    putStrLn (show (eval errorTerm))