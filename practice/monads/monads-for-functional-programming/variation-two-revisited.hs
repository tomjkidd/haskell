{- 2.8 Variation two, revisited: State -}
data Term = Con Int | Div Term Term
    deriving (Show)

type M a = State -> (a, State)
type State = Int

unit :: a -> M a
unit a = \x -> (a, x)
--unit a s = (a, s)?

bind :: M a -> (a -> M b) -> M b
bind m k = \x -> let (a,y) = m x in
                 let (b,z) = k a y in
                 (b,z)
tick :: M ()
tick = \x -> ((), x + 1)
--tick x = ((), x + 1)?

eval :: Term -> M Int
eval (Con a) = unit a
eval (Div t u) = bind (eval t) (\a ->
    bind (eval u) (\b ->
        bind tick (\() -> unit(quot a b))))
        
answer, errorTerm :: Term
answer = (Div (Div (Con 1972) (Con 2)) (Con 23))
errorTerm = (Div (Con 1) (Con 0))

main = do
    putStrLn (show answer ++ " with initial state 0")
    putStrLn (show (eval answer 0))
    putStrLn (show errorTerm ++ " with initial state 0")
    putStrLn (show (eval errorTerm 0))
