{- 2.3 Variation two: State -}
data Term = Con Int | Div Term Term
    deriving (Show)

type M a = State -> (a, State)
type State = Int

eval :: Term -> M Int -- M Int has type Int -> (a, Int)
eval (Con a) x = (a, x)
eval (Div t u) x = let (a, y) = eval t x in
                   let (b, z) = eval u y in
                   (quot a b, z + 1)
{- Here eval is keeping a counter, x. each eval application or the quot computation increments the counter. For each of the computations when pattern matched against the Div constructor, the state is captured and passed to the next computation (x to y to z to z + 1). 

Note that the definition has changed, where eval needs a second Int parameter in order to signify its initial state. -}

answer, errorTerm :: Term
answer = (Div(Div (Con 1972) (Con 2)) (Con 23))
errorTerm = (Div (Con 1) (Con 0))

-- eval answer 0 -- (42, 2)
-- eval errorTerm 0 -- divide by zero exception