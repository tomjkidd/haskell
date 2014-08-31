{- 2.1 Variation zero: The basic evaluator -}

-- In simple english, a term is either a constant Con, or a quotient Div
data Term = Con Int | Div Term Term
    deriving (Show)

eval :: Term -> Int
eval (Con a) = a
eval (Div t u) = quot (eval t) (eval u)

answer, errorTerm :: Term
answer = (Div(Div (Con 1972) (Con 2)) (Con 23))
errorTerm = (Div (Con 1) (Con 0))

main = do
    putStrLn (show $ eval answer)
    putStrLn (show $ eval errorTerm)
