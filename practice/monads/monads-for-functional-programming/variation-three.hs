{- 2.4 Variation three: Output -}
data Term = Con Int | Div Term Term
    deriving (Show)

type M a = (Output, a)
type Output = String

eval :: Term -> M Int
eval (Con a) = (line(Con a) a, a)
eval (Div t u) = let (x, a) = eval t in
                   let (y, b) = eval u in
                   (x ++ y ++ line(Div t u) (quot a b), quot a b)
line :: Term -> Int -> Output
line t a = "eval(" ++ show t ++ ") <= " ++ show a ++ "\n"

{- -}

answer, errorTerm :: Term
answer = (Div(Div (Con 1972) (Con 2)) (Con 23))
errorTerm = (Div (Con 1) (Con 0))

-- eval answer -- ("eval(Con 1972) <= 1972\neval(Con 2) <= 2\neval(Div (Con 1972) (Con 2)) <= 986\neval(Con 23) <= 23\neval(Div (Div (Con 1972) (Con 2)) (Con 23)) <= 42\n",42)
-- eval errorTerm -- ("eval(Con 1) <= 1\neval(Con 0) <= 0\neval(Div (Con 1) (Con 0)) <= *** Exception: divide by zero