{- 2.9 Variation three, revisited: Output -}
data Term = Con Int | Div Term Term
    deriving (Show)

type M a = (Output, a)
type Output = String

unit :: a -> M a
unit a = ("", a)

bind :: M a -> (a -> M b) -> M b
bind m k = let (x, a) = m in
           let (y, b) = k a in
           (x++y, b)

out :: Output -> M ()
out x = (x, ())

line :: Term -> Int -> Output
line t a = "eval(" ++ show t ++ ") <= " ++ show a ++ "\n"

eval :: Term -> M Int
eval (Con a) = bind (out(line(Con a) a)) (\() -> unit a)
eval (Div t u) = bind (eval t) (\a ->
    bind (eval u) (\b ->
        bind (out(line (Div t u) (quot a b))) (\() -> unit(quot a b))))
        
answer, errorTerm :: Term
answer = (Div (Div (Con 1972) (Con 2)) (Con 23))
errorTerm = (Div (Con 1) (Con 0))

main = do
    putStrLn (show answer)
    putStrLn (show (eval answer))
    putStrLn (show errorTerm)
    putStrLn (show (eval errorTerm))
