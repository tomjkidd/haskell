{- 2.2 Variation one: Exceptions -}
data Term = Con Int | Div Term Term
    deriving (Show)

data M a = Raise Exception | Return a
    deriving (Show)
type Exception = String

-- A value of type M a either has the form Raise e, where e is an exception string, or Return a, where a is a value of type a. This is similar to the Either datatype encountered in Real World Haskell.

-- Raise and Return are the valid type constructors provided for the data type.

eval :: Term -> M Int
eval (Con a) = Return a
eval (Div t u) = case eval t of
                   Raise e -> Raise e
                   Return a ->
                     case eval u of
                       Raise e -> Raise e
                       Return b ->
                         if b == 0
                           then Raise "divide by zero"
                           else Return (quot a b)

answer, errorTerm :: Term
answer = (Div(Div (Con 1972) (Con 2)) (Con 23))
errorTerm = (Div (Con 1) (Con 0))

-- eval answer -- Return 42
-- eval errorTerm -- Raise "divide by zero"