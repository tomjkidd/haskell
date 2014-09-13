import Data.Char

-- Tokens are operators, identifiers, and numbers.
data Token = TokOp Operator
           | TokIdent String
           | TokNum Int
    deriving (Show, Eq)
data Expression

tokenize :: String -> [Token]
tokenize = undefined

parse :: [Token] -> Expression
parse = undefined

evaluate :: Expression -> Double
evaluate = undefined

data Operator = Plus | Minus | Times | Div
    deriving (Show, Eq)

operator :: Char -> Operator
operator c | c == '+' = Plus
           | c == '-' = Minus
           | c == '*' = Times
           | c == '/' = Div
    
-- Tokenizer
opToStr :: Operator -> String
opToStr o = case o of
                Plus -> "+"
                Minus -> "-"
                Times -> "*"
                Div -> "/"
                
showContent :: Token -> String
showContent (TokOp op) = opToStr op
showContent (TokIdent str) = str
showContent (TokNum i) = show i

tokenize' :: String -> [Token]
tokenize' [] = []
tokenize' (x:xs) | elem x "+-*/" = TokOp (operator x) : tokenize' xs
                 | isDigit x = TokNum (digitToInt x) : tokenize' xs
                 | isAlpha x = TokIdent [x] : tokenize' xs
                 | isSpace x = tokenize' xs
                 | otherwise = error $ "Cannot tokenize " ++ [x]

main :: IO ()
main = do
    print $ tokenize' "+-*/ 12ab "
    line <- getLine
    putStrLn line
    main