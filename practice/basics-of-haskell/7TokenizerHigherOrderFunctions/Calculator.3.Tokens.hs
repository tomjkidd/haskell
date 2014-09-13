import Data.Char

-- Tokens are operators, identifiers, and numbers.
data Token = TokOp Operator
           | TokIdent String
           | TokNum Int
           | TokLParen
           | TokRParen
           | TokAssign
    deriving (Show, Eq)
data Expression

tokenize :: String -> [Token]
tokenize [] = []
tokenize (x:xs) | elem x "+-*/" = TokOp (operator x) : tokenize xs
                 | isDigit x = number x xs
                 | isAlpha x = identifier x xs
                 | isSpace x = tokenize xs
                 | x == '(' = TokLParen : tokenize xs
                 | x == ')' = TokRParen : tokenize xs
                 | x == '=' = TokAssign : tokenize xs
                 | otherwise = error $ "Cannot tokenize " ++ [x]

identifier c cs = let (str, cs') = span isAlphaNum cs in 
                  TokIdent (c:str) : tokenize cs'
number c cs = let (digs, cs') = span isDigit cs in
                  TokNum (read (c:digs)) : tokenize cs'

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

main :: IO ()
main = do
    print $ tokenize' "+-*/ 12ab "
    print $ tokenize "x2=(12 + 24) / x1"
    line <- getLine
    putStrLn line
    main