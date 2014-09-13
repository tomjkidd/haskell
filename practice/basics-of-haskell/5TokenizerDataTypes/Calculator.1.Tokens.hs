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


-- Tokenizer
opToStr :: Operator -> Char
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
    line <- getLine
    putStrLn line
    main