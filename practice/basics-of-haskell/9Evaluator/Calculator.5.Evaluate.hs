import Data.Char

-- Tokens are operators, identifiers, and numbers.
data Token = TokOp Operator
           | TokIdent String
           | TokNum Double
           | TokLParen
           | TokRParen
           | TokAssign
           | TokEnd
    deriving (Show, Eq)
    
data Operator = Plus | Minus | Times | Div
    deriving (Show, Eq)

-- Tree is used as the basis for expression trees
-- NOTE: Expression was removed and Tree is used to represent an Expression
data Tree = SumNode Operator Tree Tree
          | ProdNode Operator Tree Tree
          | AssignNode String Tree
          | UnaryNode Operator Tree
          | NumNode Double
          | VarNode String
    deriving Show

operator :: Char -> Operator
operator c | c == '+' = Plus
           | c == '-' = Minus
           | c == '*' = Times
           | c == '/' = Div

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

parse :: [Token] -> Tree
parse toks = let (tree, toks') = expression toks
             in
                if null toks'
                then tree
                else error $ "Leftover tokens: " ++ show toks'

evaluate :: Tree -> Double
evaluate (NumNode x) = x
evaluate (SumNode op left right) =
    let lft = evaluate left
        rgt = evaluate right
    in
        case op of
            Plus -> lft + rgt
            Minus -> lft - rgt
evaluate (ProdNode op left right) =
    let lft = evaluate left
        rgt = evaluate right
    in
        case op of
            Times -> lft * rgt
            Div -> lft / rgt
evaluate (UnaryNode op tree) =
    let x = evaluate tree
    in case op of
        Plus -> x
        Minus -> -x
evaluate (AssignNode str tree) = evaluate tree
evaluate (VarNode str) = 0
        
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

expression :: [Token] -> (Tree, [Token])
expression toks = 
    let (termTree, toks') = term toks
    in
        case lookAhead toks' of
            (TokOp op) | elem op [Plus, Minus] ->
                let (exTree, toks'') = expression (accept toks')
                in (SumNode op termTree exTree, toks'')
            TokAssign ->
                case termTree of
                    VarNode str ->
                        let (exTree, toks'') = expression (accept toks')
                        in (AssignNode str exTree, toks'')
                    _ -> error "Only variables can be assigned to"
            _ -> (termTree, toks')

term :: [Token] -> (Tree, [Token])
term toks =
    let (facTree, toks') = factor toks
    in
        case lookAhead toks' of
            (TokOp op) | elem op [Times, Div] ->
                let (termTree, toks'') = term (accept toks')
                in (ProdNode op facTree termTree, toks'')
            _ -> (facTree, toks')

factor :: [Token] -> (Tree, [Token])
factor toks =
    case lookAhead toks of
        (TokNum x) -> (NumNode x, accept toks)
        (TokIdent str) -> (VarNode str, accept toks)
        (TokOp op) | elem op [Plus, Minus] ->
            let (facTree, toks') = factor (accept toks)
            in (UnaryNode op facTree, toks')
        TokLParen ->
            let (expTree, toks') = expression (accept toks)
            in
                if lookAhead toks' /= TokRParen
                then error "Missing right parenthesis"
                else (expTree, accept toks')
        _ -> error $ "Parse error on token: " ++ show toks

-- lookAhead will take the current head of the list
lookAhead :: [Token] -> Token
lookAhead [] = TokEnd
lookAhead (c:cs) = c

-- accept will take the current tail of the list
accept :: [Token] -> [Token]
accept [] = error "Nothing to accept"
accept (t:ts) = ts

main = (print . evaluate . parse . tokenize) "x1 = -15 / (2 + x2)"