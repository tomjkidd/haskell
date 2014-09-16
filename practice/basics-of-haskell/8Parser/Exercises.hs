import Data.Char

data Token = TokLParen | TokRParen | TokEnd
    deriving (Show, Eq)

data Tree = Node Tree Tree | Leaf
    deriving Show


lookAhead :: [Char] -> Token
lookAhead [] = TokEnd
lookAhead (c:cs) | c == '(' = TokLParen
                 | c == ')' = TokRParen
                 | otherwise = error $ "Bad input: " ++ (c:cs)
accept :: [Char] -> [Char]
accept [] = error "Nothing to accept"
accept (c:cs) = cs

root, expr, par :: [Char] -> (Tree, [Char])

{- 1. The shape of a binary tree may be encoded using matching pairs 
of parentheses, The string of parentheses obtained this way matches 
the following grammar: -}
{- 
Root <- Par
Expr <- Par Par
Par <- '(' Expr ')'
     | '(' ')'
-}
root = par

expr str = let (tree, str') = par str
               (tree', str'') = par str'
           in (Node tree tree', str'')

par str = 
    case firstChar of
        TokLParen ->
            case secondChar of
                TokRParen -> (Leaf, allButFirstTwo)
                _ -> let (tree, str') = expr allButFirst
                     in 
                        if lookAhead str' == TokRParen
                        then (tree, accept str')
                        else error $ "Unmatched right parenthesis"
        _ -> error $ "Bad expression"
    where firstChar = lookAhead str
          allButFirst = accept str
          allButFirstTwo = accept (accept str)
          secondChar = lookAhead (accept str)
        

parse str = let (tree, str') = root str
            in
                if null str'
                then tree
                else error $ "Unconsumed string " ++ str'
{- TODO: Might be able to extend the tree with an Empty constructor 
which would allow more structures than the current one.
-}
                
{- 2. Write a parser that splits a string into a list of words using 
space characters as separators (use isSpace)
-}
type Word = String

sentence :: String -> [Word]
sentence "" = []
sentence str = let (w, str') = word' str
               in w : (sentence str')

word :: String -> (Word, String)
word "" = ("", "")
word (c:cs) | isSpace c = ("", cs)
            | otherwise = let (w, cs') = word cs
                          in (c:w, cs')

word' :: String -> (Word, String)
word' str = let (w, str') = span (not . isSpace) str
                (_, str'') = span isSpace str'
            in (w, str'')

{- 3. Generalize the sentence parser from 2 to take a pluggable 
parser. Then new function is called several and takes as an argument 
a generic function String -> (a, String), which is supposed to parse 
a string and return the result of type a together with the leftover 
string. Use it to split a string into a list of numbers -}

type Parser a = String -> (a, String)

several :: Parser a -> String -> [a]
several p "" = []
several p str = let (a, str') =  p str
                in a : (several p str')

num :: Parser Int
num str = let (n, str') = span (not . isSpace) str
              (_, str'') = span isSpace str'
          in (read n, str'')               

main = do
    print $ parse "(()(()()))"
    print $ sentence "Ceci n'est pas une phrase"
    print $ several num "12 4 128"
