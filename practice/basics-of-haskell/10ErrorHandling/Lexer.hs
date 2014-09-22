module Lexer (
    Operator(..), Token(..), tokenize, lookAhead, accept
) where

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
                  
-- lookAhead will take the current head of the list
lookAhead :: [Token] -> Token
lookAhead [] = TokEnd
lookAhead (c:cs) = c

-- accept will take the current tail of the list
accept :: [Token] -> [Token]
accept [] = error "Nothing to accept"
accept (t:ts) = ts