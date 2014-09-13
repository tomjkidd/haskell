import Data.Char

data Token = TokOp Operator
           | TokIdent String
           | TokNum Int
           | TokAssign
           | TokLParen
           | TokRParen
    deriving (Show, Eq)
data Expression

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
                 -- |isAlpha x = TokIdent [x] : tokenize xs
                 | isAlpha x = identifier x xs
                 | isSpace x = tokenize xs
                 | x == '(' = TokLParen : tokenize xs
                 | x == ')' = TokRParen : tokenize xs
                 | x == '=' = TokAssign : tokenize xs
                 | otherwise = error $ "Cannot tokenize " ++ [x]


identifier c cs = let (str, cs') = alnums cs in 
                  TokIdent (c:str) : tokenize cs'
number c cs = let (digs, cs') = nums cs in
                  TokNum (read (c:digs)) : tokenize cs'
                  
alnums :: String -> (String, String)
--alnums xs = (takeWhile isAlphaNum xs, dropWhile isAlphaNum xs)
alnums = span isAlphaNum

nums :: String -> (String, String)
nums = span isDigit

-- 1. Implement the function toInts using map
toInts :: String -> [Int]
toInts = map digitToInt

-- 2. Implement the function squares that takes a list of integers and returns the list of their squares.
squares :: [Int] -> [Int]
squares = map (\x -> x * x)

-- 3. Implement the function inCircle2 that takes a list of 2-D points and returns only those that fit inside the circle of radius 2.
type Point = (Double, Double)

inCircle2 :: [Point] -> [Point]
inCircle2 = filter (inCircle' 2.0)

inCircle' :: Double -> Point -> Bool
inCircle' r (x,y) = if sqrt ( x * x + y * y ) <= r then True else False

-- 4. Use foldl to calculate the sum of squares given a list of doubles
squares' :: [Int] -> Int
squares' = foldl (\acc x -> acc + x * x) 0

-- 5. The accumulator in foldl can also be a list. With this in mind, implement function rev that reverses a list
rev :: [a] -> [a]
rev = foldl (\acc x -> x:acc) []

-- 6. Just as a proof of concept, implement a version of alnums using foldl, even though it's going to be awkward and inefficient.
notQuiteAlnums = foldl (\(t, d) x -> if isAlphaNum x
                                  then (t++[x], d)
                                  else (t, d++[x])) ([],[])
                                  
-- This only will split the array into two arrays. Need to capture stop condition.
alnums'' :: String -> (String, String, Bool)
alnums'' = foldl (\(t, d, ignore) x -> case ignore of
                        False -> if isAlphaNum x
                                 then (t++[x], d, False)
                                 else (t, d++[x], True)
                        True -> (t, d++[x], True))
                ([],[],False)
alnums' :: String -> (String, String)
alnums' str = let (als, rest, _) = alnums'' str in (als, rest) 

-- 7. Extend the tokenizer above to recognize more tokens: LParen and RParen corresponding to ( and ); as well as TokAssign for =

main = do
    print $ toInts "2013"
    print $ squares [1..10]
    print $ inCircle2 [(0, 0), (2, -2), (1, -1), (1.9, 0.1), (10, 1)]
    print $ alnums "R2D2+C3Po"
    print $ squares' [3,4,5]
    print $ rev "spot on"
    print $ notQuiteAlnums "R2D2+C3Po"
    print $ alnums' "R2D2+C3Po"
    print $ alnums' "a14"
    print $ tokenize "12 + 24 / x1"
    print $ tokenize "x2=(12 + 24) / x1"