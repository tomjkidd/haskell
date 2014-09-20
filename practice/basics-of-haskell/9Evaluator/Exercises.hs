{- 1. Implement function translate, which takes a dictionary and a 
list of strings and returns a list of translated strings. If a string 
is not in a dictionary, it should be replaced with "whatchamacallit". 
For bonus points, try using the higher order map function from the 
Prelude, and the where clause. Remember that a function defined 
inside where has access to the arguments of the outer function.
-}

import qualified Data.Map as M
import Data.Char (toLower)
import Data.List (sortBy)

type Dict = M.Map String String

translate :: Dict -> [String] -> [String]
translate dict strs = map (unwrap dict) strs
    where
        unwrap :: Dict -> String -> String
        unwrap dict str = case M.lookup str dict of
                            Just a -> a
                            Nothing -> "whatchamacallit"

testTranslation :: Dict -> IO ()
testTranslation dict = do
    print $ translate dict ["where", "is", "the", "colosseum"]
    
testInsertion :: Dict -> IO Dict
testInsertion dict = do
    return $ M.insert "colosseum" "colosseo" dict
{- 2. Implement a function paren that takes an expression tree and 
turns it into a string with fully parenthesized expression. For 
instance, when acting on testExpr it should produce the string
(x = ((2.0 * (y = 5.0)) + 3.0)) 
-}
data Operator = Plus | Minus | Times | Div
    deriving (Show, Eq)

data Tree = SumNode Operator Tree Tree
          | ProdNode Operator Tree Tree
          | AssignNode String Tree
          | UnaryNode Operator Tree
          | NumNode Double
          | VarNode String
    deriving Show
paren :: Tree -> String
paren (SumNode op left right) = 
    case op of
        Plus -> display "+" left right
        Minus -> display "-" left right
paren (ProdNode op left right) =
    case op of
        Times -> display "*" left right
        Div -> display "/" left right
paren (AssignNode var exp) = let tree = paren exp
                             in "(" ++ var ++ " = " ++ tree ++ ")"
paren (UnaryNode op exp) = let tree = paren exp
                               opS = case op of
                                        Plus -> " +"
                                        Minus -> " -"
                            in "(" ++ opS ++ tree ++ ")"
paren (NumNode x) = show x
paren (VarNode var) = var

display :: String -> Tree -> Tree -> String
display op left right =
    let l = paren left
        r = paren right
    in "(" ++ l ++ " " ++ op ++ " " ++ r ++ ")"

-- x = 2 * (y = 5) + 3
testExpr = AssignNode "x" (SumNode Plus
                              (ProdNode Times
                                  (NumNode 2.0)
                                  (AssignNode "y" (NumNode 5)))
                              (NumNode 3))
                              
{- 3. The code below creates a frequency map of words in a given 
text. Fill in the implementation of indexWords, which counts the 
frequency of each word in a list of words; and splitWords, which 
splits a string into (lowercased) words, removing punctuation and 
newlines in the process. You might want to use the function 
findWithDefault from Data.Map and the function words from the 
Prelude. For bonus points try using map and foldl.
-}

type Index = M.Map String Int -- Index is a Map with String keys and Int values

indexWords :: Index -> [String] -> Index
indexWords dict [] = dict
indexWords dict (str:strs) = let result = M.lookup str dict
                                 dict' = case result of
                                            Nothing -> M.insert str 1 dict
                                            Just a -> M.insert str (a + 1) dict
                             in indexWords dict' (strs)

splitWords :: String -> [String]
splitWords str = words (map (\c -> if c `elem` ".,-_?'" 
                                   then ' ' 
                                   else toLower c) str)

mostFrequent :: [String] -> [(String, Int)]
mostFrequent wrds =
    let index = indexWords M.empty wrds
    in take 9 (sortBy cmpFreq (M.toList index))
  where
    cmpFreq :: (String, Int) -> (String, Int) -> Ordering
    cmpFreq (w1, n1) (w2, n2) = compare n2 n1

main = 
    let dict = M.fromList [("where","dove"), ("is","e"), ("the","il")]
    in do
        testTranslation dict
        dict' <- testInsertion dict
        testTranslation dict'
        putStrLn "The original dictionary is unchanged:"
        testTranslation dict
        print $ paren testExpr
        text <- readFile "moby.txt"
        print $ mostFrequent (splitWords text)