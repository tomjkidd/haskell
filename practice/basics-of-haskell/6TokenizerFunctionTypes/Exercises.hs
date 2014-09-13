import Data.Char

data Token = Digit | Alpha
    deriving (Show, Eq)
    
tokenize :: String -> [Token]
tokenize (c: rest) = if isDigit c
                     then Digit : tokenize rest
                     else Alpha : tokenize rest
tokenize [] = []

-- 1. Rewrite Fibonacci numbers function using guards
fib n | n == 1 = 1
      | n == 2 = 1
      | otherwise = fib (n-1) + fib (n-2)
      
-- 2. Implement function cat that concatenates two lists
cat :: [a] -> [a] -> [a]
cat [] ys = ys
cat (x:xs) ys = x : cat xs ys

-- 3. Use cat and currying to define a function pig that prepends "pig" to any string
pig :: String -> String
pig x = cat "pig" x

-- 4. Implement function toInts that takes a number in the form of a string and returns a list of its digits as integers
toInts :: String -> [Int]
toInts [] = []
toInts (x:xs) = digitToInt x : toInts xs

-- 5. Implement function sumDig that takes a number in the form of a string and calculates the sum of its digits.
sumDig :: String -> Int
sumDig str = foldr (+) 0 (toInts str)

main = do
    print $ tokenize "passwd123"
    print $ fib 20
    putStrLn $ cat "Hello " "World!"
    putStrLn $ pig "sty"
    print $ toInts "2013"
    print $ sumDig "30750"
    