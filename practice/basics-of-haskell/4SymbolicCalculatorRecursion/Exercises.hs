-- 1. Print squares of numbers from 1 to 10 (using conditional)
loop :: Int -> IO ()
loop n = if n > 10 then return () else do
    putStrLn (show (n*n))
    loop (n + 1)
    
-- 2. Create a definition for factorial
fact :: Int -> Int
fact n = if n < 1 then 1 else n * fact (n-1)

-- 3. Factorial is incorrect after n = 21 because Int overflows. Implement a version that uses infinite precision Integer instead
fact' :: Integer -> Integer
fact' n = if n < 1 then 1 else n * fact' (n-1)

-- 4. Define a Fibonacci number generator
fib :: Int -> Int
fib n = if n > 2 then fib (n-1) + fib (n-2) else 1

main :: IO ()
main = do
    loop 1
    print $ fact 5
    print $ fact 23
    print $ fact' 23
    print $ fib 20