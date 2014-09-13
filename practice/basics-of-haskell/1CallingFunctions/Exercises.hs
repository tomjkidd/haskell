--pyth takes two numbers and returns the sum of squares
pyth :: Num a => a -> a -> a
pyth a b = a^2 + b^2

--pyth' takes one tuble of two numbers and returns the sum of squares
pyth' :: Num a => (a, a) -> a
pyth' (a, b) = a^2 + b^2

main = do
    -- 1. Add parentheses to make pyth 3 * 2 pyth -1 8 compile
    putStrLn $ show $ pyth (3 * 2) (pyth (-1) 8)
    
    -- 2. Add parentheses to make pyth' 3 * 2 pyth' -1 8 compile
    putStrLn $ show $ pyth' (3 * 2, pyth' ((-1), 8))
    
    -- 3. Remove parentheses from print (sqrt (pyth 3 ((-1) - 3)))
    print $ sqrt $ pyth 3 $ (-1) - 3