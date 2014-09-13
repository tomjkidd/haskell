-- 1. Define a function putStrLn' using putStr and putChar to output the newline
putStrLn' :: String -> IO ()
putStrLn' str = do
    putStr str
    putChar '\n'
    
-- 2. Define a function putQStrLn that outputs a string surrounded by quotes
putQStrLn :: String -> IO ()
putQStrLn str = do
    putChar '"'
    putStr str
    putChar '"'
    putChar '\n'
    
main = do
    putStrLn' "First line"
    putStrLn' "Second line"
    
    putQStrLn "You can quote me."
    
    putStrLn "Enter a quotable line"
    str <- getLine
    putQStrLn str