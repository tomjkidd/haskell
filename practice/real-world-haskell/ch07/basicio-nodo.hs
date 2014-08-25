main =
    putStrLn "Greetings! What is your name?" >> -- do nothing with return...
    getLine >>=
    (\inpStr -> putStrLn $ "Welcome to Haskell, " ++ inpStr ++ ".") >>
    putStrLn "This was just chained on using >>="