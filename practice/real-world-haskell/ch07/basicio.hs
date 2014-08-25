main = do
    putStrLn "Greetings! What is your name?"
    inpStr <- getLine
    putStrLn $ "Welcome to Haskell, " ++ inpStr ++ "."
    
{-
putStrLn :: String -> IO ()
getLine :: IO String
() :: ()

Anything that is type IO something is an I/O action

() is an empty tuple, pronounce "unit". This indicates that there is no return value (for putStrLn)
() is similar to void in Java or C.

Actions:
  * have type IO t
  * Are first-class values in Haskell and fit seamlessly with the type system
  * Produce an effect when performed, but no when evaluated. This is, they produce an effect only when called by something else in an I/O context.
  *Any expression my produce an action as its value, but the action will not perform I/O until it is executed inside another I/O action (or it is main)
  *Performing (executing) an action of type IO t may perform I/O and will ultimately deliver a result of type t.
-}