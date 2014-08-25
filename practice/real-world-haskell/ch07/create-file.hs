{- This was a good simple opportunity to use some of the tools presented
in Chapter 7.

The following file asks for a filename, and creates a new empty 
filename with that name in the current directory.

Note that it is done with >>, >>= and does not use a do statement.
-}
main =  putStrLn "Please enter the desired filename" >>
        getLine >>=
        (\filename -> writeFile filename "")