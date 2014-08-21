-- file: ch04/InteractWith.hs
import System.Environment (getArgs)

splitLines :: String -> [String]
splitLines [] = []
splitLines ls =
    let (pre, suf) = break isLineTerminator ls
    in pre : case suf of
                ('\r':'\n':rest) -> splitLines rest
                ('\r':rest) -> splitLines rest
                ('\n':rest) -> splitLines rest
                _ -> []
isLineTerminator c = c == '\r' || c == '\n'

fixLines :: String -> String
fixLines = (unlines . splitLines)

interactWith f inputFile outputFile = do
    input <- readFile inputFile
    writeFile outputFile (f input)

main = mainWith myFunction
    where mainWith f = do
            args <- getArgs
            case args of
                [input,output] -> interactWith f input output
                _ -> putStrLn "error: exactly two arguments needed"
            
          myFunction = fixLines