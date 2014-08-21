-- file: ch04/InteractWith.hs
import System.Environment (getArgs)

interactWith f inputFile outputFile = do
    input <- readFile inputFile
    writeFile outputFile (f input)

main = mainWith myFunction
    where mainWith f = do
            args <- getArgs
            case args of
                [input,output] -> interactWith f input output
                _ -> putStrLn "error: exactly two arguments needed"
          myFunction = id