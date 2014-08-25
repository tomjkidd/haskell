import System.IO

main :: IO ()
main = do
       inpStr <- readFile "input.txt"
       writeFile "output.txt" (map toUpper inpStr)