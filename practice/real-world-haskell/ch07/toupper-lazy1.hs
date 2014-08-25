import System.IO
import Data.Char(toUpper)

main :: IO ()
main = do
       inh <- openFile "input.txt" ReadMode
       outh <- openFile "output.txt" WriteMode
       inpStr <- hGetContents inh
       let result = processData inpStr -- Because inpStr is not used after this line, lazy evaluation can free memory in order to read large files.
       hPutStr outh result
       hClose inh
       hClose outh
       
processData :: String -> String
processData = map toUpper