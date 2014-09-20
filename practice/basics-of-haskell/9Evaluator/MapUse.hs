import qualified Data.Map as M

main = do
    let dict = M.fromList [("a",1),("b",2)]
    print $ (M.lookup "a" dict) -- Just 1
    print $ (M.lookup "z" dict) -- Nothing
    print $ (M.findWithDefault 0 "z" dict) -- 0