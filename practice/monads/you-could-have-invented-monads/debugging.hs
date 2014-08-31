{- Base functions -}
f :: Float -> Float
f x = x + 1

g :: Float -> Float
g x = x - 1

{- Debugging functions -}
f', g' :: Float -> (Float, String)
f' x = (f x, "f called")
g' x = (g x, "g called")

{- Here, the author reversed the arguments that are usually in the standard definition -}
bind :: (Float -> (Float,String)) -> ((Float, String) -> (Float, String))
bind f' (gx, gs) = let (fx, fs) = f' gx in (fx, gs++"\n"++fs)

unit :: Float -> (Float,String)
unit x = (x, "")

lift :: (Float -> Float) -> Float -> (Float,String)
lift f x = (unit.f) x

main = do
    putStrLn "lift (f.g) 42"
    putStrLn (show composition)
    
    putStrLn "bind (lift f) (lift g 42)"
    putStrLn (show bound)
    
    where composition = lift (f.g) 42
          bound = bind (lift f) (lift g 42)