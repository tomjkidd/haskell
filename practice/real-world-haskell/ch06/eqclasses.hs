{- Here is an example of developing a type class -}
data Color = Red | Green | Blue

class BasicEq a where
    isEqual :: a -> a -> Bool
    
instance BasicEq Bool where
    isEqual True True = True
    isEqual False False = True
    isEqual _ _ = False
    
class BasicEq2 a where
    isEqual2 :: a -> a -> Bool
    isNotEqual2 :: a -> a -> Bool
    
class BasicEq3 a where
    isEqual3 :: a -> a -> Bool
    isEqual3 x y = not (isNotEqual3 x y)
    
    isNotEqual3 :: a -> a -> Bool
    isNotEqual3 x y = not (isEqual3 x y)

instance BasicEq3 Color where
    isEqual3 Red Red = True
    isEqual3 Green Green = True
    isEqual3 Blue Blue = True
    isEqual3 _ _ = False
    