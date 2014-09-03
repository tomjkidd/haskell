improve :: Fractional a => a -> a -> a
improve x y = (y + x/y)/2

--satis :: Num -> Num -> Bool
satis x y = abs (y^2 - x) < eps
    where eps = 0.0001

--tkUntil :: (a -> Bool) -> (a -> a) -> a -> a
until' p f x | p x == True = x
             | otherwise = until' p f (f x)

sqrt' x = until' (satis x) (improve x) x

-- y - f(y)/f'(y)
-- f(x) = x^2 - a, f'(x) = 2x
deriv f x = (f (x + dx) - (f x))/dx
    where dx = 0.0001
    
newton f = until' satis improve
    where satis y = abs(f y) < eps
          improve y = y - (f y)/(deriv f y)
          eps = 0.0001

sqrt'' x = newton f x
    where f y = y^2 - x
    
cubrt x = newton f x
    where f y = y^3 - x