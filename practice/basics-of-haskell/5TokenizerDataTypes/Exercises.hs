-- 1. Write a function that takes an Operator and returns the appropriate character.
data Operator = Plus | Minus | Times | Div

opToChar :: Operator -> Char
opToChar o = case o of
                Plus -> '+'
                Minus -> '-'
                Times -> '*'
                Div -> '/'
                
--2. Define a data type Point with one constructor Pt that takes two Doubles for the x and y values. Write a function inc that takes a Point and returns a new Point whose coordinates are one more than the original.
data Point = Pt Double Double
    deriving Show

inc :: Point -> Point
inc (Pt x y) = Pt (x + 1) (y + 1)

-- 3. Solve the previous exercise using pairs rather than Points
inc' :: (Int, Int) -> (Int, Int)
inc' (x,y) = (x+1,y+1)

-- data List = Cons Int List | Empty
data List a = Cons a (List a) | Empty

singleton :: List a -> Bool
singleton (Cons _ Empty) = True
singleton _ = False

sumLst :: List Int -> Int
sumLst (Cons h rest) = h + sumLst rest
sumLst Empty = 0

sumLst' :: [Int] -> Int
sumLst' (h:rest) = h + sumLst' rest
sumLst' [] = 0

lst, lst0, lst1, lst2 :: List Int
lst = Cons 2 (Cons 4 (Cons 6 Empty))
lst0 = Empty
lst1 = Cons 1 lst0
lst2 = Cons 2 lst1

lst' = [2,4,6]

-- 4. Implement norm, that takes a list of Doubles and returns the square root of the sum of squares of its elements
norm :: [Double] -> Double
norm xs = sqrt (foldr (+) 0 xs)

-- 5. Implement the function decimate that skips every other element of a list
decimate :: [a] -> [a]
decimate [x] = [x]
decimate (x:y:xs) = x : decimate xs
decimate [] = []

-- 6. Implement a function that takes a pair of lists and returns a list of pairs. For instance ([1,2,3,4], [1,4,9]) should produce [(1,1),(2,4),(3,9)]. Notice that the longer of the two lists is truncated if necessary.
zipList :: ([a],[b]) -> [(a,b)]
zipList ((x:xs), (y:ys)) = (x,y) : zipList (xs, ys)
zipList ([], _) = []
zipList (_, []) = []

main = do
    print $ opToChar Plus
    print $ inc (Pt (-1) 3)
    print $ inc' (-1, 3)
    print $ singleton Empty
    print $ singleton $ Cons 2 Empty
    print $ singleton $ Cons 3 $ Cons 4 Empty
    print $ sumLst lst
    print $ sumLst Empty
    print $ sumLst' lst'
    print $ sumLst' []
    print $ norm [1.1, 2.2, 3.3]
    print $ decimate [1,2,3,4,5]
    print $ zipList ([1,2,3,4], "Hello")