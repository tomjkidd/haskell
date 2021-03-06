import Data.List (foldl')

tkLength :: [a] -> Int
tkLength [] = 0
tkLength (x:xs) = 1 + tkLength xs

tkNull :: [a] -> Bool
tkNull [] = True
tkNull _ = False

tkHead :: [a] -> a
tkHead [] = error "No element for head"
tkHead (x:xs) = x

tkTail :: [a] -> [a]
tkTail [] = []
tkTail (x:xs) = xs

tkLast :: [a] -> a
tkLast [] = error "No element for last"
tkLast [x] = x
tkLast (x:xs) = tkLast xs

tkInit :: [a] -> [a]
tkInit [x] = []
tkInit (x:xs) = x: tkInit xs

tkAppend :: [a] -> [a] -> [a]
tkAppend [] ys = ys
tkAppend (x:xs) ys = x : tkAppend xs ys

tkConcat :: [[a]] -> [a]
tkConcat [] = []
tkConcat ((xs):ys) = tkAppend xs (concat ys)

tkReverse :: [a] -> [a]
tkReverse [] = []
--tkReverse xs = foldl (flip (:)) [] xs
tkReverse xs = foldl (\x y -> y:x) [] xs

tkAnd :: [Bool] -> Bool
tkAnd [x] = x
tkAnd (x:xs) = x && tkAnd xs

tkOr :: [Bool] -> Bool
tkOr [x] = x
tkOr (x:xs) = x || tkOr xs

tkTake :: Int -> [a] -> [a]
tkTake _ [] = []
tkTake 0 _ = []
tkTake n (x:xs) = x : tkTake (n-1) xs

tkDrop :: Int -> [a] -> [a]
tkDrop _ [] = []
tkDrop 0 xs = xs
tkDrop n (x:xs) = tkDrop (n-1) xs

tkSplitAt :: Int -> [a] -> ([a], [a])
tkSplitAt n xs = (tkTake n xs, tkDrop n xs)

tkTakeWhile :: (a -> Bool) -> [a] -> [a]
tkTakeWhile _ [] = []
tkTakeWhile f (x:xs) | stop = []
                     | otherwise = x : tkTakeWhile f xs
                where stop = (not.f) x
                
tkDropWhile :: (a -> Bool) -> [a] -> [a]
tkDropWhile _ [] = []
tkDropWhile f (x:xs) | keepDropping = tkDropWhile f xs
                     | otherwise = x:xs
                where keepDropping = f x

tkSpan :: (a -> Bool) -> [a] -> ([a],[a])
tkSpan f xs = (tkTakeWhile f xs, tkDropWhile f xs)

tkBreak :: (a -> Bool) -> [a] -> ([a],[a])
tkBreak f xs = tkSpan (not.f) xs

tkElem :: Eq a => a -> [a] -> Bool
tkElem x [] = False
tkElem x (y:ys) | y == x = True
                | otherwise = tkElem x ys

tkFilter :: (a -> Bool) -> [a] -> [a]
tkFilter _ [] = []
tkFilter f (x:xs) | f x == True = x : tkFilter f xs
                  | otherwise = tkFilter f xs
                  
tkIsPrefixOf :: (Eq a) => [a] -> [a] -> Bool
tkIsPrefixOf [] _ = True
tkIsPrefixOf _ [] = False
tkIsPrefixOf (x:xs) (y:ys) | x == y = tkIsPrefixOf xs ys
                           | otherwise = False

tkIsInfixOf :: (Eq a) => [a] -> [a] -> Bool
tkIsInfixOf [] _ = True
tkIsInfixOf _ [] = False
tkIsInfixOf a@(x:xs) b@(y:ys) | tkIsPrefixOf a b = True
                              | otherwise = tkIsInfixOf a ys
                              
tkIsSuffixOf :: (Eq a) => [a] -> [a] -> Bool
tkIsSuffixOf _ [] = False
tkIsSuffixOf a@(x:xs) b@(y:ys) | lena > lenb = False
                               | lena == lenb = tkIsPrefixOf a b
                               | otherwise = tkIsSuffixOf a ys
            where lena = length a
                  lenb = length b

tkZip :: [a] -> [b] -> [(a,b)]
tkZip [] _ = []
tkZip _ [] = []
tkZip (x:xs) (y:ys) = (x,y) : tkZip xs ys

tkZipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
tkZipWith _ [] _ = []
tkZipWith _ _ [] = []
tkZipWith f (x:xs) (y:ys) = (f x y) : tkZipWith f xs ys

tkLines :: String -> [String]
tkLines [] = []
tkLines ls@(x:xs) = (line) : (tkLines rest)
    where (line, restWithNewline) = tkBreak (\a -> a == '\n') ls
          rest = drop 1 restWithNewline

tkUnlines :: [String] -> String
tkUnlines [] = []
tkUnlines (l:ls) = l ++ "\n" ++ tkUnlines ls

tkWords :: String -> [String]
tkWords [] = []
tkWords ws@(x:xs) = word : tkWords rest
    where (word, restWithWhitespace) = tkBreak (\a -> tkIsWhitespace a) ws
          rest = tkDropWhile tkIsWhitespace restWithWhitespace

-- This is the most basic helper...
tkIsWhitespace :: Char -> Bool
tkIsWhitespace x = case x of
                        '\r' -> True
                        '\n' -> True
                        '\t' -> True
                        ' ' -> True
                        _ -> False

tkUnwords :: [String] -> String
tkUnwords [x] = x
tkUnwords (x:xs) = x ++ " " ++ tkUnwords xs

tkMap :: (a -> b) -> [a] -> [b]
tkMap _ [] = []
tkMap f (x:xs) = f x : tkMap f xs

tkSum :: Num a => [a] -> a
tkSum [] = 0
tkSum (x:xs) = x + tkSum xs

{- Note: Folds are named from where they start.
A left fold starts at the beginning of a list (left-to-right)
A right fold starts at the end of a list (right-to-left)
-}
tkFoldl :: (a -> b -> a) -> a -> [b] -> a
tkFoldl f acc [] = acc
tkFoldl f acc (x:xs) = tkFoldl f acc' xs
    where acc' = f acc x

{- Helpful for reference to track how foldl works on simple input
    foldl (+) 0 [1,2,3]
        == foldl (+) 0 (1:2:3:[])
        == foldl (+) (0+1) (2:3:[])
        == foldl (+) ((0+1)+2) (3:[])
        == foldl (+) (((0+1)+2)+3) ([])
        == (((0+1)+2)+3)
-}

tkFoldr :: (a -> b -> b) -> b -> [a] -> b
{- a is the type of an element, b is the acc type -}
tkFoldr _ acc [] = acc
tkFoldr f acc (x:xs) = f x (tkFoldr f acc xs)
{- Helpful for reference to also track how foldr works
    foldr (+) 0 [1,2,3]
        == foldr (+) 0 (1:2:3:[])
        == 1+foldr (+) 0 (2:3:[])
        == 1+(2+foldr (+) 0 (3:[]))
        == 1+(2+(3+foldr (+) 0 ([])))
        == (1+(2+(3+0)))
-}

{- 
foldl uses the acc value immediately and combines it with the first 
element to make a new acc value, with which a direct, contained 
recursive call is made to process the rest of the list.

foldr waits to use the acc value until the end, combining it with the 
last element. It makes recursive calls deferring function application of 
each until the end of the list is reached.

foldr is equivalent to replacing the empty list with the acc value, and 
replacing each cons (:) in the list with an application of the function f 
provided.

In the book, f is called step, and acc is called zero.

foldl creates a thunk. There are space and time concerns for this, so 
expect to see foldr used more often. Excessive thunking is also called a space leak.
foldl' does not build up thunks.
-}

tkLookup :: Eq a => a -> [(a,b)] -> Maybe b
tkLookup key [] = Nothing
tkLookup key ((k,v):xs) | key == k = Just v
                        | otherwise = tkLookup key xs

tkLookupFoldr :: Eq a => a -> [(a,b)] -> Maybe b
tkLookupFoldr key = snd . (foldr find (key, Nothing))
    where find (k,v) (key', acc) | key' == k = (key', Just v)
                                 | otherwise = (key', acc)

tkLookupFoldl :: Eq a => a -> [(a,b)] -> Maybe b
tkLookupFoldl key = fst . (foldl' find (Nothing, key))
    where find (acc, key') (k,v) | key' == k = (Just v, key')
                                 | otherwise = (acc, key')
                                 
tkReplicate :: Int -> a -> [a]
tkReplicate 0 _ = []
tkReplicate n x = x : tkReplicate (n-1) x