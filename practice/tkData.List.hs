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
