-- This file is meant to solve the ninety-nine haskell problems
-- http://www.haskell.org/haskellwiki/H-99:_Ninety-Nine_Haskell_Problems

-- 1. Find the last element of a list
myLast :: [a] -> a
myLast [x] = x
--myLast (x:xs) = myLast xs -- My answer was different due to the _
myLast (_:xs) = myLast xs
{- The key here is to express the pattern [x] in order to match when 
the list has only a single element, and to recursively remove an 
element at a time until only that last element is left.-}

-- 2. Find the last but one element of a list
myButLast :: [a] -> a
--myButLast [x] = x
--myButLast xs = head (tail (reverse xs)) -- Found that another way is just myButLast = head . tail . reverse
myButLast = head . tail . reverse
{- The key to this answer is that if we call reverse and then tail, 
we put the second to last element at the front of the resulting list. 
Then head will access that element. -}

-- I wanted to find a lower level way to solve the problem...
myButLast2 :: [a] -> a
myButLast2 (x:y:[]) = x
myButLast2 (_: xs) = myButLast2 xs
{- The key to this answer was to express the pattern (x:y:[]) in 
order to match when the list has only two elements. -}

--another nice one...
myButLast3 :: [a] -> a
myButLast3 [x,_] = x
myButLast3 (_:xs) = myButLast3 xs

-- 3. Find the K'th element of a list. The first element in the list is 1 (one-index!)
myElementAt :: [a] -> Int -> a
--myElementAt xs 1 = head xs -- This is equivalent to the line below...
myElementAt (x:_) 1 = x
myElementAt (_:xs) n = myElementAt xs (n-1)
myElementAt _ _ = error "Index out of bounds"
{- The key here is again pattern matching. When n is greater than 
one, we want to reduce the length of the list by one and decrement 
how many more elements to skip. When n is equal to 1, we take the 
head of the list -}

-- 4. Find the number of elements of a list
myLength :: [a] -> Int
myLength [] = 0
myLength (x:xs) = 1 + myLength xs
{- I have encountered this one a few times now, where the recursive 
definition allows you to blow through the array. -}

myLength2 :: [a] -> Int
--myLength2 xs = sum (map (\x -> 1) xs)
myLength2 = sum . map (\x -> 1) 
{- Like for myButLast, xs can be excluded altogether and function 
composition can be used.
Here, each element can be mapped to one, and then summed to get the 
result. -}

-- 5. Reverse a list
myReverse :: [a] -> [a]
myReverse [] = []
myReverse xs = (last xs) : myReverse (init xs)
{- The key here is that to reverse an array, we want to build a new 
one from the back to the front. For example myReverse [1,2,3,4,5] 
should equal [5,4,3,2,1] which is equal to 5:4:3:2:1:[]. So for each 
recursion, we grab the last element and then pass init to recursively 
reduce the list. Each step adds the last element as the next element 
to the list. The base case ensures that the last step is to use cons 
with nil, which completes the list syntax. -}

myReverse2 :: [a] -> [a]
--myReverse2 xs = foldl (flip (:)) [] xs -- again, partial application can be used to remove the xs...
myReverse2 = foldl (flip (:)) []
{- I wanted to build this function using a fold. foldl would go from 
left to right, so foldl would be needed in order to build the desired 
list (ie 5:4:3:2:1:[]). So, with starting reasoning similar to the 
myReverse, I looked at the type for foldl.

foldl :: (a -> b -> a) -> a -> [b] -> a

Here, my desired output is [b], so this turns to...

foldl :: ([b] -> b -> [b]) -> [b] -> [b] -> [b]

where the first arg is the accumulator function, the second arg is 
the initial value of the accumulator and the third argument is the 
list to reverse. The last term is the reversed list that is the 
return value.

(:) :: a -> [a] -> [a]

The function argument in foldl wants [a] -> a -> [a], which can be 
achieved with the flip function.

flip (:) :: [a] -> a -> [a]

Now the empty list is provided as the initial accumulator value, and 
when xs is supplied to the function, it will fold with cons to build 
the reversed list. -}

-- 6. Find out whether a list is a palindrome
--isPalindrome :: Ord a => [a] -> Bool -- The Ord can be relaxed to just equality, since that is the only test performed by the function
isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome [] = True
--isPalindrome [x] = True -- A don't care can be inserted since x isn't used
isPalindrome [_] = True
isPalindrome xs = (head xs == last xs) && isPalindrome ((init . tail) xs)
{- Here, we check that the head and last element are the same in the 
list and then pass the remaining elements of the list to recursively 
check them. When either 1 (odd number of elements) or 0 (even number 
of elements) is matched, recursion stops. -}

-- 7. Flatten a nested list structure
flatten :: [[a]] -> [a]
flatten (xs : []) = xs
flatten ((xs):(ys)) = xs ++ flatten ys
{- The site mentioned that a data type would need to be introduced, so 
I thought to first simplify the problem and make the input a list of 
lists instead of a list of items and/or other lists. -}

data NestedList a = Elem a | List [NestedList a]
{- Here a algebraic data type NestedList is defined. It can either be an element,
Elem, or a list of other NestedElements. -}
nlFlatten :: NestedList a -> [a]
nlFlatten (Elem a) = [a]
nlFlatten (List []) = []
nlFlatten (List (x:xs)) = nlFlatten x ++ nlFlatten (List xs)
{- Here each type of potential input is handled: If just an element, the 
value from the element is put into a list. If a List, we return an empty 
list if the list is empty, otherwise we handle the first element in the 
list and concat this with a recursive call to flatten the rest of 
the list.

To contrast this definition with flatten, NestedList could be an 
arbitrarily deep nesting at any point in the list, whereas flatten 
can only handle the strict list of lists.

Part of the power of defining it this way is that it is concise, 
but not at the expense of generality.
-}

-- 8. Eliminate consecutive duplicates of list elements
compress :: (Eq a) => [a] -> [a]
compress [] = []
compress [x] = [x]
compress (x:xs) = if x == head xs then compress xs
                  else x : compress xs
{- Here, if the head of the tail is the same as the current head, 
then we can ignore the element and recursively compress the tail.
When the head is not equal to the head of the tail, then we use 
cons to add it to the list.
-}

compress2 :: (Eq a) => [a] -> [a]
compress2 [] = []
compress2 [x] = [x]
compress2 (x:xs) | x == head xs = compress xs
                 | x /= head xs = x : compress xs
{- Here, pattern matching is used as an equivalent of the if then else -}

-- 9. Pack consecutive duplicates of list elements into sublists. If a list contains repeated elements they should be placed in separate sublists.

pack :: (Eq a) => [a] -> [[a]]
pack [] = []
--pack [x] = [[x]] -- This line is unnecessary because of the pattern below!
pack (x:xs) = (x : takeWhile (==x) xs) : pack (dropWhile (==x) xs)
{- This one again works with the (x:y:z:[]) style of building the 
sublists. We take all the matching xs from the front of xs to make 
an element for the result. This element is a list of the repeats, 
which I found out can also be acquired as part of a tuple with the 
span function of Data.List. Then, the remaining elements in the 
list can be recursively processed to group the remaining occurrences.

This is the third or forth time I've seen (element) used as a way 
to indicate enclosing an element. I played with adding and removing 
() to deal with function evaluation, and the solution showed me 
that I had some extra.

A function that does what pack does here is also part of Data.List as the group function.
-}

pack2 :: (Eq a) => [a] -> [[a]]
pack2 [] = []
pack2 (x:xs) = (x : taken) : pack (remaining)
    where taken = takeWhile (==x) xs
          remaining = dropWhile (==x) xs
{- Here I just wanted to condense the expression and move the details into a where clause -}

{- 10. Run-length encoding of a list. Use the result of problem P09 to implement the so-called run-length encoding data compression method. Consecutive duplicates of elements are encoded as lists (N E) where N is the number of duplicates of the element E
-}

encode :: (Eq a) => [a] -> [(Int, a)]
encode xs = map (\x -> (length x, head x)) (pack xs) 
{- When pack is used as the basis for the list of lists grouping each 
value, a map over the pack result can build the desired tuple with the 
length and head functions pretty easily.
-}

{- 11. Modified run-length encoding
Modify the result of P10 in such a way that if an element has no 
duplicates it is simply copied into the result list. Only elements with 
duplicates are transferred as (N E) lists.
-}

data SingleOrMultiple a = Single a | Multiple Int a
    deriving (Show)

encodeModified :: (Eq a) => [a] -> [SingleOrMultiple a]
encodeModified xs = map determineType (pack xs)
    where determineType ys = if length ys == 1 then Single (head ys) else Multiple (length ys) (head ys)
{- This is not too different from encode, the only special things is 
that a data type was created to represent the Single and Multiple 
constructors. A determine function was then created to create a Single 
or Multiple based on if the length == 1.
-}

-- 12. Decode a run-length encoded list
decompressSingleOrMultiple :: SingleOrMultiple a -> [a]
decompressSingleOrMultiple (Single a) = [a]
decompressSingleOrMultiple (Multiple 0 a) = []
decompressSingleOrMultiple (Multiple n a) = a: decompressSingleOrMultiple (Multiple (n-1) a)

decodeModified :: [SingleOrMultiple a] -> [a]
decodeModified [] = []
decodeModified (x:xs) = decompressSingleOrMultiple x ++ decodeModified xs
{- Here, the decompress function was created to define how to go from 
an a to a list of as based on if the element is a Single or Multiple. 
This result is then concatenated to the result of recursively doing 
this for the remaining elements.

Is there a function that can do something similar to what is needed in 
the standard list functions? Yes, replicate.

How can the solution change with replicate?
-}
decodeMod2 :: [SingleOrMultiple a] -> [a]
decodeMod2 [] = []
decodeMod2 ((Single x):xs) = x : decodeMod2 xs
decodeMod2 ((Multiple n x):xs) = (replicate n x) ++ decodeMod2 xs
{- When the head element is a Single, cons can be used to add the 
element. When the head element is a Multiple, concat is used to add the 
elements. In both cases, the recursive definition will process the 
remaining elements.
-}

-- 13. Run-length encoding of a list (direct solution)
{- Implement the so-called run-length encoding data compression method 
directly, ie don't explicitly create the sublists containing the 
duplicates, as in problem 9, but only count them. As in P11, simplify 
the result list by replacing the singleton lists (1 X) by X.
-}

encodeDirect :: Eq a => [a] -> [SingleOrMultiple a]
encodeDirect [] = []
encodeDirect (x:xs) | count == 1 = (Single x) : (encodeDirect xs)
                    | otherwise = (Multiple count x) : (encodeDirect rest)
                    where (spanned, rest) = span (==x) xs
                          count = 1 + (length spanned)
                          
{- Here, the span function is used to separate the list into the part 
that matches and the rest of the list. If the count is 1, then a Single 
is added to the list, otherwise a Multiple is added to the list. The 
rest of the list is processed recursively. -}

-- 14. Duplicate the elements of a list
dupli :: [a] -> [a]
dupli [] = []
dupli (x:xs) = x:x:dupli xs
{- This result is easy to obtain immediately, using the (head:tail) 
pattern, the current head is just used twice to create the next 
elements in the list. Recursively applying dupli to the remaining 
elements will duplicate every element in the list.-}

dupli2 :: [a] -> [a]
dupli2 [] = []
dupli2 (x:xs) = (replicate 2 x) ++ dupli2 xs

-- 15. Replicate the elements of a list a given number of times
repli :: [a] -> Int -> [a]
repli [] _ = []
repli _ 0 = []
repli (x:xs) n = (replicate n x) ++ repli xs n
{- Here I used the previous result as a basis, which made it pretty 
straight forward.
-}

repli2 :: [a] -> Int -> [a]
repli2 [] _ = []
repli2 _ 0 = []
repli2 (x:xs) n = x : repli2 [x] (n-1) ++ repli2 xs n
{- Here, I wanted to avoid using replicate. There are probably more 
obvious ways to implement this...
-}

-- 16. Drop every N'th element from a list
dropEvery :: [a] -> Int -> [a]
dropEvery [] _ = []
dropEvery xs n = front ++ dropEvery rest n
    where front = if length (take n xs) == n  then init(take n xs) else take n xs
          rest = drop n xs
{- The first implementation I made incorrectly handled the case when 
there are fewer elements remaining than the amount to drop. In that 
case, all the remaining elements should be kept and added to the list.
-}

dropEvery2 :: [a] -> Int -> [a]
dropEvery2 [] _ = []
dropEvery2 xs n = front xs ++ dropEvery2 (rest xs) n
    where hasEnough = length (take n xs) == n
          front | hasEnough = init . (take n)
                | otherwise = take n
          rest = drop n
{- Just wanted to use patterns instead of the if-then-else expression.
-}

-- 17. Split a list into two parts, the length of the first part is given. Do not use any predefined predicates.
{- When using take and drop, this is pretty trivial -}
split :: [a] -> Int -> ([a],[a])
split xs n = ((take n xs), (drop n xs))

{- What about when you can't use the built in functions... I'll just 
build my own local ones for practice. -}
split2 :: [a] -> Int -> ([a],[a])
split2 [] _ = ([],[])
split2 xs n = ((myTake n xs), (myDrop n xs))
    where myTake 0 xs = []
          myTake n (x:xs) = x : myTake (n-1) xs
          myDrop _ [] = []
          myDrop 0 (x:xs) = x : myDrop 0 xs 
          myDrop n (x:xs) = myDrop (n-1) xs
{- myTake will reach it's base case after the first n calls, building 
the front of the split. myDrop will ignore the list until after the 
first n calls, building the end of the split. -}

-- 18. Extract a slice from a list.

mySlice :: [a] -> Int -> Int -> [a]
mySlice [] _ _ = []
mySlice _ _ 2 = []
mySlice (x:xs) 1 n = x : mySlice xs 1 (n-1)
mySlice (x:xs) m n = mySlice xs (m-1) n
{-This was not protected at all for when m > n, so that aside, the 
recursive definition is pretty straight forward. Getting the pattern 
matching was the most sensitive part.
-}

mySlice2 :: [a] -> Int -> Int -> [a]
mySlice2 _ 0 _ = error "start must be greater than 1"
mySlice2 xs m n | m > n = error "start must be greater than end"
                | otherwise = take (n-m +1) $ drop (m-1) xs
{- An easy alternative is to drop elements until the start index and 
then take elements until the end index -}

-- 19. Rotate a list N places to the left
myRotate :: [a] -> Int -> [a]
myRotate xs n = (drop m xs) ++ (take m xs) 
    where m | n >= 0 = mod n (length xs)
            | n < 0  = mod ((length xs) + n) (length xs)
{- Here, the mod operator is used to drop the front elements and add
them to the rear. -}

-- 20. Remove the K'th element from a list
removeAt :: Int -> [a] -> [a]
removeAt 1 (x:xs) = xs
removeAt n y@(x:xs) | n >=0 = x : removeAt (n-1) xs
                    | otherwise = y
removeAt _ [] = []