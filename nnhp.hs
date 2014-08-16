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
-}

pack2 :: (Eq a) => [a] -> [[a]]
pack2 [] = []
pack2 (x:xs) = (x : taken) : pack (remaining)
    where taken = takeWhile (==x) xs
          remaining = dropWhile (==x) xs
{- Here I just wanted to condense the expression and move the details into a where clause -}        