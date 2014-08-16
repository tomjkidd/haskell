{- I encountered this problem at an AthenaHealth interview
You start out with a piece of paper where the numbers go from 1 to 2^n,
where n is the number of folds you want to perform.

For example, when n is 3...
[[1,2,3,4,5,6,7,8]] is the starting arrangement

A left fold of the value would pivot the list at the middle, and rotate the left hand values over the right hand side.

"l"
[
	[4,3,2,1],
	[5,6,7,8]
]

A right fold would do something similar, except from right to left

"r"
[
	[8,7,6,5],
	[1,2,3,4]
]

The idea is that for an arbitrary set of instructions, we want to get the resulting 'stack' 

"lrl" ->
[[1,2,3,4,5,6,7,8]]
->
[[4,3,2,1],
[5,6,7,8]]
->
[[8,7],
[1,2],
[4,3],
[5,6]]
->
[[5],
[4],
[1],
[8],
[7],
[2],
[3],
[6]]

"lll" ->
[[1,2,3,4,5,6,7,8]]
->
[[4,3,2,1],
[5,6,7,8]]
->
[[6,5],
[3,4],
[2,1],
[7,8]]
->
[[7],
[2],
[3],
[6],
[5],
[4],
[1],
[8]]

-}

{- First, create a couple of support functions that will break a list into 
two pieces. These will help deal with the pivoting.
-}
leftHalf :: [a] -> [a]
leftHalf xs = take (div (length xs) 2) xs

rightHalf :: [a] -> [a]
rightHalf xs = drop (div (length xs) 2) xs

{- Next, define how the folds work in both directions.
-}
leftPaperFold :: [[a]] -> [[a]]
leftPaperFold lls = (map (\ls -> reverse (leftHalf ls)) (reverse lls))
	++ (map (\ls -> rightHalf ls) lls)
	
rightPaperFold :: [[a]] -> [[a]]
rightPaperFold lls = (map (\ls -> reverse (rightHalf ls)) (reverse lls)) ++ (map (\ls -> leftHalf ls) lls)

{- Provide a handler to allow a string to contain instructions and to 
operate on arbitrary input.
-}
paperfold :: String -> [[a]] -> [[a]]
paperfold [] xs = xs
paperfold (c:cs) xs | c == 'l' = paperfold cs (leftPaperFold xs)
paperfold (c:cs) xs | c == 'r' = paperfold cs (rightPaperFold xs)

{- Provide a function that creates the starting 2-D array so that just the
instruction string is needed as the input.
-}
takePaperfoldInstructions :: [Char] -> [[Int]]
takePaperfoldInstructions cs = paperfold cs [[1..2^(length cs)]]