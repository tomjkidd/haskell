module Convert ( convert ) where

{- The main goal is to create a program that can take a number and 
print its word representation -}

{- To start, try to solve a simpler problem, 0 <= n < 100 -}
convert2 n = combine2 (digits2 n)
digits2 n = (n `div` 10, n `mod` 10)

units = ["zero","one", "two", "three", "four", "five",
    "six", "seven", "eight", "nine", "ten"]

teens = ["eleven", "twelve", "thirteen", "fourteen", "fifteen",
    "sixteen", "seventeen", "eighteen", "nineteen"]

tens = ["twenty", "thirty", "forty", "fifty", "sixty", "seventy",
    "eighty", "ninety"]
    
combine2 (0, u) = units !! u
combine2 (1, 0) = units !! 10
combine2 (1, u) = teens !! (u - 1)
combine2 (t, 0) = tens !! (t-2)
combine2 (t, u) = tens !! (t-2) ++ "-" ++ units !! (u)

{- Now for 0<= n < 1000 -}
convert3 n = combine3 (digits3 n)
digits3 n = (n `div` 100, n `mod` 100)

combine3 (0, t) = convert2 (t)
combine3 (h, 0) = units !! h ++ " hundred"
combine3 (h, t) = units !! h ++ " hundred " ++ convert2(t)

{- Now for 0<= n < 1000000 -}
convert6 n = combine6 (digits6 n)
digits6 n = (n `div` 1000, n `mod` 1000)

combine6 (0, h) = convert3 h
combine6 (m, 0) = convert3 m ++ " thousand"
combine6 (m, h) = convert3 m ++ " thousand" ++ " " ++ convert3 h

{- Now for 0<= n < one billion-}
convert9 n = (prefix n) ++ combine9 (digits9 (abs n))
digits9 n = (n `div` 1000000, n `mod` 1000000)

prefix n = if n < 0 then "negative " else ""

combine9 (0, t) = convert6 t
combine9 (m, 0) = convert6 m ++ " million"
combine9 (m, t) = convert6 m ++ " million" ++ " " ++ convert6 t

convert = convert9