--1. Define a function flop that reverses a pair.
flop :: (a, b) -> (b,a)
flop (a,b) = (b,a)

--2. Define a function that takes two points (pairs) and returns a segment (a pair of points)
makeSegment pointA pointB = (pointA, pointB)

--3. Define a function that takes a segment and returns it's center point
center :: Fractional a => ((a,a),(a,a)) -> (a,a)
center ((px,py),(qx,qy)) = (px + (qx-px)/2, py + (qy-py)/2)

main = do
    print $ flop (1, "one")
    print $ makeSegment (1,2) (3,4)
    print $ center $ makeSegment (1,2) (3,4)