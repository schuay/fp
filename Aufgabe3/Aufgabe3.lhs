Types for all following assignments

> type Matrix = [[Integer]]
> type Laenge = Integer
> type Zeilen = Integer
> type Spalten = Integer
> type Fuellwert = Integer

1

Takes a list of lines and returns the length of the longest line.

> longestLine :: [[a]] -> Int
> longestLine line = maximum $ map length line

Pads a line to a specific length with zeroes (0).

> pad :: Int -> a -> [a] -> [a]
> pad len fill list
>   | len < 1 = error "unzulaessig"
>   | otherwise = take len list ++ take rest padding
>   where
>   rest = len - (length list)
>   padding = repeat fill

Pads all lines of a matrix data structure to the length of the longest
line. Special case of anp2.

> anp1 :: [[Integer]] -> Matrix
> anp1 [] = [[1]]
> anp1 input = anp2 input rows cols 0
>   where
>   rows = fromIntegral $ length input
>   cols = fromIntegral $ longestLine input

2

Creates a matrix of specified dimensions by filling missing positions with
the given value.

> anp2 :: [[Integer]] -> Zeilen -> Spalten -> Fuellwert -> Matrix
> anp2 input rows cols fill = map padToCols padToRows
>   where
>   iRows = fromIntegral rows
>   iCols = fromIntegral cols
>   padToRows = pad iRows [] input
>   padToCols = pad iCols fill

3

Returns a normed (anp2) and then transposed matrix.

> transp :: [[Integer]] -> Zeilen -> Spalten -> Fuellwert -> Matrix
> transp input rows cols fill = [ transpR row | row <- [0 .. cols - 1] ]
>   where
>   matrix = anp2 input rows cols fill
>   transpR row = [ x !! (fromIntegral row) | x <- matrix ]

4

Returns the scalar product of lhs (normed to (1,l)) and rhs
(normed to (l,1)).

> sp :: [[Integer]] -> [[Integer]] -> Laenge -> Fuellwert -> Integer
> sp lhs rhs len fill = foldr (+) 0 $ zipWith (*) lhsNormed rhsNormed
>   where

We need our lhs and rhs in the form of [a,b,c], [x,y,z] so we can easily
use zipWith and foldr to form the scalar product. Norm lhs and extract the
first row. Norm and transpose rhs and extract the first row.

>   lhsNormed = anp2 lhs 1 len fill !! 0
>   rhsNormed = transp rhs len 1 fill !! 0
