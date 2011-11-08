Types for all following assignments.

> type Matrix = [[Integer]]
> type Laenge = Integer

> type Skalar = Integer
> type Zeilen = Integer
> type Spalten = Integer
> type SpaZei = Integer
> type Fuellwert = Integer
> type ProtoMatrix = ([[Integer]],Zeilen,Spalten,Fuellwert)

> type Typung_mnpw = (Zeilen,SpaZei,Spalten,Fuellwert)
> type Typung_mnw = (Zeilen,Spalten,Fuellwert)
> type Typung_mw = (SpaZei,Fuellwert)
> type Potenz = Integer
> type ProtoprotoMatrix = [[Integer]]


Test data

> p1 = [[1,2,3], [4,5,6]]
> p2 = [[1,2], [3,4], [5,6]]
> p3 = [[1,2], [3,4]]

1 :: Returns a normed matrix according to the ProtoMatrix specification,
multiplied by the given scalar.

> msk :: ProtoMatrix -> Skalar -> Matrix
> msk (input, rows, cols, fill) scalar = [ map (scalar *) row | row <- matrix ]
>   where
>   matrix = anp2 input rows cols fill

2 :: Returns the matrix product of p1 and p2 after norming them according
to the provided Typung_mnpw specification.

> mm :: ProtoprotoMatrix -> ProtoprotoMatrix -> Typung_mnpw -> Matrix
> mm p1 p2 (m, n, p, fill) = map genRow m1
>   where
>   m1 = anp2 p1 m n fill

Transpose m2 because it's simpler to handle rows than columns.

>   m2 = transp p2 n p fill

genRow takes a row from lhs and calculates resulting row by forming
a scalar product with all rows from m2.

>   genRow :: [Integer] -> [Integer]
>   genRow lhs = [ sproduct lhs rhs | rhs <- m2 ]

sproduct takes 2 lists of integers and returns their scalar product.

>   sproduct :: [Integer] -> [Integer] -> Integer
>   sproduct lhs rhs = foldr (+) 0 $ zipWith (*) lhs rhs

3 :: Returns the matrix sum of p1 and p2 after norming them according
to the provided Typung_mnw specification.

> ms :: ProtoprotoMatrix -> ProtoprotoMatrix -> Typung_mnw -> Matrix
> ms p1 p2 (m, n, fill) = zipWith addRows m1 m2
>   where
>   m1 = anp2 p1 m n fill
>   m2 = anp2 p2 m n fill
>   addRows = zipWith (+)

4 :: Returns the nth matrix potenz of p after norming p according to
the provided Typung_mw specification. Passing a negative n results in
error "unzulaessig".

> identity :: Integer -> Matrix
> identity 0 = error "unzulaessig"
> identity m = map identityRow [0 .. mInt - 1]
>   where
>   identityRow n = replicate n 0 ++ [1] ++ replicate (mInt - 1 - n) 0
>   mInt = fromIntegral m

> mp :: ProtoprotoMatrix -> Typung_mw -> Potenz -> Matrix
> mp p (m, fill) n
>   | n < 0 = error "unzulaessig"
>   | n == 0 = identity m
>   | otherwise = foldl mult (identity m) (replicate nInt p)
>   where
>   mult lhs rhs = mm lhs rhs (m, m, m, fill)
>   nInt = fromIntegral n

-------------------------------------------------------------------------
The full contents of assignment 3 which we can reuse to some extent here.

1

Takes a list of lines and returns the length of the longest line.

> longestLine :: [[a]] -> Int
> longestLine line = maximum $ map length line

Pads a line to len with fill.

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
> transp _ _ 0 _ = error "unzulaessig"
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
