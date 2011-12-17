import Data.List (sort)

{- Terms: bare skyline = skyline without visibility infos -}

{- skyline -}

type Row = [Integer]
type Skyline = [Row]

{- 1a -}

row :: [a] -> Int -> a
row m i = m !! i

col :: [[a]] -> Int -> [a]
col m i = map (!! i) m

cell :: [[a]] -> Int -> Int -> a
cell m i j = row (col m j) i

allRows = id

allCols m = map (col m) [ 0 .. (length m) - 1 ]

{- Returns (0,0) if matrix is empty or invalid; (rows,cols) otherwise -}

dim :: [[a]] -> (Int, Int)
dim [] = (0,0)
dim [[]] = (0,0)
dim m | not colsequal = (0,0)
      | otherwise = (rows, head collens)
    where rows = length m
          collens = map length m
          colsequal = and $ zipWith (==) collens (tail collens)

mid = tail . init

stripVisibility :: [[a]] -> [[a]]
stripVisibility = (map mid) . mid

{- Takes a skyline, returns whether the size is valid. -}

isValidSLSize :: Skyline -> Bool
isValidSLSize m = a > 2 && b > 2
    where (a,b) = dim m

{- Takes a bare skyline row of valid size,
 - returns whether it contains all values. -}

isValidSLLine :: Row -> Bool
isValidSLLine r = and $ zipWith (==) values (sort r)
    where values = map (* 10) [ 1 .. ]

allLinesValid :: Skyline -> Bool
allLinesValid m = and $ map isValidSLLine allLines
    where allLines = (allRows s) ++ (allCols s)
          s = stripVisibility m

{- lvis,rvis,visline expect bare skyline rows -}

lvis :: Row -> Integer
lvis s = fromIntegral $ lvis' s 0
    where lvis' (x:xs) mx = lvis' xs (max x mx) +  if x > mx then 1 else 0
          lvis' _ _ = 0

rvis :: Row -> Integer
rvis = lvis . reverse

{- visline takes a (nonempty and valid) bare skyline row
 - and returns a an equivalent line with added visibility fields -}

visline :: Row -> Row
visline s = [lvis s] ++ s ++ [rvis s]

{- Transposes a matrix -}
transpose :: [[a]] -> [[a]]
transpose m = [ map ( !! i) m | i <- [ 0 .. cols - 1 ] ]
    where (rows,cols) = dim m

{- standardize sets corner cells to 0 -}

standardize :: Skyline -> Skyline
standardize m = [ standardize' $ head m ] ++ (mid m) ++
                [ standardize' $ last m ]
    where standardize' r = [ 0 ] ++ (mid r) ++ [ 0 ]

{- Checks if the given skyline is an (n+2)x(n+2) matrix,
 - each row and column contains all values [ 10 .. 10 * n ],
 - and all visibility infos are correct. Returns true if all conditions
 - are satisfied, else false. -}

isValidSL :: Skyline -> Bool
isValidSL m = and [ isValidSLSize m,
                  allLinesValid m,
                  (standardize m) == (compVisibility (stripVisibility m)) ]

{- 1b -}

{- Given a skyline *without* visibility information (an n x n matrix),
 - calculates the visibility and returns the skyline *with* visibility
 - infos. If the given skyline is not valid, it is returned without change. -}

compVisibility :: Skyline -> Skyline
compVisibility m = standardize $ map visline colvis
    where colvis = (transpose . map visline . transpose) m

{- 1c -}

{- Given a (partially) empty skyline, returns a complete valid skyline if possible
 - or Nothing otherwise. Empty fields are set to 0. All existing skyline infos
 - need to be valid. Maximal size of passed skylines is 5 x 5. -}

-- buildSkyscrapers :: Skyline -> Maybe Skyline

{- sudoku -}

type Sudoku = [Row]

data Variant = Basic | Cross | Color deriving (Eq,Show)

{- 2a -}

{- Given a sudoku and a variant argument, returns true if the sudoku is a valid
 - sudoku of the specified variant and false otherwise. Empty fields are any
 - fields not containing a number from 1-9.
 -
 - Basic: 1-9 in each subsquare, row and column
 - Cross: Basic + in both diagonals
 - Color: Basic + in each position of subsquares -}

-- isValidSDK :: Sudoku -> Variant -> Bool

{- 2b -}

{- Given a sudoku and a variant argument, returns a solved version of possible
 - or Nothing otherwise. -}

-- solve :: Sudoku -> Variant -> Maybe Sudoku
