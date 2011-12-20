import Data.List (sort, (\\), nub)

{- Terms: bare skyline = skyline without visibility infos -}

sl0 = [[0,2,1,0],[2,0,0,1],[1,0,0,2],[0,1,2,0]]
sl1 = compVisibility [[10,20,30,40],[20,30,40,10],[30,40,10,20],[40,10,20,30]]
sl2 = [[0,5,4,3,2,1,0],[5,0,0,0,0,0,1],[4,0,0,0,0,0,2],[3,0,0,0,0,0,2],[2,0,0,0,0,0,2],[1,0,0,0,0,0,2],[0,1,2,2,2,2,0]]

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
transpose m = [ map (!! i) m | i <- [ 0 .. s ] ]
    where (rows,cols) = dim m
          s = cols - 1

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

initialRow :: Integer -> Row
initialRow n = map (* 10) [1 .. n]

perms :: Row -> [Row]
perms [] = [[]]
perms r = [ x:xs | x <- r, xs <- perms (r \\ [x])]

{- Takes 2 rows and returns whether all nonzero positions match.
 - rowsMatch [10,20,40,30] [0,20,0,0] -> True -}

rowsMatch :: Row -> Row -> Bool
rowsMatch x y = length x == length y &&
                and [ a == b | (a,b) <- zip x y, a /= 0, b /= 0 ]

{- Takes a template row,
 - and returns a list of valid candidate rows.
 - candidateRows [3,10,0,0,1] -> [[3,10,20,30,1]]  -}
candidateRows :: Row -> [Row]
candidateRows t = filter (rowsMatch t) (map visline rows)
    where rows = perms (initialRow (fromIntegral $ length bt))
          bt = mid t

replaceRow :: Int -> Skyline -> Row -> Skyline
replaceRow i m r = (take i m) ++ [r] ++ (drop (i + 1) m)

candidates :: Skyline -> [Skyline]
candidates m = filter isValidSL cs
    where
    cs = map (\x -> [head m] ++ x ++ [last m]) (candidates' m 1)

{- Takes a template matrix and a row index. Returns a list of candidate
 - matrices which fulfill the horizontal visibility requirements.
 - NOTE: vertical visibility requirements are NOT checked, and the
 - returned matrices are missing the vertical visibility rows! -}

candidates' :: Skyline -> Int -> [Skyline]
candidates' m i
    | i > len = [[]]
    | otherwise = concat [ map (r:) (next r) | r <- candidateRows t ]
    where
    {- Last row is visibility info, and we are only interested in
     - skyline rows -}
    len = length m - 2
    t = row m i

    {- Returns all possible combinations of next lines. Terminates if m is invalid -}
    next r = if not (mvalid m) then [] else candidates' (replaceRow i m r) (i + 1)

    {- Returns whether columns of stripped partial matrix contain no duplicates -}
    mvalid = (all noDups) . mid . transpose . (take i) . mid

    {- Takes a row and returns True if it contains no duplicate items -}
    noDups r = (length $ nub r) == (length r)

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

buildSkyscrapers :: Skyline -> Maybe Skyline
buildSkyscrapers m
    | cs == [] = Nothing
    | otherwise = Just (head cs)
    where cs = candidates m

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
