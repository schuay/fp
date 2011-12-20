import Data.List (sort, (\\), nub)
import Data.Maybe (listToMaybe)

{- Terms: bare skyline = skyline without visibility infos -}

sl0 = [[0,2,1,0],[2,0,0,1],[1,0,0,2],[0,1,2,0]]
sl1 = compVisibility [[10,20,30,40],[20,30,40,10],[30,40,10,20],[40,10,20,30]]
sl2 = [[0,5,4,3,2,1,0],[5,0,0,0,0,0,1],[4,0,0,0,0,0,2],[3,0,0,0,0,0,2],[2,0,0,0,0,0,2],[1,0,0,0,0,0,2],[0,1,2,2,2,2,0]]
sl3 = [[0,2,2,3,1,3,0],[2,0,0,0,0,0,2],[1,0,0,0,0,0,4],[2,0,0,0,0,0,3],[3,0,0,0,0,0,2],[3,0,0,0,0,0,1],[0,3,2,2,4,1,0]]

{- skyline -}

type Row = [Integer]
type Col = [Integer]
type Skyline = [Row]

{- 1a -}

row :: [a] -> Int -> a
row m i = m !! i

col :: [[a]] -> Int -> [a]
col m i = map (!! i) m

cell :: [[a]] -> Int -> Int -> a
cell m i j = row (col m j) i

{- Given a Skyline, returns a list of all rows -}

allRows :: Skyline -> [Row]
allRows = id

{- Given a Skyline, returns a list of all cols -}

allCols :: Skyline -> [Col]
allCols m = map (col m) [ 0 .. (length m) - 1 ]

{- Returns (0,0) if matrix is empty or invalid; (rows,cols) otherwise -}

dim :: [[a]] -> (Int, Int)
dim m | rows == 0 || cols == 0 = (0,0)
      | not colsequal = (0,0)
      | otherwise = (rows, cols)
    where rows = length m
          collens = map length m
          cols = head collens
          colsequal = and $ zipWith (==) collens (tail collens)

mid :: [a] -> [a]
mid = tail . init

{- Given a Skyline, returns a bare skyline -}

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

{- Takes a bare skyline, returns whether all rows and cols
 - contain all values -}

allLinesValid :: Skyline -> Bool
allLinesValid m = and $ map isValidSLLine allLines
    where allLines = (allRows m) ++ (allCols m)

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
transpose m = [ col m i | i <- [ 0 .. s ] ]
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
                    allLinesValid s,
                    (standardize m) == (compVisibility s) ]
    where s = stripVisibility m

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
    where rows = perms (initialRow len)
          len = fromIntegral $ length $ mid t

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
    | otherwise = [ r:s | r <- candidateRows t, s <- next r ]
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
compVisibility m
    | a < 2 || b < 2 || (not $ allLinesValid m) = m
    | otherwise = standardize $ map visline colvis
    where colvis = (transpose . map visline . transpose) m
          (a,b) = dim m

{- 1c -}

{- Given a (partially) empty skyline, returns a complete valid skyline if possible
 - or Nothing otherwise. Empty fields are set to 0. All existing skyline infos
 - need to be valid. Maximal size of passed skylines is 5 x 5. -}

buildSkyscrapers :: Skyline -> Maybe Skyline
buildSkyscrapers m = listToMaybe cs
    where cs = candidates m

-- 2

type Sudoku = [Row]

data Variant = Basic | Cross | Color deriving (Eq,Show)

type RowInd = Integer
type ColInd = Integer
type Position = (RowInd,ColInd)

type Scale = Integer

sudokuLen = 9
sudokuSubLen = 3
sudokuSubCount = sudokuLen `div` sudokuSubLen
validInd = [0 .. sudokuLen - 1]
validNum = [1 .. 9]


part :: Scale -> Integer -> Integer
part s i
  | i < 3     = 0*s
  | i < 6     = 1*s
  | otherwise = 2*s

check19 :: [Integer] -> Bool
check19 [] = True
check19 x = (as == rest) && (check19 rest)
  where (a:as) = [ y | y<-x, y>0, y<=9 ]
        rest = [ z | z<-as, z/=a ]

getSub :: Sudoku -> Integer -> [Integer]
getSub a i
  | length a < 7 = []
  | otherwise    = [cRow!!pos] ++ [cRow!!(1+pos)] ++ [cRow!!(2+pos)] ++ (getSub next i)
  where cRow = a!! (fromIntegral . part 3) i
	next = tail a
	pos = fromIntegral ((i `mod` 3) * 3)

getDia1 :: Sudoku -> [Integer]
getDia1 s = [ r !! i | (i,r) <- zip [0..] s ]

getDia2 :: Sudoku -> [Integer]
getDia2 = getDia1 . (map reverse)

getColorF :: Sudoku -> Integer -> [Integer]
getColorF [] _ = []
getColorF a i = [cRow!!pos] ++ [cRow!!(3+pos)] ++ [cRow!!(6+pos)] ++ (getColorF next i)
  where cRow = a!! (fromIntegral . part 1) i
        next = drop 3 a
	pos = fromIntegral (i `mod` 3)

-- a

isValidSDKSpecial :: Sudoku -> Variant -> Bool
isValidSDKSpecial _ Basic = True
isValidSDKSpecial a Cross = (check19 . getDia1) a && (check19 . getDia2) a
isValidSDKSpecial a Color = and (map (check19 . getColorF a) validInd)

isValidSDK :: Sudoku -> Variant -> Bool
isValidSDK a v = rowsOk && colsOk && subsOk && specialOk v
  where rowsOk = and (map (check19 . row a . fromIntegral) validInd)
        colsOk = and (map (check19 . col a . fromIntegral) validInd)
        subsOk = and (map (check19 . getSub a) validInd)
	specialOk = isValidSDKSpecial a

-- b

getSpecial :: Sudoku -> Position -> Variant -> [Integer]
getSpecial _ (_,_) Basic = []
getSpecial a (r,c) Cross
  | r == 4 && c == 4 = getDia1 a ++ getDia2 a
  | r == c           = getDia1 a
  | c == 8 - r       = getDia2 a
  | otherwise        = []
getSpecial a (r,c) Color = getColorF a color
  where color = (r `mod` 3) * 3 + (c `mod` 3)

allowedChars :: Sudoku -> Position -> Variant -> [Integer]
allowedChars a (r,c) v = [ x | x <- validNum, allowed x ]
  where cRow = row a (fromIntegral r)
        cCol = col a (fromIntegral c)
	cSub = getSub a ((part 3 r) + (part 1 c))
	cSpecial = getSpecial a (r,c) v
	allowed x = (not.or) (map (elem x) [cRow, cCol, cSub, cSpecial])

firstJust :: [Maybe a] -> Maybe a
firstJust [] = Nothing
firstJust (Nothing:xs) = firstJust xs
firstJust (x:xs) = x

replace :: Sudoku -> Position -> Integer -> Sudoku
replace (a:as) (r,c) i
  | r == 0 = replaceInRow a c i : as
  | otherwise = a : replace as (r-1,c) i
  where replaceInRow :: Row -> Integer -> Integer -> Row
        replaceInRow (r:rs) c i
	  | c == 0 = i : rs
	  | otherwise = r : replaceInRow rs (c-1) i

nextPos :: Position -> Position
nextPos (r,c)
  | c < 8     = (r,c+1)
  | otherwise = (r+1,0)

tryPos :: Sudoku -> Position -> Variant -> Integer -> Maybe Sudoku
tryPos a p v i = solvePos nextA (nextPos p) v
  where nextA = replace a p i

solvePos :: Sudoku -> Position -> Variant -> Maybe Sudoku
solvePos a (r,c) v
  | not validPosition   = Just a
  | cur `elem` validNum = solvePos a (nextPos (r,c)) v
  | otherwise           = firstJust (map (tryPos a (r,c) v) allowed)
  where allowed = allowedChars a (r,c) v
        cur = (a!!(fromIntegral r))!!(fromIntegral c)
	validPosition = r `elem` validInd && c `elem` validInd

solve :: Sudoku -> Variant -> Maybe Sudoku
solve a v
  | not (isValidSDK a v) = Nothing
  | otherwise            = solvePos a (0,0) v
