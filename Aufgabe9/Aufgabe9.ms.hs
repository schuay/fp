-- 2

type Row = [Integer]
type Sudoku = [Row]

data Variant = Basic | Cross | Color deriving (Eq,Show)

type RowInd = Integer
type ColInd = Integer
type Position = (RowInd,ColInd)

type Scale = Integer

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

getRow :: Sudoku -> RowInd -> Row
getRow a i = a!!(fromIntegral i)

getCol :: Sudoku -> ColInd -> [Integer]
getCol [] _ = []
getCol (a:as) i = a!!(fromIntegral i) : (getCol as i)

getSub :: Sudoku -> Integer -> [Integer]
getSub a i
  | length a < 7 = []
  | otherwise    = [cRow!!pos] ++ [cRow!!(1+pos)] ++ [cRow!!(2+pos)] ++ (getSub next i)
  where cRow = a!! (fromIntegral . part 3) i
	next = tail a
	pos = fromIntegral ((i `mod` 3) * 3)

getDia1 :: Sudoku -> [Integer]
getDia1 [] = []
getDia1 (a:as) = a!!0 : (getDia1 next)
  where next = [ tail x | x<-as ]

getDia2 :: Sudoku -> [Integer]
getDia2 [] = []
getDia2 (a:as) = (last a) : (getDia2 next)
  where next = [ init x | x<-as ]

getColorF :: Sudoku -> Integer -> [Integer]
getColorF [] _ = []
getColorF a i = [cRow!!pos] ++ [cRow!!(3+pos)] ++ [cRow!!(6+pos)] ++ (getColorF next i)
  where cRow = a!! (fromIntegral . part 1) i
        next = drop 3 a
	pos = fromIntegral (i `mod` 3)

validInd = [0..8]
validNum = [1..9]

-- a

isValidSDKSpecial :: Sudoku -> Variant -> Bool
isValidSDKSpecial _ Basic = True
isValidSDKSpecial a Cross = (check19 . getDia1) a && (check19 . getDia2) a
isValidSDKSpecial a Color = and (map (check19 . getColorF a) validInd)

isValidSDK :: Sudoku -> Variant -> Bool
isValidSDK a v = rowsOk && colsOk && subsOk && specialOk v
  where rowsOk = and (map (check19 . getRow a) validInd)
        colsOk = and (map (check19 . getCol a) validInd)
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
getSpecial a (r,c) Color = getColorF a col
  where col = (r `mod` 3) * 3 + (c `mod` 3)

allowedChars :: Sudoku -> Position -> Variant -> [Integer]
allowedChars a (r,c) v = [ x | x <- validNum, allowed x ]
  where cRow = getRow a r
        cCol = getCol a c
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
