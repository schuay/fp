-- 1

{- Our number space -}
pSpace = [1,5 ..]

{- Returns whether x is a factor of y -}
isFactorOf x y = y `mod` x == 0

{- Applied to a list of numbers, returns a corresponding
 - list marking the (factor of x) positions = True -}
isFactorOfL :: (Integral a) => a -> [a] -> [Bool]
isFactorOfL x y = map (isFactorOf x) y

{- Returns a list of all factor candidates n <- pSpace for x
 - with n <= x/5 -}
factorCandidates x = takeWhile smallerThanFifth (tail pSpace)
    where
    smallerThanFifth = (<= (x `div` 5))

{- Returns whether x <- pSpace -}
inP :: Integer -> Bool
inP x = x `elem` (takeWhile (<= x) pSpace)

{- Returns whether x is a Primal number -}
istPrimal :: Integer -> Bool
istPrimal x
    | not (inP x) = False
    | otherwise = faktorisiere x == []

-- 2

{- Returns a list of all possible factor combinations
 - of x; contains doubles: [(1,3), (3,1)] -}
faktorisiere :: Integer -> [(Integer, Integer)]
faktorisiere x
    | not (inP x) = error "Unzulaessig"
    | otherwise = zip factorList (map (div x) factorList)
    where
    factorList = filter (`isFactorOf` x) (factorCandidates x)

-- 3

type Editor = String
type Suchzeichenreihe = String
type Index = Integer
type Vorkommen = Integer
type Alt = String
type Neu = String

{- Returns whether the beginning of haystack matches needle -}
matches :: Suchzeichenreihe -> Editor -> Bool
matches "" _ = False
matches needle haystack = needle == (take (length needle) haystack)

{- Returns the index of the first found element or -1 if not found -}
suche :: Editor -> Suchzeichenreihe -> Index
suche haystack needle
    | indexList == [] = (-1)
    | otherwise = head indexList
    where
    indexList = sucheAlle haystack needle

-- 4

{- Returns all indices of found instances of needle in haystack.
 - Searching for "aba" in "ababa" returns [0] -}
recursiveFind :: Index -> Editor -> Suchzeichenreihe -> [Index]
recursiveFind _ [] _ = []
recursiveFind _ _ [] = []
recursiveFind pos haystack needle
    | needle `matches` haystack = pos : (recursiveFind nextPos nextHaystack needle)
    | otherwise = recursiveFind (succ pos) (drop 1 haystack) needle
    where
    needleLen = toInteger (length needle)
    nextPos = pos + needleLen
    nextHaystack = drop (length needle) haystack

{- Wrapper function for recursiveFind; returns all indices of found
 - instances of needle in haystack -}
sucheAlle :: Editor -> Suchzeichenreihe -> [Index]
sucheAlle = recursiveFind 0

-- 5

{- Replaces the nth (index) occurence of alt in text with neu.
 - If there is no nth occurence of alt, returns text. -}
ersetze :: Editor -> Vorkommen -> Alt -> Neu -> Editor
ersetze text index alt neu
    | index < 0 || index > fromIntegral (length indexList) = text
    | otherwise = insFst begin ++ neu ++ insSnd end
    where
    indexList = sucheAlle text alt
    index0 = fromIntegral (pred index)
    begin = fromIntegral (indexList !! index0)
    end = begin + (length alt)
    insFst x = take x text
    insSnd x = drop x text
