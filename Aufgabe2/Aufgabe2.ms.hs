-- 1, 2 Helpers

-- P
p = [1, 5 ..]

{-|inP:
 - Determines if n is in P.
 -}
inP :: Integer -> Bool
inP n = (n-1) `mod` 4 == 0

-- 1

{-|istPrimal:
 - Determines if n is the product of two numbers in P.
 -}
istPrimal :: Integer -> Bool
istPrimal n
  | not(inP n) = False
  | otherwise  = istPrimalSub 1 n
    where istPrimalSub :: Int -> Integer -> Bool
          istPrimalSub i n
            | p!!i > n `div` 2  = True
            | n `mod` p!!i == 0 = False
            | otherwise         = istPrimalSub (i+1) n

-- 2

{-|faktorisiere:
 - Returns a list of all factors of n or an error if n is not in p.
 -}
faktorisiere :: Integer -> [(Integer,Integer)]
faktorisiere n
   | not(inP n) = error "Unzulaessig"
   | otherwise  = faktorisiereSub 1 n
     where faktorisiereSub :: Int -> Integer -> [(Integer,Integer)]
           faktorisiereSub i n
             | p!!i > n `div` 2  = []
             | n `mod` p!!i == 0 = (p!!i, n `div` p!!i) : faktorisiereSub (i+1) n
             | otherwise         = faktorisiereSub (i+1) n

-- 3, 4, 5 Helpers

type Editor = String
type Suchzeichenreihe = String
type Index = Integer
type Vorkommen = Integer
type Alt = String
type Neu = String

-- 3

{-|searchFrom:
 - Returns the i + the index of the first appearance of s in e or -1 if s does
 - not occur in e.
 -}
searchFrom :: Editor -> Suchzeichenreihe -> Index -> Index
searchFrom [] _ _ = -1
searchFrom _ [] _ = -1
searchFrom e s i
  | (take (length s) e) == s = i
  | otherwise                = searchFrom (tail e) s (i+1)

{-|suche:
 - Returns the index of the first appearance of s in e or -1 if s does not
 - occur in e. (calls searchFrom)
 -}
suche :: Editor -> Suchzeichenreihe -> Index
suche e s = searchFrom e s 0

-- 4

{-|sucheAlleSub:
 - Returns a list with the indices (+i) of all appearances of s in e or an empty
 - list if s does not occur in e.
 -}
sucheAlleSub :: Editor -> Suchzeichenreihe -> Index -> [Index]
sucheAlleSub [] _ _ = []
sucheAlleSub _ [] _ = []
sucheAlleSub e s i
  | (take len e) /= s = sucheAlleSub (tail e) s (i+1)
  | otherwise         = i : (sucheAlleSub (drop len e) s (i+(fromIntegral len)))
    where len = length s

{-|sucheAlle:
 - Returns a list with the indices of all appearances of s in e or an empty
 - list if s does not occur in e.
 -}
sucheAlle :: Editor -> Suchzeichenreihe -> [Index]
sucheAlle e s = sucheAlleSub e s 0

-- 5

{-|ersetze:
 - Replaces the i-th appearance of the String s in the Editor e with t and returns
 - the modified Editor. If there is no i-th appearance of s in e, e is returned
 - unmodified.
 -}
ersetze :: Editor -> Vorkommen -> Alt -> Neu -> Editor
ersetze e i s t
  | ix < 0 || ix >= num = e
  | otherwise           = take ith e ++ t ++ (drop (len+ith) e)
    where ix = fromIntegral(i-1)
          num = length (sucheAlle e s)
	  ith = fromIntegral((sucheAlle e s)!!ix)
	  len = length s
