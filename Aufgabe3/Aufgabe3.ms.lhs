Some type definitions.

> type Matrix = [[Integer]]
> type Zeilen = Integer
> type Spalten = Integer
> type Fuellwert = Integer
> type Laenge = Integer

-- 1 --

anp1:
Returns an (length l, x)-matrix, where x is the length of the longest list in l.

> anp1 :: [[Integer]] -> Matrix
> anp1 [] = [[1]]
> anp1 l = anp2 l lenz lens 0
>   where lenz = fromIntegral (length l)
>         lens = fromIntegral (foldl (max) 0 (map (length) l))

getN:
Returns a list of as with a length of n. If the original list is shorter than
n it is padded with ws.

> getN :: [a] -> Integer -> a -> [a]
> getN as n w
>   | n <= 0    = error "unzulaessig"
>   | len < exp = getN (as ++ [w]) n w
>   | otherwise = take exp as
>   where len = length as
>         exp = fromInteger(n)

-- 2 --

anp2:
Returns a (z,s)-matrix. If lists in l are too short, they are padded with w.
If l is too short it is padded with lists of ws with the length s.

> anp2 :: [[Integer]] -> Zeilen -> Spalten -> Fuellwert -> Matrix
> anp2 l z s w = map (\x -> getN x s w) (getN l z [])

-- 3 --

transp:
First applies anp2 to l, then transposes the resulting (z,s)-matrix and returns
the (s,z)-matrix.

> transp :: [[Integer]] -> Zeilen -> Spalten -> Fuellwert -> Matrix
> transp _ _ 0 _ = error "unzulaessig"
> transp l z s w = map (\x -> x!!0) anpL : rest
>   where anpL = anp2 l z s w
>         rest
>           | s==1      = []
>           | otherwise = transp (map (drop 1) anpL) z (s-1) w

-- 4 --

sp:
Returns the scalar product of the vectors l1 and l2. Only the first row of l1
and the first column of l2 are used. If either is shorter than l it is padded
with ws.

> sp :: [[Integer]] -> [[Integer]] -> Laenge -> Fuellwert -> Integer
> sp l1 l2 l w
>   | l <= 0     = error "unzulaessig"
>   | otherwise = foldl (+) 0 (zipWith (*) anpL1 anpL2)
>   where anpL1 = (anp2 l1 1 l w)!!0
>         anpL2 = (transp l2 l 1 w)!!0
