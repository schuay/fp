Some type definitions.

> type Skalar = Integer
> type Matrix = [[Integer]]
> type Zeilen = Integer
> type Spalten = Integer
> type SpaZei = Integer
> type Fuellwert = Integer
> type Laenge = Integer
> type ProtoMatrix = ([[Integer]],Zeilen,Spalten,Fuellwert)

> type Typung_mnpw = (Zeilen,SpaZei,Spalten,Fuellwert)
> type Typung_mnw = (Zeilen,Spalten,Fuellwert)
> type Typung_mw = (SpaZei,Fuellwert)
> type Potenz = Integer
> type ProtoprotoMatrix = [[Integer]]

-- 3.2 --

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

anp2:
Returns a (z,s)-matrix. If lists in l are too short, they are padded with w.
If l is too short it is padded with lists of ws with the length s.

> anp2 :: [[Integer]] -> Zeilen -> Spalten -> Fuellwert -> Matrix
> anp2 l z s w = map (\x -> getN x s w) (getN l z [])

-- 3.3 --

transp:
First applies anp2 to l, then transposes the resulting (z,s)-matrix and returns
the (s,z)-matrix.

> transp :: [[Integer]] -> Zeilen -> Spalten -> Fuellwert -> Matrix
> transp _ _ 0 _ = []
> transp l z s w = map (\x -> x!!0) anpL : transp (map (drop 1) anpL) z (s-1) w
>   where anpL = anp2 l z s w

-- 3.4 --

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

-- 4.1 --

msk:
Returns the product of the matrix m and the Scalar sk.
m is generated from (l,z,s,w) via anp2, where l are some initial values, z is
the number of rows, s is the number of columns and w is the value to use if a
field of the matrix is unknown.

> msk :: ProtoMatrix -> Skalar -> Matrix
> msk (l,z,s,w) sk = map (map (sk*)) m
>   where m = anp2 l z s w

-- 4.2 --

mm:
Multiplies m1 and m2 and returns the product.
m1 and m2 are generated from p1, p2 and (m,n,p,w) via anp2, where p1 and p2
are some initial values for m1 and m2, m is the number of rows in m1, n is
the number of columns in m1 and the number of rows in m2, p is the number of
columns in m2 and w is the value to use if a field of either matrix is unknown.

> mm :: ProtoprotoMatrix -> ProtoprotoMatrix -> Typung_mnpw -> Matrix
> mm p1 p2 (m,n,p,w) = map (\x -> sp m1 x n w) (cols m2 p) : rest
>   where m1 = anp2 p1 m n w
>         m2 = anp2 p2 n p w
>         rest
>           | m==1      = []
>           | otherwise = mm (drop 1 m1) m2 (m-1,n,p,w)
>         cols :: Matrix -> Spalten -> [Matrix]
>         cols m 1 = [m]
>         cols m s = m : cols (map (drop 1) m) (s-1)

-- 4.3 --

ms:
Returns the sum of m1 and m2.
m1 and m2 are generated from p1, p2 and (m,n,w) via anp2, where p1 and p2
are some initial values for m1 and m2, m is the number of rows for m1 and m2,
n is the number of columns for m1 and m2 and w is the value to use if a field
of either matrix is unknown.

> ms :: ProtoprotoMatrix -> ProtoprotoMatrix -> Typung_mnw -> Matrix
> ms p1 p2 (m,n,w)
>   | m==1      = [zipWith (+) (m1!!0) (m2!!0)]
>   | otherwise = zipWith (+) (m1!!0) (m2!!0) : ms next1 next2 (m-1,n,w)
>   where m1 = anp2 p1 m n w
>         m2 = anp2 p2 m n w
>         next1 = drop 1 m1
>         next2 = drop 1 m2

-- 4.4 --

em:
Returns the identity matrix with m rows and columns.

> em :: SpaZei -> Matrix
> em 1 = [[1]]
> em m = anp2 (em (m-1) ++ [cur ++ [1]]) m m 0
>   where cur = getN [] (m-1) 0

mp:
Returns mx to the power of n.
mx is generated from p and (m,w), where p are some initial values, m is the
number of rows and columns of mx and w is the value to use if a field of the
matrix is unknown.

> mp :: ProtoprotoMatrix -> Typung_mw -> Potenz -> Matrix
> mp _ (m,_) 0 = em m
> mp p (m,w) n
>   | n<0       = error "unzulaessig"
>   | otherwise = mm mx (mp mx (m,w) (n-1)) (m,m,m,w)
>   where mx = anp2 p m m w
