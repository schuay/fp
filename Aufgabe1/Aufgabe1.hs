pick :: Integer -> [Integer] -> [Integer]
pick n l = [ x | x <- l, x == n]

pickAll :: [Integer] -> [Integer] -> [Integer]
pickAll l1 l2 = [ x | x <-l2, elem x l1 ]

variations :: Integer -> Integer -> Integer
variations n r
         | r < 0 || n < r = -1
         | n == r + 1 = n
         | otherwise = n * variations (n - 1) r

type Symbol = Char
type Text = String
type NumberOf = Integer

numberOfOcc :: Symbol -> Text -> NumberOf
numberOfOcc _ [] = 0
numberOfOcc c (x:xs)
    | c == x = 1 + numberOfOcc c xs
    | otherwise = numberOfOcc c xs

{-
mostCommonSymbol :: Text -> Symbol
mostCommonSymbol [] = error "kein Resultat"
mostCommonSymbol xs = [ numberOfOcc x xs | x <- xs ]
-}
