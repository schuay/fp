-- 1

{-|pick:
 - Returns all elements of the list (second argument), which are
 - equal to the Integer (first argument).
 -}
pick :: Integer -> [Integer] -> [Integer]
pick n l = [ x | x <- l, x == n]

-- 2

{-|pickAll:
 - Returns the elements of the second argument, which are also present
 - int the first.
 -}
pickAll :: [Integer] -> [Integer] -> [Integer]
pickAll l1 l2 = [ x | x <-l2, elem x l1 ]

-- 3

{-|variations:
 - Calculates the number of possibilities to take r (second argument)
 - elements from a set with n (first argument) elements (also regarding
 - the order in which they are taken).
 -}
variations :: Integer -> Integer -> Integer
variations n r
         | r < 0 || n < r = -1
         | n == r + 1 = n
         | otherwise = n * variations (n - 1) r

-- 4

type Symbol = Char
type Text = String
type NumberOf = Integer

{-|numberOfOcc:
 - Counts the number of occurences of the symbol (first argument) in
 - the Text (second argument).
 -}
numberOfOcc :: Symbol -> Text -> NumberOf
numberOfOcc _ [] = 0
numberOfOcc c (x:xs)
    | c == x = 1 + numberOfOcc c xs
    | otherwise = numberOfOcc c xs

-- 5

{-|delOne:
 - Deletes the last occurence of every symbol in a Text.
 -}
delOne :: Text -> Text
delOne [] = []
delOne (x:xs)
     | elem x xs = x : delOne xs
     | otherwise = delOne xs

{-|mostCommonSymbol:
 - Returns the most common symbol in a Text or calls 'error' if no
 - such symbol exists.
 -}
mostCommonSymbol :: Text -> Symbol
mostCommonSymbol [] = error "kein Resultat"
mostCommonSymbol (x:[]) = x
mostCommonSymbol x = (mostCommonSymbol . delOne) x
