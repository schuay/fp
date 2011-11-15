-- 1

{-|pick:
 - Returns all elements of the list (second argument), which are
 - equal to the Integer (first argument).
 -}
pick :: (Eq a) => a -> [a] -> [a]
pick n l = filter ((==) n) l

-- 2

{-|pickAll:
 - Returns the elements of the second argument, which are also present
 - int the first.
 -}
pickAll :: (Eq a) => [a] -> [a] -> [a]
pickAll l1 l2 = filter (\x -> elem x l1) l2

-- 3

{-|variations:
 - Calculates the number of possibilities to take r (second argument)
 - elements from a set with n (first argument) elements (also regarding
 - the order in which they are taken).
 -}
variations :: Integer -> Integer -> Integer
variations n r
    | r < 0 || n < r = -1
    | otherwise = foldr (*) 1 [n,n - 1 .. lim]
    where lim = n - r + 1

-- 4

{-|numberOfOcc:
 - Counts the number of occurences of the symbol (first argument) in
 - the Text (second argument).
 -}
numberOfOcc :: (Eq a) => a -> [a] -> Int
numberOfOcc n = length . (pick n)

-- 5

{-|delOne:
 - Deletes the last occurence of every symbol in a Text.
 -}
delOne :: (Eq a) => [a] -> [a]
delOne [] = []
delOne (x:xs)
    | elem x xs = x : delOne xs
    | otherwise = delOne xs

{-|mostCommonSymbol:
 - Returns the most common symbol in a Text or calls 'error' if no
 - such symbol exists.
 -}
mostCommonSymbol :: (Eq a) => [a] -> a
mostCommonSymbol [] = error "kein Resultat"
mostCommonSymbol (x:[]) = x
mostCommonSymbol x = (mostCommonSymbol . delOne) x
