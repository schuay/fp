import Data.List

{- 1
 - ctxlines contains strings which demonstrate that:
 - unlines . lines /= id
 - all of them have a last line that does not end in \n-}

ctxlines :: [String]
ctxlines = ["\\n", "a", "\n\n\n\n\\n"]

{- likewise for the opposite direction
 - all contain "\n" in a word-}

ctxunlines :: [[String]]
ctxunlines = [["\n"], ["", "\n"], ["", "", "\n"]]

{- and for words in both directions
 - extra spaces are not preserved in unwords.words
 - and words.unwords stumbles on words containing spaces -}

ctxwords :: [String]
ctxwords = ["a ", " a", "a b c "]

ctxunwords :: [[String]]
ctxunwords = [["a b"], ["a ", "b c", "x"], ["a b c"]]

{- 2
 - rows in reversed order -}

unixtac :: String -> String
unixtac = unlines . reverse . lines

{- first n rows -}

unixhead :: Int -> String -> String
unixhead n = unlines . (take n) . lines

{- last n rows -}

unixtail :: Int -> String -> String
unixtail n = unlines . reverse . take n . reverse . lines

{- all rows that contain needle as substring -}

unixgrep :: String -> String -> String
unixgrep needle = unlines . (filter $ isInfixOf needle) . lines

{- 3
 - same as 2) but using common function aslines -}

aslines :: ([String] -> [String]) -> String -> String
aslines f = unlines . f . lines 

unixtac' :: String -> String
unixtac' = aslines reverse

unixhead' :: Int -> String -> String
unixhead' n = aslines $ take n

unixtail' :: Int -> String -> String
unixtail' n = aslines $ reverse . take n . reverse

unixgrep' :: String -> String -> String
unixgrep' needle = aslines $ filter $ isInfixOf needle

{- 4
 - reverse each line by characters -}

aswords :: ([String] -> [String]) -> String -> String
aswords f = unwords . f . words 

unixrev :: String -> String
unixrev = aslines $ map reverse

{- reverse each line by words -}

wordrev :: String -> String
wordrev = aslines $ map $ aswords reverse

{- 5
 - get wordcount -}

unixwcw :: String -> Int
unixwcw = length . words

unixwcr :: String -> Int
unixwcr = length . lines

{- get (row, word, char) counts; every element of the string counts as a char -}

unixwc :: String -> (Int, Int, Int)
unixwc x = (unixwcr x, unixwcw x, length x)
