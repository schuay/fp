-- 1

{-|ctxlines:
 - unlines always appends a line break after the last line
 -}
ctxlines   = [ "bla", "\ntest", "viele\nzauber\nsachen" ]

{-|ctxunlines:
 - unlines concatenates the lines, but ignores line breaks in these lines
 - lines, which is calles afterwards doesn't
 -}
ctxunlines = [ ["bla\n"], ["\ntest"], ["viele", "", "zauber\nsachen"] ]

{-|ctxwords:
 - words removes the additional spaces, so unwords can't restore them
 -}
ctxwords   = [ "bla ", " test", "viele zauber  sachen" ]

{-|ctxunwords:
 - unwords correctly assembles the words and retains the spaces, but words
 - removes them
 -}
ctxunwords = [ ["bla", ""], ["", "test"], ["viele", "zauber", "", "sachen"] ]

-- 2

{-|found:
 - Return True if p is found in s, false otherwise.
 - An empty string is always found.
 -}
found :: String -> String -> Bool
found s p
  | (length s) < (length p) = False
  | take (length p) s == p  = True
  | otherwise               = found (tail s) p

{-|unixtac:
 - Reverse lines of s.
 -}
unixtac :: String -> String
unixtac s = (unlines.reverse.lines) s

{-|unixhead:
 - Print the first n lines of s.
 -}
unixhead :: Int -> String -> String
unixhead n s = unlines (take n (lines s))

{-|unixtail:
 - Print the last n lines of s.
 -}
unixtail :: Int -> String -> String
unixtail n s = unlines ((reverse.(take n).reverse) (lines s))

{-|unixgrep:
 - Print all lines in s containing p.
 -}
unixgrep :: String -> String -> String
unixgrep p s = unlines [ x | x<-(lines s), (found x p) ]

-- 3

{-|aslines:
 - Split s into lines, apply f to the list of lines and put the result back in
 - one string.
 -}
aslines :: ([String]->[String]) -> String -> String
aslines f s = (unlines.f.lines) s

{-|unixtac':
 - Like unixtac, but uses aslines.
 -}
unixtac' :: String -> String
unixtac' s = aslines reverse s

{-|unixhead':
 - Like unixhead, but uses aslines.
 -}
unixhead' :: Int -> String -> String
unixhead' n s = aslines (take n) s

{-|unixtail':
 - Like unixtail, but uses aslines.
 -}
unixtail' :: Int -> String -> String
unixtail' n s = aslines (reverse.(take n).reverse) s

{-|unixgrep':
 - Like unixgrep, but uses aslines.
 -}
unixgrep' :: String -> String -> String
unixgrep' p s = aslines match s
  where match :: [String] -> [String]
        match s = [ x | x<-s, (found x p) ]

-- 4

{-|unixrev:
 - Reverse the characters in each line in s.
 -}
unixrev :: String -> String
unixrev s = aslines (map reverse) s

{-|wordrev:
 - Reverse the words in each line in s.
 -}
wordrev :: String -> String
wordrev s = aslines (map (unwords.reverse.words)) s

-- 5

{-|unixwcw:
 - Count the number of words in s.
 -}
unixwcw :: String -> Int
unixwcw s = (length.words) s

{-|unixwc:
 - Count the number of lines, words and characters in s.
 -}
unixwc :: String -> (Int,Int,Int)
unixwc s = ((length.lines) s, unixwcw s, length s)
