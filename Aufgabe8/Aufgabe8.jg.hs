import Data.List ((\\), nub, intersect, union, sort)
import Data.Maybe (fromMaybe)

type State = Integer
type StartState = State
type AcceptingStates = [State]
type Word a = [a]
type Row a = [[a]]
data AMgraph a = AMg [(Row a)] deriving (Eq,Show)
type Automaton a = AMgraph a
type Postfix a = Word a

{- Test automaton -}

auto = AMg [["dea","a","ca","ba","",""],["de","a","c","b","",""],
            ["de","a","x","","",""],["dey","a","","b","",""],
            ["","","","bz","",""],["","","","","",""]]

{- Test skyscraper lines -}

s1 = [1,30,10,20,1]
s2 = [1,20,10,2]
s3 = [1,50,30,10,40,20,2]

i1 = [1,1]
i2 = [2,20,10,2]
i3 = [1,15,10,2]
i4 = [1,30,10,2]
i5 = [2,50,30,10,40,20,2]

{- Return all edges entering state -}
enteringEdges :: Eq a => Automaton a -> State -> Row a
enteringEdges (AMg m) state = map (!! (fromIntegral state)) m

{- Return all edges leaving state -}
leavingEdges :: Eq a => Automaton a -> State -> Row a
leavingEdges (AMg m) state = m !! (fromIntegral state)

{- Returns all backwards steps (next index, edge label) possible from
 - current state -}
allSteps :: Eq a => Automaton a -> State -> [(State,a)]
allSteps graph state = singleEdgesLst
    where
    -- [(0,""), (1,"ab"), ...]
    indexEdgesLst = zip [0..] (enteringEdges graph state)
    -- [(1,"ab"), ...]
    filteredIndexEdgesLst =filter (\(ind,edges) -> length edges > 0) indexEdgesLst
    -- (1,"ab") -> [(1,a),(1,b)]
    splitIndexEdgesTuple (ind,edges) = [ (ind,edge) | edge <- edges]
    -- [(1,a),(1,b),...]
    singleEdgesLst = concat $ map splitIndexEdgesTuple filteredIndexEdgesLst

{- Return all next states reachable from state -}
nextStates :: Eq a => Automaton a -> State -> [State]
nextStates graph state = [ index | (index,edges) <- indexedList, length edges > 0 ]
    where indexedList = zip [0..] (leavingEdges graph state)

{- Returns the first word accepted by graph its prefix matching pattern and
 - maximum length len or Nothing if none matched. -}
allWords :: Eq a => Automaton a -> StartState -> AcceptingStates -> Integer
                                -> Postfix a -> Maybe (Word a)
allWords graph start accpt len pattern
    | words == [] = Nothing
    | otherwise = Just (head words)
    where words = allWords' graph start accpt len [] pattern

allWords' :: Eq a => Automaton a -> StartState -> AcceptingStates -> Integer
                                 -> Word a -> Postfix a -> [Word a]
-- max word length reached, terminate
allWords' _ start accpt 0 word _ = if start `elem` accpt then [word] else []
allWords' graph start accpt len word pattern
-- pattern matched and reached endstate, terminate
    | start `elem` accpt && pattern == [] = [word]
    | otherwise = concat [ allWords' graph state accpt (len - 1)
                           (char:word) (tailOrEmpty pattern)
                         | (state,char) <- allSteps graph start,
                           equalFirstIfExists char pattern ]
    where tailOrEmpty [] = []
          tailOrEmpty xs = tail xs
          equalFirstIfExists _ [] = True
          equalFirstIfExists a xs = a == head xs

{- Returns all reachable states when starting from state -}

reachableStates :: Eq a => (Automaton a) -> [State] -> State -> [State]
reachableStates graph visited state
    | length next == 0 = visited
    | otherwise = nub $ concat retList
    where next = nextStates graph state \\ visitedPlusCurrent
          visitedPlusCurrent = visited `union` [state]
          nextVisited = visitedPlusCurrent ++ next
          retList = map (reachableStates graph nextVisited) next

{- Returns possible words which end in postfix and start from state (at most
 - one per reachable accepting node) -}

postFixCandidates :: Eq a => AMgraph a -> State -> [State] -> [a] -> [Word a]
postFixCandidates graph@(AMg m) start accpt postfix
    = map (\(Just a) -> a) $ filter (/= Nothing) $ map
      (\x -> allWords graph x [start] maxLen (reverse postfix)) endStateCandidates
    -- maximum length of words we need to construct
    where maxLen = fromIntegral $ (length m) + (length postfix)
          endStateCandidates = accpt `intersect` (reachableStates graph [] start)

{- 1 -}

{- Returns true if postfix is a possible postfix of a word when starting
 - from state -}

isPostfix :: Eq a => (Automaton a) -> StartState -> AcceptingStates
                                   -> (Postfix a) -> Bool
isPostfix graph@(AMg m) start accpt postfix
    = length (postFixCandidates graph start accpt postfix) > 0

type Prefix a = Word a

{- 2 -}

{- Returns the prefix of any word starting at state and ending in an
 - accepted state such that prefix ++ postfix == word -}

givePrefix :: Eq a => (Automaton a) -> StartState -> AcceptingStates
                                    -> (Postfix a) -> (Maybe (Prefix a))
givePrefix graph start accpt postfix
    | candidates == [] = Nothing
    | otherwise = Just $ prefix $ head candidates
    where candidates = postFixCandidates graph start accpt postfix
          prefix w = take ((length w) - (length postfix)) w

{- 3 -}

type Skyscraperline = [Integer]
type Length = Integer
type VisFromLeft = Integer
type VisFromRight = Integer

{- Strips visibility fields -} 

getSkyscrapers :: Skyscraperline -> Skyscraperline
getSkyscrapers = init . tail

{- Returns true if all heights are valid and present -}

heightsValid :: Skyscraperline -> Bool
heightsValid s = sort (getSkyscrapers s) == sortedLine
    where len = (fromIntegral $ length $ getSkyscrapers s)
          sortedLine = map (*10) [1..len]

{- isValid takes a line (including visibility information) and checks if
 - 1) it contains at least 1 skyscraper, 2) all heights are valid and present,
 - and 3) if the given visibility information is correct. -}

isValid :: Skyscraperline -> Bool
isValid s = length s > 2 && heightsValid s &&
            s == (computeVisibility $ getSkyscrapers s)

{- computeVisFrom{Left,Right} and computeVisibility expect
 - list *without* visibility information -}

computeVisFromLeft :: Skyscraperline -> VisFromLeft
computeVisFromLeft s = 1 + (fromIntegral $ length $ takeWhile id $
                       zipWith (<) s (tail s))

computeVisFromRight :: Skyscraperline -> VisFromRight
computeVisFromRight = computeVisFromLeft . reverse

{- computeVisibility takes a (nonempty and valid) line without visibility
 - information and returns a an equivalent line with added visibility fields -}

computeVisibility :: Skyscraperline -> Skyscraperline
computeVisibility s = [computeVisFromLeft s] ++ s ++ [computeVisFromRight s]

{- Generate all skyscraper line permutations of length l -}

scraperPerms :: Integer -> [Skyscraperline]
scraperPerms l = scraperPerms' [] (map (10*) [1..l])
    where
    scraperPerms' sequ (x:[]) = [sequ ++ [x]]
    scraperPerms' sequ vals = concat [ scraperPerms' (sequ ++ [x])
                                       (filter (/= x) vals) | x <- vals]

{- Filters all possible permutations of length len to fit the given conditions -}

allSkyscraperLines :: Length -> VisFromLeft -> VisFromRight -> [Skyscraperline]
allSkyscraperLines len l r = filter (\x -> head x == l &&  last x == r) $
                             map computeVisibility $ scraperPerms len

{- Returns first line of allSkyscraperLines if it exists, otherwise Nothing -}

buildSkyscrapers :: Length -> VisFromLeft -> VisFromRight -> Maybe Skyscraperline
buildSkyscrapers len l r
    | len < 1 || l + r > len = Nothing
    | length candidates == 0 = Nothing
    | otherwise = Just $ head candidates 
    where candidates = allSkyscraperLines len l r

{- Returns the number of lines in allSkyscraperLines -}

noOfSkyscraperLines :: Length -> VisFromLeft -> VisFromRight -> Integer
noOfSkyscraperLines len l r = toInteger $ length $ allSkyscraperLines len l r
