import Data.List (sort, (\\), nub, union)
import Data.Maybe (listToMaybe, isNothing)
import Test.HUnit

{- Test variables -}

skyline1_ok = [[0,3,1,2,2,3,0],[2,30,50,40,20,10,4],[3,10,30,20,50,40,2],[2,20,10,50,40,30,3],[2,40,20,30,10,50,1],[1,50,40,10,30,20,4],[0,1,2,3,3,2,0]]
skyline1_ok_start = [[0,3,1,2,2,3,0],[2,0,0,0,0,0,4],[3,0,0,0,0,0,2],[2,0,0,0,0,0,3],[2,0,0,0,0,0,1],[1,0,0,0,0,0,4],[0,1,2,3,3,2,0]]
skyline1_ok_novis = [[30,50,40,20,10],[10,30,20,50,40],[20,10,50,40,30],[40,20,30,10,50],[50,40,10,30,20]]

skyline2_ok = [[0,2,2,3,1,3,0],[2,40,20,10,50,30,2],[1,50,10,40,30,20,4],[2,30,50,20,40,10,3],[3,10,30,50,20,40,2],[3,20,40,30,10,50,1],[0,3,2,2,4,1,0]]
skyline2_ok_start = [[0,2,2,3,1,3,0],[2,0,0,0,0,0,2],[1,0,0,0,0,0,4],[2,0,0,0,0,0,3],[3,0,0,0,0,0,2],[3,0,0,0,0,0,1],[0,3,2,2,4,1,0]]
skyline2_ok_novis = [[40,20,10,50,30],[50,10,40,30,20],[30,50,20,40,10],[10,30,50,20,40],[20,40,30,10,50]]

skyline3_nok_start = [[0,3,1,2,2,3,0],[2,0,0,0,0,0,4],[3,0,0,0,0,0,2],[2,0,0,0,0,0,3],[2,0,0,0,0,0,2],[1,0,0,0,0,0,4],[0,1,2,3,3,2,0]]
skyline4_nok_start = [[0,2,2,3,1,3,0],[2,0,0,0,0,0,2],[1,0,0,0,0,0,4],[2,0,0,0,0,0,3],[3,0,0,0,0,0,2],[3,0,0,0,0,0,1],[0,3,2,2,4,2,0]]
skyline5_nok_start = [[0,2,2,3,1,3,0],[2,0,0,0,0,0,2],[6,0,0,0,0,0,4],[2,0,0,0,0,0,3],[3,0,0,0,0,0,2],[3,0,0,0,0,0,1],[0,3,2,2,4,2,0]]

skyline6_nok = [[0,2,2,3,1,3,0],[2,40,20,10,50,30,3],[1,50,10,40,30,20,4],[2,30,50,20,40,10,3],[3,10,30,50,20,40,2],[3,20,40,30,10,50,1],[0,3,2,2,4,1,0]]
skyline7_nok = [[0,2,2,3,1,3,0],[2,40,20,10,50,30,2],[1,50,10,40,30,20,4],[2,30,50,20,40,10,6],[3,10,30,50,20,40,2],[3,20,40,30,10,50,1],[0,3,2,2,4,1,0]]
skyline8_nok = [[0,2,2,3,1,3,0],[2,40,20,10,50,30,2],[1,50,10,40,30,20,4],[2,30,50,20,40,10,3],[3,10,30,50,20,60,2],[3,20,40,30,10,50,1],[0,3,2,2,4,1,0]]
skyline9_nok = [[0,2,2,3,1,3,0],[2,40,20,10,0,30,2],[1,50,10,40,30,20,4],[2,30,50,20,40,10,3],[3,10,30,50,20,-10,2],[3,20,40,30,10,50,1],[0,3,2,2,4,1,0]]

testsV = [skyline1_ok, skyline2_ok, skyline6_nok, skyline7_nok, skyline8_nok, skyline9_nok]
testsB = [skyline1_ok_start, skyline2_ok_start, skyline3_nok_start, skyline4_nok_start, skyline5_nok_start]

basic_start1 = [[9,1,6,0,0,4,0,7,2],
                [8,0,0,6,2,0,0,5,0],
                [5,0,0,0,0,8,9,3,0],
                [0,6,0,0,0,0,2,0,0],
                [0,0,0,2,0,7,0,0,0],
                [0,0,5,0,0,0,0,9,0],
                [0,9,7,8,0,0,0,0,3],
                [0,8,0,0,7,6,0,0,9],
                [4,5,0,1,0,0,6,8,7]]

basic_start2 = [[6,0,0,3,0,0,1,0,0],
                [0,7,1,6,2,0,0,0,0],
                [8,0,5,0,0,1,0,0,0],
                [5,0,0,8,7,0,9,0,1],
                [0,0,9,0,0,0,6,0,0],
                [4,0,7,0,6,9,0,0,8],
                [0,0,0,2,0,0,8,0,7],
                [0,0,0,0,8,6,4,1,0],
                [0,0,8,0,0,3,0,0,2]]


basic_start3 = [[9,0,6,0,1,3,0,0,8],
                [0,5,8,0,0,0,0,9,0],
                [0,3,0,0,0,0,0,1,0],
                [0,6,0,8,0,0,9,2,0],
                [0,0,3,4,0,9,1,0,0],
                [0,4,9,0,0,6,0,3,0],
                [0,9,0,0,0,0,0,8,0],
                [0,1,0,0,0,0,6,7,0],
                [4,0,0,9,6,0,3,0,1]]


basic_inv1 =   [[9,0,6,0,1,3,0,0,8],
                [0,5,8,0,0,0,0,9,0],
                [0,3,0,0,0,0,0,1,0],
                [0,6,0,8,0,0,9,2,0],
                [0,0,3,4,0,9,9,0,0],
                [0,4,9,0,0,6,0,3,0],
                [0,9,0,0,0,0,0,8,0],
                [0,1,0,0,0,0,6,7,0],
                [4,0,0,9,6,0,3,0,1]]

basic_inv2 =   [[9,0,6,0,1,3,0,0,8],
                [1,5,8,0,0,0,0,9,0],
                [0,3,1,0,0,0,0,1,0],
                [0,6,0,8,0,0,9,2,0],
                [0,0,3,4,0,9,1,0,0],
                [0,4,9,0,0,6,0,3,0],
                [0,9,0,0,0,0,0,8,0],
                [0,1,0,0,0,0,6,7,0],
                [4,0,0,9,6,0,3,0,1]]

-- cross start sudokus

cross_start1 =  [[0,0,3,4,0,0,0,8,0], 
                 [4,5,6,7,8,9,1,2,0],
                 [7,8,0,1,2,3,0,5,0], 
                 [0,6,4,2,1,7,9,3,0], 
                 [2,9,1,5,3,8,0,7,4], 
                 [3,0,8,6,9,4,0,0,0], 
                 [0,3,5,9,0,2,0,4,1], 
                 [0,1,7,8,0,0,0,6,2], 
                 [0,0,2,0,6,1,0,0,0]]

cross_start2 =  [[0,0,3,4,0,0,0,0,0], 
                 [4,5,0,7,8,9,1,0,0],
                 [7,8,0,1,2,3,0,5,0], 
                 [0,6,4,2,1,7,9,0,0], 
                 [2,9,1,5,0,0,0,7,4], 
                 [0,0,8,6,9,4,0,0,0], 
                 [0,0,5,9,0,2,0,4,1], 
                 [0,0,7,8,0,0,0,6,2], 
                 [0,0,2,0,0,1,0,0,0]]


cross_inv1 =    [[0,0,3,4,0,0,0,0,0], 
                 [4,5,0,7,8,9,1,0,0],
                 [7,8,0,1,2,3,0,5,0], 
                 [0,6,4,2,1,7,9,0,0], 
                 [2,9,1,5,0,0,0,7,4], 
                 [0,0,8,6,9,4,0,0,0], 
                 [0,0,5,9,0,2,0,4,1], 
                 [0,0,7,8,0,0,0,6,2], 
                 [0,0,2,0,0,1,0,0,6]]

-- color start sudokus

color_start1 = [[1,2,3,0,5,0,0,0,9], 
                [0,0,0,0,0,0,0,0,3], 
                [7,0,9,0,2,3,4,5,6], 
                [2,0,1,0,0,4,8,9,5],
                [8,0,5,9,1,2,0,0,0], 
                [0,0,0,5,3,8,2,7,1], 
                [0,0,0,3,6,7,5,4,8], 
                [5,4,8,2,9,0,0,0,0], 
                [0,6,7,8,4,0,0,0,0]]

color_start2 = [[0,0,3,4,5,0,0,0,0], 
                [0,0,6,0,0,0,1,2,3], 
                [0,8,9,1,2,3,4,5,6], 
                [0,3,1,6,7,4,8,9,5],
                [0,7,5,0,0,0,3,0,0], 
                [6,9,4,5,3,0,2,0,0], 
                [9,1,2,3,0,0,0,0,0], 
                [0,0,0,2,9,0,6,0,0], 
                [0,0,0,8,4,0,0,0,0]]

color_inv1   = [[0,0,3,4,5,0,0,0,0], 
                [0,0,6,0,0,6,1,2,3], 
                [0,8,9,1,2,3,4,5,6], 
                [0,3,1,6,7,4,8,9,5],
                [0,7,5,0,0,0,3,0,0], 
                [6,9,4,5,3,0,2,0,0], 
                [9,1,2,3,0,0,0,0,0], 
                [0,0,0,2,9,0,6,0,0], 
                [0,0,0,8,4,0,0,0,0]]

compareSkyLine s1 s2 = zipWith id (conv s1) s1 == zipWith id (conv s2) s2 where conv m = [init . tail] ++ (replicate (length m - 2) id) ++ [init . tail]

{- Test cases -}

tests = test [ "1" ~: True ~=? isValidSL [[0,5,4,3,2,1,0],[5,10,20,30,40,50,1],[4,20,30,40,50,10,2],[3,30,40,50,10,20,2],[2,40,50,10,20,30,2],[1,50,10,20,30,40,2],[0,1,2,2,2,2,0]],
               "2" ~: True ~=? isValidSL [[1,5,4,3,2,1,0],[5,10,20,30,40,50,1],[4,20,30,40,50,10,2],[3,30,40,50,10,20,2],[2,40,50,10,20,30,2],[1,50,10,20,30,40,2],[0,1,2,2,2,2,0]],
               "3" ~: False ~=? isValidSL [[0,6,4,3,2,1,0],[5,10,20,30,40,50,1],[4,20,30,40,50,10,2],[3,30,40,50,10,20,2],[2,40,50,10,20,30,2],[1,50,10,20,30,40,2],[0,1,2,2,2,2,0]],
               "4" ~: False ~=? isValidSL [[0,5,4,3,2,1,0],[5,10,20,30,40,50,1],[4,20,30,40,50,10,2],[3,30,40,50,20,20,2],[2,40,50,10,20,30,2],[1,50,10,20,30,40,2],[0,1,2,2,2,2,0]],
               "5" ~: False ~=? isValidSL [[0,5,4,3,2,1,0],[5,10,20,30,40,50,1],[4,20,30,50,10,2],[3,30,40,50,10,20,2],[2,40,50,10,20,30,2],[1,50,10,20,30,40,2],[0,1,2,2,2,2,0]],
               "6" ~: False ~=? isValidSL [],
               "7" ~: True ~=? compareSkyLine (compVisibility [[10,20,30,40,50],[20,30,40,50,10],[30,40,50,10,20],[40,50,10,20,30],[50,10,20,30,40]]) ([[0,5,4,3,2,1,0],[5,10,20,30,40,50,1],[4,20,30,40,50,10,2],[3,30,40,50,10,20,2],[2,40,50,10,20,30,2],[1,50,10,20,30,40,2],[0,1,2,2,2,2,0]]),
               "8" ~: True ~=? compareSkyLine (compVisibility [[10,20,30,40,50],[20,30,40,50,10],[30,40,50,10,20],[40,50,10,20,30],[50,10,20,30,40]]) ([[1,5,4,3,2,1,2],[5,10,20,30,40,50,1],[4,20,30,40,50,10,2],[3,30,40,50,10,20,2],[2,40,50,10,20,30,2],[1,50,10,20,30,40,2],[3,1,2,2,2,2,4]]),
               "9" ~: False ~=? compareSkyLine (compVisibility [[10,20,30,40,50],[20,30,40,50,10],[30,40,50,10,20],[40,50,10,20,30],[50,10,20,30,40]]) ([[1,5,4,3,2,1,2],[5,10,20,30,40,50,1],[4,20,30,40,50,10,2],[3,30,40,50,10,20,2],[2,40,50,10,20,30,2],[1,50,10,20,30,40,2],[3,1,2,2,3,2,4]]),
               "10" ~: False ~=? compareSkyLine (compVisibility [[10,20,30,40,50],[20,30,40,50,10],[30,40,50,10,20],[40,50,10,20,30],[50,10,20,30,40]]) ([[1,5,4,3,2,1,2],[5,10,20,30,40,50,1],[4,20,30,40,50,10,2],[3,30,40,50,10,20,2],[2,40,1337,10,20,30,2],[1,50,10,20,30,40,2],[3,1,2,2,2,2,4]]),
               "11" ~: False ~=? compareSkyLine (compVisibility [[10,20,30,40,50],[20,30,40,50,10],[30,40,50,10,20],[40,50,10,20,30],[50,10,20,30,40]]) ([[5,10,20,30,40,50,1],[4,20,30,40,50,10,2],[3,31,40,50,10,20,2],[2,40,50,10,20,30,2],[1,50,10,20,30,40,2]]),
               "7" ~: Just [[0,2,1,0],[2,10,20,1],[1,20,10,2],[0,1,2,0]] ~=? buildSkyscrapers [[0,2,1,0],[2,0,0,1],[1,0,0,2],[0,1,2,0]],
               "7" ~: Nothing ~=? buildSkyscrapers [[0,2,2,0],[2,0,0,1],[1,0,0,2],[0,1,2,0]],
               "7" ~: Just [[0,5,4,3,2,1,0],[5,10,20,30,40,50,1],[4,20,30,40,50,10,2],[3,30,40,50,10,20,2],[2,40,50,10,20,30,2],[1,50,10,20,30,40,2],[0,1,2,2,2,2,0]] ~=? buildSkyscrapers [[0,5,4,3,2,1,0],[5,0,0,0,0,0,1],[4,0,0,0,0,0,2],[3,0,0,0,0,0,2],[2,0,0,0,0,0,2],[1,0,0,0,0,0,2],[0,1,2,2,2,2,0]],
               "7" ~: Nothing ~=? buildSkyscrapers [[0,6,4,3,2,1,0],[5,0,0,0,0,0,1],[4,0,0,0,0,0,2],[3,0,0,0,0,0,2],[2,0,0,0,0,0,2],[1,0,0,0,0,0,2],[0,1,2,2,2,2,0]],
               "7" ~: Nothing ~=? buildSkyscrapers [[0,5,4,3,2,1,0],[5,0,0,0,0,0,1],[4,0,0,0,0,0,2],[3,0,0,0,0,0,2],[2,0,0,0,0,0,2],[1,0,0,0,0,0,2],[0,1,2,2,2,1,0]],
               "7" ~: Just [[0,5,1,3,2,4,0],[2,10,50,30,40,20,3],[3,20,10,40,50,30,2],[2,30,20,50,10,40,2],[2,40,30,10,20,50,1],[1,50,40,20,30,10,4],[0,1,2,2,2,2,0]] ~=? buildSkyscrapers [[0,5,1,3,2,4,0],[2,0,0,0,0,0,3],[3,0,0,0,0,0,2],[2,0,0,0,0,0,2],[2,0,0,0,0,0,1],[1,0,0,0,0,0,4],[0,1,2,2,2,2,0]],
               "7" ~: [True, True, False, False, False, False] ~=? (map (isValidSL) testsV),
               "7" ~: True ~=? compareSkyLine (compVisibility skyline1_ok_novis) (skyline1_ok),
               "7" ~: True ~=? compareSkyLine (compVisibility skyline2_ok_novis) (skyline2_ok),
               "7" ~: [(Just skyline1_ok), (Just skyline2_ok), Nothing, Nothing, Nothing] ~=? map (buildSkyscrapers) testsB,
               "7" ~: True ~=? isValidSDK basic_start1 Basic,
               "7" ~: True ~=? isValidSDK basic_start2 Basic,
               "7" ~: True ~=? isValidSDK basic_start3 Basic,
               "7" ~: True ~=? isValidSDK cross_start1 Cross,
               "7" ~: True ~=? isValidSDK cross_start2 Cross,
               "7" ~: True ~=? isValidSDK color_start1 Color,
               "7" ~: True ~=? isValidSDK color_start2 Color,
               "7" ~: False ~=? isValidSDK basic_inv1 Basic,
               "7" ~: False ~=? isValidSDK basic_inv2 Basic,
               "7" ~: False ~=? isValidSDK cross_inv1 Cross,
               "7" ~: False ~=? isValidSDK color_inv1 Color,
               "7" ~: Just [[9,1,6,3,5,4,8,7,2],[8,7,3,6,2,9,1,5,4],[5,2,4,7,1,8,9,3,6],[7,6,8,9,3,5,2,4,1],[1,4,9,2,8,7,3,6,5],[2,3,5,4,6,1,7,9,8],[6,9,7,8,4,2,5,1,3],[3,8,1,5,7,6,4,2,9],[4,5,2,1,9,3,6,8,7]] ~=? solve basic_start1 Basic,
               "7" ~: Just [[6,2,4,3,5,7,1,8,9],[9,7,1,6,2,8,3,5,4],[8,3,5,4,9,1,7,2,6],[5,6,3,8,7,2,9,4,1],[2,8,9,1,3,4,6,7,5],[4,1,7,5,6,9,2,3,8],[3,4,6,2,1,5,8,9,7],[7,5,2,9,8,6,4,1,3],[1,9,8,7,4,3,5,6,2]] ~=? solve basic_start2 Basic,
               "7" ~: Just [[9,7,6,5,1,3,2,4,8],[1,5,8,6,4,2,7,9,3],[2,3,4,7,9,8,5,1,6],[7,6,1,8,3,5,9,2,4],[8,2,3,4,7,9,1,6,5],[5,4,9,1,2,6,8,3,7],[6,9,7,3,5,1,4,8,2],[3,1,5,2,8,4,6,7,9],[4,8,2,9,6,7,3,5,1]] ~=? solve basic_start3 Basic,
               "7" ~: Nothing ~=? solve basic_inv1 Basic,
               "7" ~: Nothing ~=? solve basic_inv2 Basic,
               "7" ~: Just [[1,2,3,4,5,6,7,8,9],[4,5,6,7,8,9,1,2,3],[7,8,9,1,2,3,4,5,6],[5,6,4,2,1,7,9,3,8],[2,9,1,5,3,8,6,7,4],[3,7,8,6,9,4,2,1,5],[6,3,5,9,7,2,8,4,1],[9,1,7,8,4,5,3,6,2],[8,4,2,3,6,1,5,9,7]] ~=? solve cross_start1 Cross,
               "7" ~: Just [[1,2,3,4,5,6,7,8,9],[4,5,6,7,8,9,1,2,3],[7,8,9,1,2,3,4,5,6],[5,6,4,2,1,7,9,3,8],[2,9,1,5,3,8,6,7,4],[3,7,8,6,9,4,2,1,5],[6,3,5,9,7,2,8,4,1],[9,1,7,8,4,5,3,6,2],[8,4,2,3,6,1,5,9,7]] ~=? solve cross_start2 Cross,
               "7" ~: Nothing ~=? solve cross_inv1 Cross,
               "7" ~: Just [[1,2,3,4,5,6,7,8,9],[4,5,6,7,8,9,1,2,3],[7,8,9,1,2,3,4,5,6],[2,3,1,6,7,4,8,9,5],[8,7,5,9,1,2,3,6,4],[6,9,4,5,3,8,2,7,1],[9,1,2,3,6,7,5,4,8],[5,4,8,2,9,1,6,3,7],[3,6,7,8,4,5,9,1,2]] ~=? solve color_start1 Color,
               "7" ~: Just [[1,2,3,4,5,6,7,8,9],[4,5,6,7,8,9,1,2,3],[7,8,9,1,2,3,4,5,6],[2,3,1,6,7,4,8,9,5],[8,7,5,9,1,2,3,6,4],[6,9,4,5,3,8,2,7,1],[9,1,2,3,6,7,5,4,8],[5,4,8,2,9,1,6,3,7],[3,6,7,8,4,5,9,1,2]] ~=? solve color_start2 Color,
               "7" ~: Nothing ~=? solve color_inv1 Color
             ]

{- Terms: bare skyline = skyline without visibility infos -}

sl0 = [[0,2,1,0],[2,0,0,1],[1,0,0,2],[0,1,2,0]]
sl1 = compVisibility [[10,20,30,40],[20,30,40,10],[30,40,10,20],[40,10,20,30]]
sl2 = [[0,5,4,3,2,1,0],[5,0,0,0,0,0,1],[4,0,0,0,0,0,2],[3,0,0,0,0,0,2],[2,0,0,0,0,0,2],[1,0,0,0,0,0,2],[0,1,2,2,2,2,0]]
sl3 = [[0,2,2,3,1,3,0],[2,0,0,0,0,0,2],[1,0,0,0,0,0,4],[2,0,0,0,0,0,3],[3,0,0,0,0,0,2],[3,0,0,0,0,0,1],[0,3,2,2,4,1,0]]

{- skyline -}

type Row = [Integer]
type Col = [Integer]
type Skyline = [Row]

{- 1a -}

row :: [a] -> Int -> a
row m i = m !! i

col :: [[a]] -> Int -> [a]
col m i = map (!! i) m

cell :: [[a]] -> Int -> Int -> a
cell m i j = row (col m j) i

{- Given a Skyline, returns a list of all rows -}

allRows :: Skyline -> [Row]
allRows = id

{- Given a Skyline, returns a list of all cols -}

allCols :: Skyline -> [Col]
allCols m = map (col m) [ 0 .. (length m) - 1 ]

{- Returns (0,0) if matrix is empty or invalid; (rows,cols) otherwise -}

dim :: [[a]] -> (Int, Int)
dim m | rows == 0 || cols == 0 = (0,0)
      | not colsequal = (0,0)
      | otherwise = (rows, cols)
    where rows = length m
          collens = map length m
          cols = head collens
          colsequal = and $ zipWith (==) collens (tail collens)

mid :: [a] -> [a]
mid = tail . init

{- Given a Skyline, returns a bare skyline -}

stripVisibility :: [[a]] -> [[a]]
stripVisibility = (map mid) . mid

{- Takes a skyline, returns whether the size is valid. -}

isValidSLSize :: Skyline -> Bool
isValidSLSize m = a > 2 && b > 2
    where (a,b) = dim m

{- Takes a bare skyline row of valid size,
 - returns whether it contains all values. -}

isValidSLLine :: Row -> Bool
isValidSLLine r = and $ zipWith (==) values (sort r)
    where values = map (* 10) [ 1 .. ]

{- Takes a bare skyline, returns whether all rows and cols
 - contain all values -}

allLinesValid :: Skyline -> Bool
allLinesValid m = and $ map isValidSLLine allLines
    where allLines = (allRows m) ++ (allCols m)

{- lvis,rvis,visline expect bare skyline rows -}

lvis :: Row -> Integer
lvis s = fromIntegral $ lvis' s 0
    where lvis' (x:xs) mx = lvis' xs (max x mx) +  if x > mx then 1 else 0
          lvis' _ _ = 0

rvis :: Row -> Integer
rvis = lvis . reverse

{- visline takes a (nonempty and valid) bare skyline row
 - and returns a an equivalent line with added visibility fields -}

visline :: Row -> Row
visline s = [lvis s] ++ s ++ [rvis s]

{- Transposes a matrix -}

transpose :: [[a]] -> [[a]]
transpose m = [ col m i | i <- [ 0 .. s ] ]
    where (rows,cols) = dim m
          s = cols - 1

{- standardize sets corner cells to 0 -}

standardize :: Skyline -> Skyline
standardize m = [ standardize' $ head m ] ++ (mid m) ++
                [ standardize' $ last m ]
    where standardize' r = [ 0 ] ++ (mid r) ++ [ 0 ]

{- Checks if the given skyline is an (n+2)x(n+2) matrix,
 - each row and column contains all values [ 10 .. 10 * n ],
 - and all visibility infos are correct. Returns true if all conditions
 - are satisfied, else false. -}

isValidSL :: Skyline -> Bool
isValidSL m = and [ isValidSLSize m,
                    allLinesValid s,
                    (standardize m) == (compVisibility s) ]
    where s = stripVisibility m

{- 1b -}

initialRow :: Integer -> Row
initialRow n = map (* 10) [1 .. n]

perms :: Row -> [Row]
perms [] = [[]]
perms r = [ x:xs | x <- r, xs <- perms (r \\ [x])]

{- Takes 2 rows and returns whether all nonzero positions match.
 - rowsMatch [10,20,40,30] [0,20,0,0] -> True -}

rowsMatch :: Row -> Row -> Bool
rowsMatch x y = length x == length y &&
                and [ a == b | (a,b) <- zip x y, a /= 0, b /= 0 ]

{- Takes a template row,
 - and returns a list of valid candidate rows.
 - candidateRows [3,10,0,0,1] -> [[3,10,20,30,1]]  -}
candidateRows :: Row -> [Row]
candidateRows t = filter (rowsMatch t) (map visline rows)
    where rows = perms (initialRow len)
          len = fromIntegral $ length $ mid t

replace :: [a] -> Int -> a -> [a]
replace m i r = (take i m) ++ [r] ++ (drop (i + 1) m)

candidates :: Skyline -> [Skyline]
candidates m = filter isValidSL cs
    where
    cs = map (\x -> [head m] ++ x ++ [last m]) (candidates' m 1)

dupes :: Eq a => [a] -> Bool
dupes [] = False
dupes (x:xs) = x `elem` xs || dupes xs

{- Takes a template matrix and a row index. Returns a list of candidate
 - matrices which fulfill the horizontal visibility requirements.
 - NOTE: vertical visibility requirements are NOT checked, and the
 - returned matrices are missing the vertical visibility rows! -}

candidates' :: Skyline -> Int -> [Skyline]
candidates' m i
    | i > len = [[]]
    | otherwise = [ r:s | r <- candidateRows t, s <- next r ]
    where
    {- Last row is visibility info, and we are only interested in
     - skyline rows -}
    len = length m - 2
    t = row m i

    {- Returns all possible combinations of next lines. Terminates if m is invalid -}
    next r = if not (mvalid m) then [] else candidates' (replace m i r) (i + 1)

    {- Returns whether columns of stripped partial matrix contain no duplicates -}
    mvalid = (all nodupes) . mid . transpose . (take i) . mid

    {- Takes a row and returns True if it contains no duplicate items -}
    nodupes = not . dupes

{- Given a skyline *without* visibility information (an n x n matrix),
 - calculates the visibility and returns the skyline *with* visibility
 - infos. If the given skyline is not valid, it is returned without change. -}

compVisibility :: Skyline -> Skyline
compVisibility m
    | a < 2 || b < 2 || (not $ allLinesValid m) = m
    | otherwise = standardize $ map visline colvis
    where colvis = (transpose . map visline . transpose) m
          (a,b) = dim m

{- 1c -}

{- Given a (partially) empty skyline, returns a complete valid skyline if possible
 - or Nothing otherwise. Empty fields are set to 0. All existing skyline infos
 - need to be valid. Maximal size of passed skylines is 5 x 5. -}

buildSkyscrapers :: Skyline -> Maybe Skyline
buildSkyscrapers m = listToMaybe cs
    where cs = candidates m

-- 2

type Sudoku = [Row]

data Variant = Basic | Cross | Color deriving (Eq,Show)

type RowInd = Int
type ColInd = Int
type Position = (RowInd,ColInd)

sudokuLen = 9
validInd = [0 .. sudokuLen - 1]
validNum = [1 .. 9]

{- Sub-Sudokus and colors are numbered as follows:
 - 0|1|2
 - -+-+-
 - 3|4|5
 - -+-+-
 - 6|7|8 -}

sd0 = [[9,1,6,0,0,4,0,7,2],
                [8,0,0,6,2,0,0,5,0],
                [5,0,0,0,0,8,9,3,0],
                [0,6,0,0,0,0,2,0,0],
                [0,0,0,2,0,7,0,0,0],
                [0,0,5,0,0,0,0,9,0],
                [0,9,7,8,0,0,0,0,3],
                [0,8,0,0,7,6,0,0,9],
                [4,5,0,1,0,0,6,8,7]]

{- Checks if a row (filtered to valid numbers)
 - contains any duplicate elements -}

check19 :: Row -> Bool
check19 x = not $ dupes filt
    where filt = filter (`elem` validNum) x

splitByN :: Int -> [a] -> [[a]]
splitByN _ [] = []
splitByN n l = (take n l) : splitByN n (drop n l)

zipList :: [[a]] -> [[a]]
zipList l = [ map (!! i) l | i <- [0 .. len]]
    where minlen = minimum $ map length l
          len = minlen - 1

getSubsAsList :: Sudoku -> [[Integer]]
getSubsAsList = map concat . getSubs

getSubs :: Sudoku -> [[Row]]
getSubs m = concat $ map zipList colsplit
    where
    {- [[1,2,3,4...],
     -  [1,2,3,4...],
     -  [...],
     -  [...],
     -  ...]
     - -> each row is split into sections
     - [[[1,2,3],[4...]],
     -  [[1,2,3],[4,..]],
     -  [...],
     -  [...],
     -  ...]  -}
    rowsplit = map (splitByN 3) m
     {- -> group the row section lists by columns
     - [[[[1,2,3],[4...]],
     -   [[1,2,3],[4,..]],
     -   [...]],
     -   [[...],
     -   ...]  -}
    colsplit = splitByN 3 rowsplit

getSub :: Sudoku -> Int -> [Integer]
getSub m i = getSubsAsList m !! i

{- Determines the index of the correct Sub-Sudoku for
 - the given position -}
posToSub :: Position -> Int
posToSub (r,c) = subr * 3 + subc
    where (subr,subc) = (r `div` 3, c `div` 3)

{- Determines the index of the correct Color-List for
 - the given position -}
posToColor :: Position -> Int
posToColor (r,c) = colr * 3 + colc
    where (colr, colc) = (r `mod` 3, c `mod` 3)

getDia1 :: Sudoku -> [Integer]
getDia1 s = [ r !! i | (i,r) <- zip [0..] s ]

getDia2 :: Sudoku -> [Integer]
getDia2 = getDia1 . (map reverse)

getDias :: Sudoku -> [[Integer]]
getDias m = [getDia1 m, getDia2 m]

getColorF :: Sudoku -> Int -> [Integer]
getColorF m i = map (!! i) (getSubsAsList m)

getColorFs :: Sudoku -> [[Integer]]
getColorFs m = map (getColorF m . fromIntegral) validInd

-- a

{- Checks if the Sudoku meets the requirements of the given
 - Variant -}
isValidSDKSpecial :: Sudoku -> Variant -> Bool
isValidSDKSpecial _ Basic = True
isValidSDKSpecial m Cross = and $ map check19 (getDias m)
isValidSDKSpecial m Color = and $ map check19 (getColorFs m)

isValidSDK :: Sudoku -> Variant -> Bool
isValidSDK a v = rowsOk && colsOk && subsOk && specialOk
  where rowsOk = and $ map check19 (allRows a)
        colsOk = and $ map check19 (allCols a)
        subsOk = and $ map check19 (getSubsAsList a)
        specialOk = isValidSDKSpecial a v

-- b

{- Get a list of the numbers we have to regard for the given
 - position in this Sudoku Variant. -}
getSpecial :: Sudoku -> Position -> Variant -> [Integer]
getSpecial _ (_,_) Basic = []
getSpecial a (r,c) Cross
  | r == 4 && c == 4 = getDia1 a `union` getDia2 a
  | r == c           = getDia1 a
  | c == 9 - 1 - r   = getDia2 a
  | otherwise        = []
getSpecial a p Color = getColorF a (posToColor p)

{- Get a list of numbers which can be placed at the given Position
 - without violating the constraints given by a Sudoku of variant v.
 - (Some of these possibilities may result in an unsolvable Sudoku.) -}
allowedChars :: Sudoku -> Position -> Variant -> [Int]
allowedChars a p@(r,c) v = map fromIntegral allowed
  where cSub = getSub a (posToSub p)
        cSpecial = getSpecial a p v
        disallowed = concat [row a r, col a c, cSub, cSpecial]
        allowed = validNum \\ disallowed

firstJust :: [Maybe a] -> Maybe a
firstJust l = firstJust' $ dropWhile isNothing l
    where firstJust' (x:_) = x
          firstJust' _ = Nothing

replaceCell :: Sudoku -> Position -> Int -> Sudoku
replaceCell m (r,c) n = replace m r newrow
    where newrow = replace (row m r) c (fromIntegral n)

{- Note: last position does not wrap around on purpose -}

nextPos :: Position -> Position
nextPos (r,c) = (nextind `div` 9, nextind `mod` 9)
    where ind = r * 9 + c
          nextind = ind + 1

-- Place i at the given position and try to solve this new Sudoku.
tryPos :: Sudoku -> Position -> Variant -> Int -> Maybe Sudoku
tryPos a p v i = solvePos nextA (nextPos p) v
  where nextA = replaceCell a p i

solvePos :: Sudoku -> Position -> Variant -> Maybe Sudoku
solvePos a (r,c) v
  -- we reached the end of the Sudoku and filled all previous positions -> done
  | not validPosition   = Just a
  -- current position already contains a valid value, continue with the next
  | cur `elem` validNum = solvePos a (nextPos (r,c)) v
  {- Try to solve the Sudoku for each possible value at the given position
   - and return the first solved instance or Nothing. -}
  | otherwise           = firstJust (map (tryPos a (r,c) v) allowed)
  where allowed = allowedChars a (r,c) v
        cur = cell a r c
        (ri,ci) = (fromIntegral r, fromIntegral c)
        validPosition = ri `elem` validInd && ci `elem` validInd

solve :: Sudoku -> Variant -> Maybe Sudoku
solve a v
  | not (isValidSDK a v) = Nothing
  | otherwise            = solvePos a (0,0) v
