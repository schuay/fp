5 `isFactorOf` 10 == True
(-5) `isFactorOf` 10 == True
5 `isFactorOf` 7 == False
7 `isFactorOf` 7 == True
0 `isFactorOf` 10 == False
5 `isFactorOf` 0 == True

3 `isFactorOfL` [3 .. 9] == [True,False,False,True,False,False,True]

factorCandidates 125 == [5,9,13,17,21,25]

map inP [0,-1,1,5,-5,9,25] == [False,False,True,True,False,True,True]

map istPrimal [0,-1,1,5,45,49] == [False,False,True,True,False,True]

faktorisiere 45 == [(5,9),(9,5)]
faktorisiere 5 == []
{- how to test exceptions? -}
faktorisiere 0 == error "Unzulaessig"
faktorisiere (-1) == error "Unzulaessig"

matches "bc" "abcde" == False
matches "abc" "abcde" == True
matches "ab" "a" == False
matches "a" "" == False
matches "" "a" == True

suche "abcde" "abcdef" == (-1)
suche "abcde" "def" == (-1) 
suche "abcde" "" == 0
suche "" "" == (-1) -- is this correct? 
suche "abcde" "cde" == 2

sucheAlle "aaaaaa" "aa" == [0,2,4]
sucheAlle "abcdaba" "ab" == [0,4]
sucheAlle "" "ab" == []
sucheAlle "abcde" "" == [] -- correct?

ersetze "abcdeabac" 1 "ab" "x" == "xcdeabac"
ersetze "abcdeabac" 2 "ab" "x" == "abcdexac"
ersetze "abcdeabac" 3 "ab" "x" == "abcdeabac"
ersetze "abcdeabac" 1 "" "x" == "abcdeabac"
ersetze "abcdeabac" 1 "ab" "" == "cdeabac"
ersetze "" 1 "" "x" == ""
