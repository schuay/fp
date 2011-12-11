(sort $ reachableStates auto [] 0) == [0,1,2,3]
(sort $ reachableStates auto [] 1) == [0,1,2,3]
(sort $ reachableStates auto [] 2) == [0,1,2,3]
(sort $ reachableStates auto [] 3) == [0,1,2,3]
(sort $ reachableStates auto [] 4) == [0,1,2,3,4]
(sort $ reachableStates auto [] 5) == []

isPostfix auto 0 [0,1,2,3,4,5] "zb" == False
isPostfix auto 4 [0,1,2,3,4,5] "zb" == True
isPostfix auto 4 [0,1,2,4,5] "zb" == False
isPostfix auto 4 [0,1,2,4,5] "zb" == False
isPostfix auto 0 [0] "" == True
isPostfix auto 4 [0] "" == True
isPostfix auto 0 [] "" == False
isPostfix auto 0 [0] "acxaby" == True
isPostfix auto 4 [0] "acxaby" == True
isPostfix auto 0 [0] "acxabya" == True
isPostfix auto 4 [0] "acxabya" == True
isPostfix auto 0 [0] "acxabyae" == True
isPostfix auto 4 [0] "acxabyae" == True
isPostfix auto 0 [0] "acxabyaed" == True
isPostfix auto 4 [0] "acxabyaed" == True
isPostfix auto 0 [0] "acxabyaeb" == False
isPostfix auto 0 [3] "acxabyaeb" == True
isPostfix auto 4 [3] "acxabyaeb" == True
isPostfix auto 0 [0] "acxabyaedaedaedaedaed" == True
isPostfix auto 4 [0] "acxabyaedaedaedaedaed" == True

givePrefix auto 0 [0] "d" == Just ""
givePrefix auto 4 [3] "b" == Just "bddddd"
givePrefix auto 0 [3] "z" == Nothing

isValid s1 == True
isValid s2 == True
isValid s3 == True

isValid i1 == False
isValid i2 == False
isValid i3 == False
isValid i4 == False
isValid i5 == False

(length $ scraperPerms 1) == 1
(length $ scraperPerms 2) == 2
(length $ scraperPerms 3) == 6
(length $ scraperPerms 4) == 24
(length $ scraperPerms 5) == 120

s1 `elem` (allSkyscraperLines 3 1 2)
s2 `elem` (allSkyscraperLines 2 1 2)
s3 `elem` (allSkyscraperLines 5 1 3)
