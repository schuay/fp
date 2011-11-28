let t1 = Nil
let t2 = BNode 2 (BNode 3 Nil Nil) (BNode 5 Nil Nil)
flatten t1 Infix == []
flatten t2 Infix == [3,2,5]
flatten t2 GInfix == [5,2,3]
flatten t2 Praefix == [2,3,5]
flatten t2 GPostfix == [2,5,3]

let t1 = Nil
let t2 = BNode 2 (BNode 3 Nil Nil) (BNode 5 Nil Nil)
let t3 = BNode 4 (BNode 3 Nil Nil) (BNode 5 Nil Nil)
let t4 = BNode 2 (BNode 3 Nil Nil) (BNode 3 Nil Nil)
isST t1 == True
isST t2 == False
isST t3 == True
isST t4 == False

let t1 = Leaf (\x->2*x+3)
let t2 = Node (*2) (Leaf (*3)) (Leaf (*5)) (Leaf (*7))
let t3 = Node (*2) (Leaf (*3)) (Node (+1) (Leaf (*5)) (Leaf (+5)) (Leaf (+2))) (Leaf (*7))
apply ['r','l','l','m'] 5 t1 == 13
apply ['r','l','l','m'] 5 t2 == 70
apply ['m','l'] 5 t3 == 55
apply [] 5 t2 == 10

let t1 = LNode 5 []
let t2 = LNode 5 [LNode 10 [], LNode 15 [LNode 20 [], LNode 25 []]]
let t3 = LNode 5 [LNode 10 [LNode 15 [], LNode 20 [LNode 25 []]]]
mapLT (*2) t1 == LNode 10 []
mapLT (*2) t2 == LNode 10 [LNode 20 [], LNode 30 [LNode 40 [], LNode 50 []]]
mapLT (*2) t3 == LNode 10 [LNode 20 [LNode 30 [], LNode 40 [LNode 50 []]]]

